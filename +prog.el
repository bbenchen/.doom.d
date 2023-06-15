;;; +prog.el -*- lexical-binding: t; -*-

(if (modulep! :editor file-templates)
    (set-file-template! "/pom\\.xml$" :trigger "__pom.xml" :mode 'nxml-mode))

;; flycheck
(after! flycheck
  (setq flycheck-checker-error-threshold 2000))

;; xml
(after! nxml-mode
  (setq nxml-auto-insert-xml-declaration-flag nil
        nxml-slash-auto-complete-flag nil)

  (if (modulep! :checkers spell)
      (add-hook! 'nxml-mode-hook
        (defun disable-spell-for-xml()
          (if (modulep! :checkers spell +flyspell)
              (flyspell-mode -1)
            (spell-fu-mode -1))))))

;; go
(after! go-mode
  (when (executable-find "gopkgs")
    (defun go-packages-gopkgs ()
      "Return a list of all Go packages, using `gopkgs'."
      (if-let (project-dir (doom-project-root))
          (sort (process-lines "gopkgs" "-workDir" project-dir) #'string<)
        (sort (process-lines "gopkgs") #'string<)))
    (setq go-packages-function 'go-packages-gopkgs))

  (if (executable-find "gogetdoc")
      (setq godoc-at-point-function 'godoc-gogetdoc))

  (let ((command (or (executable-find "gofumpt")
                     (executable-find "goimports"))))
    (if command
        (setq gofmt-command command)))

  ;; (add-hook 'before-save-hook #'gofmt-before-save)

  (remove-hook! 'go-mode-hook #'go-eldoc-setup)

  (if (modulep! :checkers syntax)
      (add-hook! 'go-mode-hook
        (setq-local flycheck-disabled-checkers '(go-gofmt
                                                 go-golint
                                                 go-vet
                                                 go-build
                                                 go-test
                                                 go-errcheck
                                                 go-unconvert
                                                 go-staticcheck)))))

(use-package! go-fill-struct
  :when (modulep! :lang go)
  :defer t
  :init
  (map! :map go-mode-map
        :localleader
        (:prefix ("r" . "reflect")
                 "s" #'go-fill-struct)))

;; java
;; (when (and (modulep! :lang java)
;;            (modulep! :editor format))
;;   (set-formatter! 'google-java-format
;;     '("google-java-format" "-" "-a" "-" "--skip-sorting-imports")
;;     :modes 'java-mode))

(add-hook! '(java-mode-hook java-ts-mode-hook)
  (setq tab-width 4
        fill-column 120))

(defvar +java/junit-platform-console-standalone-jar
  (expand-file-name "~/.local/jdtls/test-runner/junit-platform-console-standalone.jar"))

;; check junit console launcher options for details
(defun +java/run-junit-test ()
  "Java run main/test at point."
  (interactive)
  (let* ((pkg (+java/current-package))
         (class (+java/current-class))
         (method (+java/current-method))
         (classpath (+java/maven-get-project-classpath)))
    (if (and pkg class classpath)
        (compile
         (concat "java -jar " +java/junit-platform-console-standalone-jar
                 " -cp " classpath
                 (if method
                     (format " -m '%s.%s#%s'" pkg class method)
                   (format " -c '%s.%s'" pkg class)))
         t)
      (message "Can not found package/class/classpath"))))

(defun +java/maven-get-project-classpath ()
  (when-let* ((project-dir (doom-project-root))
              (target-path (expand-file-name "target" project-dir))
              (deps-cp (+java/maven-get-deps-classpath target-path)))
    (format "%s/classes:%s/test-classes:%s" target-path target-path deps-cp)))

(defun +java/maven-get-deps-classpath (target-location)
  "Get dependencies classpath."
  (let ((deps-cp-file (format "%s/deps-cp" target-location)))
    (unless (file-exists-p deps-cp-file)
      ;; NOTE: Cache deps classpath to speed up shell command, regenerate it once you modify project dependencies.
      (let* ((project-dir (doom-project-root))
             (mvnw-file (expand-file-name "mvnw" project-dir))
             (maven (if (and (file-exists-p! mvnw-file)
                             (file-executable-p mvnw-file))
                        "./mvnw"
                      "mvn"))
             (command (concat "cd " project-dir " && "
                              maven " dependency:build-classpath -Dmdep.includeScope=test -Dmdep.outputFile=" deps-cp-file)))
        (call-process-shell-command command)))
    (with-temp-buffer
      (insert-file-contents deps-cp-file)
      (buffer-string))))

(defun +java/current-package ()
  (if (eq major-mode 'java-mode)
      (+java-current-package)
    (+java/treesit-get-package)))

(defun +java/current-class ()
  (if (eq major-mode 'java-mode)
      (+java-current-class)
    (+java/treesit-get-class)))

(defun +java/current-method ()
  (if (eq major-mode 'java-mode)
      (if (and (modulep! :tools tree-sitter)
               (modulep! :lang java +tree-sitter))
          (+java/tree-sitter-get-method))
    (+java/treesit-get-method)))

(when (and (modulep! :tools tree-sitter)
           (modulep! :lang java +tree-sitter))
  (defun +java/tree-sitter-get-method ()
    (let* ((query (tsc-make-query (tree-sitter-require 'java) [(method_declaration name: (identifier) @function.method)]))
           (root-node (tsc-root-node tree-sitter-tree))
           (captures (tsc--without-restriction
                       (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))
           (nodes (mapcar (lambda (capture)
                            (pcase-let ((`(_ . ,node) capture))
                              node))
                          captures)))

      (when (length> nodes 0)
        (let* ((cur-node (tree-sitter-node-at-pos :named))
               (found-node (cl-find-if (lambda (node) (tsc-node-eq node cur-node)) nodes)))
          (unless found-node
            (let ((parent (tsc-get-parent cur-node))
                  (break))
              (while (and parent (not break))
                (setq break (string= (tsc-node-type parent) "method_declaration"))
                (unless break
                  (setq parent (tsc-get-parent parent))))
              (if parent
                  (setq found-node (tsc-get-child-by-field parent :name)))
              ))
          (if found-node
              (tsc-node-text found-node))
          )))))

(after! java-ts-mode
  (defun +java/treesit-get-package-node ()
    (treesit-node-text
     (car (treesit-filter-child
           (treesit-buffer-root-node)
           (lambda (child)
             (member (treesit-node-type child) '("package_declaration")))))
     t))

  (defun +java/treesit-get-package ()
    (let ((p (+java/treesit-get-package-node)))
      (when (string-match "package \\(.+\\);" p)
        (match-string 1 p))))

  (defun +java/treesit-get-class ()
    (treesit-defun-name
     (car
      (treesit-filter-child
       (treesit-buffer-root-node)
       (lambda (child)
         (member (treesit-node-type child) '("class_declaration")))))))

  (defun +java/treesit-get-method ()
    (treesit-defun-name
     (treesit-parent-until
      (treesit-node-at (point))
      (lambda (parent)
        (member (treesit-node-type parent) '("method_declaration")))))))

;; pkgbuild-mode
(use-package! pkgbuild-mode
  :defer t
  :init
  (setq pkgbuild-update-sums-on-save nil)
  :config
  (add-hook! 'pkgbuild-mode-hook
    (setq mode-name "PKGBUILD"
          mode-line-process nil)))

;; plantuml
(setq plantuml-jar-path (expand-file-name "plantuml.jar" doom-user-dir)
      plantuml-output-type "png"
      org-plantuml-jar-path plantuml-jar-path)

;; protobuf
(use-package! protobuf-mode
  :defer t
  :init
  (add-hook! 'protobuf-mode-hook
    (display-line-numbers-mode t)))

;; projectile
(after! projectile
  (dolist (dir '(".bloop" ".metals"))
    (add-to-list 'projectile-globally-ignored-directories dir)))

;; scala
(after! scala-mode
  (map! :map scala-mode-map
        :localleader
        (:prefix ("b" . "sbt")
                 "." #'sbt-hydra
                 "b" #'sbt-command)))

;; sbt
(after! sbt-mode
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; sql
(after! sql
  (setq sql-product 'oracle))

(use-package! sqlup-mode
  :defer t
  :init
  (add-hook! '(sql-mode-hook sql-interactive-mode-hook) :append #'sqlup-mode)
  :config
  (setq sqlup-blacklist (append sqlup-blacklist '("name" "user"))))

(use-package! sql-indent
  :defer t
  :init
  (add-hook! 'sql-mode-hook :append #'sqlind-minor-mode))

(use-package! ob-sql-mode)

;; dap-mode
(after! dap-mode
  (add-hook! '(dap-session-created-hook dap-stopped-hook)
    (defun show-dap-hydra (&rest _)
      "Show dap hydra"
      (dap-hydra)))

  (add-hook! 'dap-terminated-hook
    (defun hide-dap-hydra (&rest _)
      "Hide dap hydra"
      (dap-hydra/nil))))

;; lsp-bridge
(use-package! lsp-bridge
  :init
  (setq lsp-bridge-user-multiserver-dir (expand-file-name "lsp-bridge/multiserver" doom-user-dir)
        lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge/langserver" doom-user-dir)
        lsp-bridge-enable-mode-line nil
        lsp-bridge-enable-log nil
        lsp-bridge-enable-org-babel t
        acm-enable-tabnine nil
        acm-enable-preview t)
  :config
  (global-lsp-bridge-mode)

  (setq lsp-bridge-diagnostic-tooltip-border-width 2
        lsp-bridge-signature-show-function 'lsp-bridge-signature-frame)

  (let ((lombok-jar-path (expand-file-name "lombok.jar" doom-user-dir)))
    (setq lsp-bridge-jdtls-jvm-args (list "-Dfile.encoding=utf8"
                                          "-server"
                                          "-Xmx6G"
                                          "-Xmn2G"
                                          "-Xss512K"
                                          "-XX:MetaspaceSize=1536M"
                                          "-XX:MaxMetaspaceSize=1536M"
                                          "-XX:InitialCodeCacheSize=128M"
                                          "-XX:ReservedCodeCacheSize=512M"
                                          "-XX:+UseG1GC"
                                          "-XX:+UseStringDeduplication"
                                          "-XX:GCTimeRatio=19"
                                          "-XX:AdaptiveSizePolicyWeight=90"
                                          "-Dsun.zip.disableMemoryMapping=true"
                                          (concat "-javaagent:" lombok-jar-path))))
  (setq lsp-bridge-jdtls-default-file (expand-file-name "lsp-bridge/langserver/jdtls.json" doom-user-dir))

  (setq lsp-bridge-get-multi-lang-server-by-project
        (lambda (_project-path filepath)
          ;; If typescript file include deno.land url, then use Deno LSP server.
          (save-excursion
            (when (string-equal (file-name-extension filepath) "ts")
              (cl-dolist (buf (buffer-list))
                (when (string-equal (buffer-file-name buf) filepath)
                  (with-current-buffer buf
                    (goto-char (point-min))
                    (when (search-forward-regexp (regexp-quote "from \"https://deno.land") nil t)
                      (cl-return "deno")))))))))

  (map! :leader
        (:prefix-map ("c" . "code")
         :desc "LSP Code actions"            "a"  #'lsp-bridge-code-action
         :desc "Jump to symbol in workspace" "j"  #'lsp-bridge-workspace-list-symbols
         :desc "LSP Rename"                  "r"  #'lsp-bridge-rename))

  (defadvice! lsp-bridge-frame-background-color-a ()
    :override #'lsp-bridge-frame-background-color
    (doom-color 'modeline-bg))

  (defadvice! +format/region-or-buffer-a (fn &rest args)
    :around #'+format/region-or-buffer
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-code-format)
      (apply fn args)))

  (defadvice! +lookup/definition-a (fn &rest args)
    :around #'+lookup/definition
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-find-def)
      (apply fn args)))

  (defadvice! +lookup/type-definition-a (fn &rest args)
    :around #'+lookup/type-definition
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-find-type-def)
      (apply fn args)))

  (defadvice! +lookup/references-a (fn &rest args)
    :around #'+lookup/references
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-find-references)
      (apply fn args)))

  (defadvice! +lookup/implementations-a (fn &rest args)
    :around #'+lookup/implementations
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-find-impl)
      (apply fn args)))

  (defadvice! +lookup/documentation-a (fn &rest args)
    :around #'+lookup/documentation
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-popup-documentation)
      (apply fn args)))

  (defadvice! better-jumper-jump-backward-a (fn &rest args)
    :around #'better-jumper-jump-backward
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-find-def-return)
      (apply fn args)))

  (defadvice! +default/diagnostics-a (fn &rest args)
    :around #'+default/diagnostics
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-diagnostic-list)
      (apply fn args))))
