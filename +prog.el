;;; +prog.el -*- lexical-binding: t; -*-

(if (modulep! :editor file-templates)
    (set-file-template! "/pom\\.xml$" :trigger "__pom.xml" :mode 'nxml-mode))

;; xml
(after! nxml-mode
  (when (modulep! :tools tree-sitter)
    (add-hook! 'nxml-mode-local-vars-hook :append #'tree-sitter!))

  (setq nxml-auto-insert-xml-declaration-flag nil
        nxml-slash-auto-complete-flag nil)

  (if (modulep! :checkers spell)
      (add-hook! 'nxml-mode-hook
        (defun disable-spell-for-xml()
          (if (modulep! :checkers spell +flyspell)
              (flyspell-mode -1)
            (spell-fu-mode -1))))))

(use-package! auto-rename-tag
  :config
  (add-hook! (nxml-mode xml-mode sgml-mode) #'auto-rename-tag-mode))

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

  ;; (add-hook 'before-save-hook #'gofmt-before-save)
  (let ((command (or (executable-find "gofumpt")
                     (executable-find "goimports"))))
    (if command
        (setq gofmt-command command)))

  (remove-hook! go-mode #'go-eldoc-setup)

  (defadvice! godef-jump-a (&rest _args)
    :override #'godef-jump
    (+lookup/definition))

  (defadvice! godef-describe-a (&rest _args)
    :override #'godef-describe
    (+lookup/documentation)))

(use-package! go-fill-struct
  :when (modulep! :lang go)
  :after go-mode
  :init
  (map! :map go-mode-map
        :localleader
        "f" #'go-fill-struct))

(use-package! go-impl
  :when (modulep! :lang go)
  :after go-mode
  :init
  (map! :map go-mode-map
        :localleader
        "I" #'go-impl))

;; java
;; (when (and (modulep! :lang java)
;;            (modulep! :editor format))
;;   (set-formatter! 'google-java-format
;;     '("google-java-format" "-" "-a" "-" "--skip-sorting-imports")
;;     :modes 'java-mode))

(map! :map (java-mode-map java-ts-mode-map)
      :localleader
      :desc "Run junit test" "t" #'+java/run-junit-test
      :desc "Update project config" "u" #'lsp-bridge-jdtls-update-project-configuration)

(defvar +java/junit-platform-console-standalone-jar
  (expand-file-name "~/.local/jdtls/test-runner/junit-platform-console-standalone.jar"))

;; check junit console launcher options for details
(defun +java/run-junit-test ()
  "Java run main/test at point."
  (interactive)
  (let* ((pkg (+java/current-package))
         (class (+java/current-class))
         (method (+java/current-method)))
    (if (and pkg class)
        (lsp-bridge-jdtls-project-is-test-file
         #'(lambda (is-test-file)
             (if is-test-file
                 (lsp-bridge-jdtls-project-get-classpaths
                  #'(lambda (classpaths)
                      (setq-local old-default-directory default-directory
                                  default-directory (doom-project-root))
                      (compilation-start (concat "java -jar " +java/junit-platform-console-standalone-jar
                                                 " execute --disable-banner  -cp " classpaths
                                                 (if method
                                                     (format " -m '%s.%s#%s'" pkg class method)
                                                   (format " -c '%s.%s'" pkg class))))
                      (setq-local default-directory old-default-directory))
                  "test")
               (message "%s is not a test file" class))))
      (user-error "Can not found package/class"))))

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
              (tsc-node-text found-node)))))))

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

;; dockerfile
(after! dockerfile-ts-mode
  (set-docsets! 'dockerfile-ts-mode "Docker"))

;; yaml
(when (treesit-available-p)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

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
(use-package! protobuf-ts-mode
  :mode "\\.proto\\'"
  :defer t)

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

;; lsp-bridge
(use-package! lsp-bridge
  :config
  (after! winner
    (appendq! winner-boring-buffers '("*lsp-bridge-code-action-menu*" "*lsp-bridge-call-hierarchy*")))

  (setq lsp-bridge-user-multiserver-dir (expand-file-name "lsp-bridge/multiserver" doom-user-dir)
        lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge/langserver" doom-user-dir)
        lsp-bridge-enable-completion-in-string t
        lsp-bridge-enable-mode-line nil
        lsp-bridge-enable-log nil
        lsp-bridge-enable-org-babel t
        lsp-bridge-diagnostic-tooltip-border-width 2
        lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        lsp-bridge-signature-show-with-frame-position "point"
        lsp-bridge-python-lsp-server "ruff"
        acm-backend-yas-match-by-trigger-keyword t
        acm-backend-search-file-words-enable-fuzzy-match t
        acm-enable-tabnine nil
        acm-enable-copilot nil
        acm-enable-preview t)

  (if (modulep! :completion vertico)
      (setq acm-candidate-match-function 'orderless-regexp))

  (global-lsp-bridge-mode)

  (when (modulep! :checkers syntax)
    (defun lsp-bridge-lsp-server-disable-flymake-or-flycheck()
      (cond ((and (modulep! :checkers syntax +flymake)
                  (lsp-bridge-has-lsp-server-p))
             (if (bound-and-true-p flymake-mode)
                 (flymake-mode -1))
             (if (featurep 'flymake-popon)
                 (flymake-popon-mode -1)))
            (t
             (if (bound-and-true-p flycheck-mode)
                 (flycheck-mode -1)))))

    (dolist (hook lsp-bridge-default-mode-hooks)
      (add-hook hook #'lsp-bridge-lsp-server-disable-flymake-or-flycheck t)))

  (let ((lombok-jar-path (expand-file-name "lombok.jar" doom-user-dir)))
    (setq lsp-bridge-jdtls-jvm-args (list "-Dfile.encoding=utf8"
                                          "-server"
                                          "-Xmx2G"
                                          "-Xmn768M"
                                          "-Xss512K"
                                          "-XX:MetaspaceSize=256M"
                                          "-XX:MaxMetaspaceSize=256M"
                                          "-XX:InitialCodeCacheSize=128M"
                                          "-XX:ReservedCodeCacheSize=128M"
                                          "-XX:MaxDirectMemorySize=512M"
                                          "-XX:+UseNUMA"
                                          "-XX:+UseZGC"
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
         :desc "LSP Rename"                  "r"  #'lsp-bridge-rename
         :desc "LSP Peek"                    "p"  #'lsp-bridge-peek))

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
