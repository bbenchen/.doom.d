;;; +prog.el -*- lexical-binding: t; -*-

(if (modulep! :editor file-templates)
    (set-file-template! "/pom\\.xml$" :trigger "__pom.xml" :mode 'nxml-mode))

;; markdown
(after! markdown-toc
  (setq markdown-toc-indentation-space 2))

;; xml
(after! nxml-mode
  (when (modulep! :tools tree-sitter)
    (setq +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode nxml-mode))
    (add-hook! 'nxml-mode-local-vars-hook :append #'tree-sitter!))

  (setq nxml-auto-insert-xml-declaration-flag nil
        nxml-slash-auto-complete-flag nil)

  (if (modulep! :checkers spell)
      (add-hook! 'nxml-mode-hook
        (defun bc/nxml--disable-spell()
          (if (modulep! :checkers spell +flyspell)
              (flyspell-mode -1)
            (spell-fu-mode -1))))))

(use-package! auto-rename-tag
  :config
  (add-hook! (nxml-mode xml-mode sgml-mode) #'auto-rename-tag-mode))

;; go
(after! go-mode
  (when (executable-find "gopkgs")
    (defun bc/go-packages--gopkgs ()
      "Return a list of all Go packages, using `gopkgs'."
      (if-let (project-dir (doom-project-root))
          (sort (process-lines "gopkgs" "-workDir" project-dir) #'string<)
        (sort (process-lines "gopkgs") #'string<)))
    (setq go-packages-function 'bc/go-packages--gopkgs))

  (if (executable-find "gogetdoc")
      (setq godoc-at-point-function 'godoc-gogetdoc))

  ;; (add-hook 'before-save-hook #'gofmt-before-save)
  (let ((command (or (executable-find "gofumpt")
                     (executable-find "goimports"))))
    (if command
        (setq gofmt-command command)))

  (remove-hook! go-mode #'go-eldoc-setup)

  (defadvice! bc/godef-jump-override-advice (&rest _args)
    :override #'godef-jump
    (+lookup/definition))

  (defadvice! bc/godef-describe-override-advice (&rest _args)
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
(after! projectile
  (delete "build.gradle" projectile-project-root-files))

(when (modulep! :editor format)
  (set-formatter! 'spring-java-format '("spring-java-format") :modes '(java-mode java-ts-mode))

  (defadvice! bc/apheleia-format-buffer-around-advice (fn &rest args)
    :around #'apheleia-format-buffer
    (let ((default-directory (doom-project-root)))
      (apply fn args))))
;; (when (and (modulep! :lang java)
;;            (modulep! :editor format))
;;   (set-formatter! 'google-java-format
;;     '("google-java-format" "-" "-a" "--skip-sorting-imports")
;;     :modes '(java-mode java-ts-mode)))

(add-hook! java-ts-mode #'rainbow-delimiters-mode)

(map! :map (java-mode-map java-ts-mode-map)
      :localleader
      :desc "Run junit test" "t" #'bc/java-run-junit-test
      :desc "Update project config" "u" #'lsp-bridge-jdtls-update-project-configuration)

;; check junit console launcher options for details
(defun bc/java-run-junit-test ()
  "Java run main/test at point."
  (interactive)
  (let* ((pkg (bc/java--current-package))
         (class (bc/java--current-class))
         (method (bc/java--current-method))
         (parameters (bc/java--current-parameters)))
    (if (and pkg class)
        (lsp-bridge-jdtls-project-is-test-file
         #'(lambda (is-test-file)
             (if is-test-file
                 (lsp-bridge-jdtls-project-get-classpaths
                  #'(lambda (classpaths)
                      (let ((junit-wrapper "junit-wrapper")
                            (classpath-file (expand-file-name "junit-classpaths" temporary-file-directory)))
                        (with-temp-file classpath-file
                          (insert "-classpath\n")
                          (insert classpaths))

                        (setq-local old-default-directory default-directory
                                    default-directory (doom-project-root))
                        (compilation-start (concat junit-wrapper (format " '%s'" classpath-file)
                                                   (if method
                                                       (if (not (string-empty-p parameters))
                                                           (format " '%s.%s#%s(%s)'" pkg class method parameters)
                                                         (format " '%s.%s#%s'" pkg class method))
                                                     (format " '%s.%s'" pkg class))))
                        (setq-local default-directory old-default-directory)))
                  "test")
               (message "%s is not a test file" class))))
      (user-error "Can not found package/class"))))

(defun bc/java--current-package ()
  (if (eq major-mode 'java-mode)
      (bc/java--current-package)
    (bc/java--treesit-get-package)))

(defun bc/java--current-class ()
  (if (eq major-mode 'java-mode)
      (bc/java--current-class)
    (bc/java--treesit-get-class)))

(defun bc/java--current-method ()
  (if (eq major-mode 'java-mode)
      (if (and (modulep! :tools tree-sitter)
               (modulep! :lang java +tree-sitter))
          (bc/java--tree-sitter-get-method))
    (bc/java--treesit-get-method)))

(defun bc/java--current-parameters ()
  (if (eq major-mode 'java-mode)
      (if (and (modulep! :tools tree-sitter)
               (modulep! :lang java +tree-sitter))
          (bc/java--tree-sitter-get-parameters))
    (bc/java--treesit-get-parameters)))

(when (and (modulep! :tools tree-sitter)
           (modulep! :lang java +tree-sitter))
  (defun bc/java--tree-sitter-get-method ()
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
                  (setq found-node (tsc-get-child-by-field parent :name)))))
          (if found-node
              (tsc-node-text found-node))))))

  (defun bc/java--tree-sitter-get-parameters ()
    (user-error "unimplemented")))

(after! java-ts-mode
  (defun bc/java--treesit-get-package ()
    (let ((package-node (treesit-node-text
                         (car (treesit-filter-child
                               (treesit-buffer-root-node)
                               (lambda (child)
                                 (member (treesit-node-type child) '("package_declaration")))))
                         t)))
      (when (string-match "package \\(.+\\);" package-node)
        (match-string 1 package-node))))

  (defun bc/java--treesit-get-class ()
    (treesit-defun-name
     (car
      (treesit-filter-child
       (treesit-buffer-root-node)
       (lambda (child)
         (member (treesit-node-type child) '("class_declaration")))))))

  (defun bc/java--treesit-get-method ()
    (treesit-defun-name (bc/java--treesit-get-method-node)))

  (defun bc/java--treesit-get-method-node ()
    (treesit-parent-until
     (treesit-node-at (point))
     (lambda (parent)
       (member (treesit-node-type parent) '("method_declaration")))))

  (defun bc/java--treesit-get-parameters ()
    (when-let* ((mnode (bc/java--treesit-get-method-node))
                (pnode (treesit-node-child-by-field-name mnode "parameters"))
                (types (cl-map 'vector
                               #'bc/java--treesit-get-parameter-type
                               (treesit-node-children pnode t))))
      (mapconcat 'identity types ",")))

  (defun bc/java--treesit-get-parameter-type (parameter-node)
    (let* ((type-node (treesit-node-child-by-field-name parameter-node "type"))
           (type-name (if (string= (treesit-node-type type-node) "generic_type")
                          (treesit-node-text (treesit-node-child type-node 0) t)
                        (treesit-node-text type-node t))))
      (cond ((string= "String" type-name) "java.lang.String")
            ((string-prefix-p "String[" type-name) (concat "java.lang." type-name))
            ((string= "Byte" type-name) "java.lang.Byte")
            ((string-prefix-p "Byte[" type-name) (concat "java.lang." type-name))
            ((string= "Char" type-name) "java.lang.Char")
            ((string-prefix-p "Char[" type-name) (concat "java.lang." type-name))
            ((string= "Short" type-name) "java.lang.Short")
            ((string-prefix-p "Short[" type-name) (concat "java.lang." type-name))
            ((string= "Integer" type-name) "java.lang.Integer")
            ((string-prefix-p "Integer[" type-name) (concat "java.lang." type-name))
            ((string= "Long" type-name) "java.lang.Long")
            ((string-prefix-p "Long[" type-name) (concat "java.lang." type-name))
            ((string= "Float" type-name) "java.lang.Float")
            ((string-prefix-p "Float[" type-name) (concat "java.lang." type-name))
            ((string= "Double" type-name) "java.lang.Double")
            ((string-prefix-p "Double[" type-name) (concat "java.lang." type-name))
            ((string= "Class" type-name) "java.lang.Class")
            ((string-prefix-p "Class[" type-name) (concat "java.lang." type-name))
            ((string= "BigDecimal" type-name) "java.math.BigDecimal")
            ((string-prefix-p "BigDecimal[" type-name) (concat "java.math." type-name))
            ((string= "BigInteger" type-name) "java.math.BigInteger")
            ((string-prefix-p "BigInteger[" type-name) (concat "java.math." type-name))
            ((string= "URI" type-name) "java.net.URI")
            ((string-prefix-p "URI[" type-name) (concat "java.net." type-name))
            ((string= "URL" type-name) "java.net.URL")
            ((string-prefix-p "URL[" type-name) (concat "java.net." type-name))
            ((string= "List" type-name) "java.util.List")
            ((string= "Map" type-name) "java.util.Map")
            ((string= "SortedMap" type-name) "java.util.SortedMap")
            ((string= "NavigableMap" type-name) "java.util.NavigableMap")
            ((string= "Set" type-name) "java.util.Set")
            ((string= "SortedSet" type-name) "java.util.SortedSet")
            ((string= "NavigableSet" type-name) "java.util.NavigableSet")
            ((string= "File" type-name) "java.io.File")
            ((string= "Path" type-name) "java.nio.file.Path")
            ((string= "Charset" type-name) "java.nio.charset.Charset")
            ((string= "Locale" type-name) "java.util.Locale")
            ((string= "UUID" type-name) "java.util.UUID")
            ((string= "ArgumentsAccessor" type-name) "org.junit.jupiter.params.aggregator.ArgumentsAccessor")
            (t type-name)))))

;; dockerfile
(after! dockerfile-mode
  (if (executable-find "podman")
      (setq dockerfile-mode-command "podman")
    (setq dockerfile-use-buildkit nil
          dockerfile-mode-command "docker"))
  (defun dockerfile-build-buffer (image-name &optional no-cache)
    "Build an image called IMAGE-NAME based upon the buffer.

If the prefix arg NO-CACHE is set, don't cache the image.

The shell command used to build the image is:

    sudo docker build    \\
      --no-cache         \\
      --force-rm         \\
      --pull             \\
      --tag IMAGE-NAME   \\
      --build-args args  \\
      -f filename        \\
      directory"
    (interactive (list (dockerfile-read-image-name) prefix-arg))
    (save-buffer)
    (compilation-start
     (format
      "%s%s%s build %s %s %s %s %s %s -f %s %s"
      (if dockerfile-use-buildkit "DOCKER_BUILDKIT=1 " "")
      (if dockerfile-use-sudo "sudo " "")
      dockerfile-mode-command
      (if no-cache "--no-cache" "")
      (if dockerfile-build-force-rm "--force-rm " "")
      (if dockerfile-build-pull "--pull " "")
      (dockerfile-tag-string image-name)
      (dockerfile-build-arg-string)
      (or dockerfile-build-extra-options "")
      (shell-quote-argument (dockerfile-standard-filename
                             (or (file-remote-p (buffer-file-name) 'localname)
                                 (buffer-file-name))))
      (shell-quote-argument (dockerfile-standard-filename
                             (or (file-remote-p default-directory 'localname)
                                 default-directory))))
     nil
     (lambda (_) (format "*docker-build-output: %s *" image-name)))))

(after! dockerfile-ts-mode
  (set-docsets! 'dockerfile-ts-mode "Docker"))

;; yaml
(use-package! yaml-ts-mode
  :when (treesit-available-p))

;; cmake
(use-package! cmake-ts-mode
  :when (treesit-available-p))

;; pkgbuild-mode
(use-package! pkgbuild-mode
  :defer t
  :init
  (setq pkgbuild-update-sums-on-save nil)
  :config
  (add-hook! 'pkgbuild-mode-hook
    (setq mode-name "PKGBUILD"
          mode-line-process nil)))

;; nginx-mode
(use-package! nginx-mode
  :defer t)

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

;; sbt
(after! sbt-mode
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; sql
(after! sql
  (setq sql-product 'oracle)

  (setq sql-product-alist
        '((ansi
           :name "ANSI"
           :font-lock sql-mode-ansi-font-lock-keywords
           :statement sql-ansi-statement-starters)

          (mysql
           :name "MySQL"
           :free-software t
           :font-lock sql-mode-mysql-font-lock-keywords
           :sqli-program sql-mysql-program
           :sqli-options sql-mysql-options
           :sqli-login sql-mysql-login-params
           :sqli-comint-func sql-comint-mysql
           :list-all "SHOW TABLES;"
           :list-table "DESCRIBE %s;"
           :prompt-regexp "^mysql> "
           :prompt-length 6
           :prompt-cont-regexp "^    -> "
           :syntax-alist ((?# . "< b") (?\\ . "\\"))
           :input-filter sql-remove-tabs-filter)

          (oracle
           :name "Oracle"
           :font-lock sql-mode-oracle-font-lock-keywords
           :sqli-program sql-oracle-program
           :sqli-options sql-oracle-options
           :sqli-login sql-oracle-login-params
           :sqli-comint-func sql-comint-oracle
           :list-all sql-oracle-list-all
           :list-table sql-oracle-list-table
           :completion-object sql-oracle-completion-object
           :prompt-regexp "^SQL> "
           :prompt-length 5
           :prompt-cont-regexp "^\\(?:[ ][ ][1-9]\\|[ ][1-9][0-9]\\|[1-9][0-9]\\{2\\}\\)[ ]\\{2\\}"
           :statement sql-oracle-statement-starters
           :syntax-alist ((?$ . "_") (?# . "_"))
           :terminator ("\\(^/\\|;\\)" . "/")
           :input-filter sql-placeholders-filter)

          (postgres
           :name "Postgres"
           :free-software t
           :font-lock sql-mode-postgres-font-lock-keywords
           :sqli-program sql-postgres-program
           :sqli-options sql-postgres-options
           :sqli-login sql-postgres-login-params
           :sqli-comint-func sql-comint-postgres
           :list-all ("\\d+" . "\\dS+")
           :list-table ("\\d+ %s" . "\\dS+ %s")
           :completion-object sql-postgres-completion-object
           :prompt-regexp "^[-[:alnum:]_]*[-=][#>] "
           :prompt-length 5
           :prompt-cont-regexp "^[-[:alnum:]_]*[-'(][#>] "
           :statement sql-postgres-statement-starters
           :input-filter sql-remove-tabs-filter
           :terminator ("\\(^\\s-*\\\\g\\|;\\)" . "\\g"))

          (sqlite
           :name "SQLite"
           :free-software t
           :font-lock sql-mode-sqlite-font-lock-keywords
           :sqli-program sql-sqlite-program
           :sqli-options sql-sqlite-options
           :sqli-login sql-sqlite-login-params
           :sqli-comint-func sql-comint-sqlite
           :list-all ".tables"
           :list-table ".schema %s"
           :completion-object sql-sqlite-completion-object
           :prompt-regexp "^sqlite> "
           :prompt-length 8
           :prompt-cont-regexp "^   \\.\\.\\.> ")))

  (if (modulep! :tools tree-sitter)
      (add-hook! 'sql-mode-local-vars-hook #'tree-sitter! 'append)))

(use-package! flymake-sqlfluff
  :when (modulep! :checkers syntax +flymake)
  :config
  (setq flymake-sqlfluff-dialect "oracle")

  (add-hook! 'sql-mode-hook #'flymake-sqlfluff-load)

  (defadvice! bc/set-flymake-sqlfluff-dialect-after-sql-set-product (&rest _args)
    :after #'sql-set-product
    (setq flymake-sqlfluff-dialect (symbol-name sql-product))))

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

;; (use-package! topsy
;;   :config
;;   (add-hook! 'magit-section-mode-hook #'topsy-mode)

;;   (after! lsp-bridge
;;     (setcdr (assoc nil topsy-mode-functions)
;;             (lambda ()
;;               (when (lsp-bridge-is-remote-file) "[lsp-bridge] remote file")))

;;     ;; 当前主要模式为 org-mode 时不激活
;;     (add-hook! 'lsp-bridge-mode-hook (unless (derived-mode-p 'org-mode)
;;                                        (topsy-mode 1)))))

;; lsp-bridge
(use-package! lsp-bridge
  :defer 2
  :init
  (setq lsp-bridge-enable-mode-line nil)
  :config
  (after! winner
    (dolist (buffer '("*lsp-bridge-code-action-menu*" "*lsp-bridge-call-hierarchy*"))
      (add-to-list 'winner-boring-buffers buffer t)))

  (setq lsp-bridge-user-multiserver-dir (expand-file-name "lsp-bridge/multiserver" doom-user-dir)
        lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge/langserver" doom-user-dir)
        lsp-bridge-enable-completion-in-string t
        lsp-bridge-enable-log nil
        lsp-bridge-enable-org-babel t
        lsp-bridge-diagnostic-tooltip-border-width 2
        lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        lsp-bridge-signature-show-with-frame-position "point"
        lsp-bridge-python-lsp-server "basedpyright"
        lsp-bridge-python-multi-lsp-server "basedpyright_ruff"
        ;; lsp-bridge-markdown-lsp-server "vale-ls"
        acm-backend-yas-match-by-trigger-keyword t
        acm-backend-search-file-words-enable-fuzzy-match t
        acm-enable-capf t
        acm-enable-tabnine nil
        acm-enable-copilot nil
        acm-enable-preview t
        acm-backend-lsp-show-progress t)

  (if (modulep! :completion vertico)
      (setq acm-candidate-match-function 'orderless-flex))

  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(gfm-mode . lsp-bridge-markdown-lsp-server) t)

  (add-to-list 'lsp-bridge-completion-stop-commands "vundo-backward")

  (global-lsp-bridge-mode)

  (defadvice! bc/acm-markdown-render-content-after-advice (&rest args)
    :after #'acm-markdown-render-content
    (goto-line 1)
    (when (modulep! :checkers syntax)
      (cond ((bound-and-true-p flymake-mode) (flymake-mode-off))
            ((bound-and-true-p flycheck-mode) (flycheck-mode -1)))))

  (when (modulep! :checkers syntax -flymake)
    (defun bc/lsp-bridge--disable-flycheck()
      (if (bound-and-true-p flycheck-mode)
          (flycheck-mode -1)))

    (dolist (hook lsp-bridge-default-mode-hooks)
      (add-hook hook #'bc/lsp-bridge--disable-flycheck t)))

  (after! insert-translated-name
    (defadvice! bc/disable-lsp-bridge-when-active-insert-translated-name (&rest _)
      :before #'insert-translated-name-active
      (lsp-bridge-mode -1))

    (defadvice! bc/enable-lsp-bridge-when-inactive-insert-translated-name (&rest _)
      :after #'insert-translated-name-inactive
      (lsp-bridge-mode 1)))

  (let ((lombok-jar-path (expand-file-name "lombok.jar" doom-user-dir)))
    (setq lsp-bridge-jdtls-jvm-args (list "-Dfile.encoding=utf8"
                                          "-server"
                                          "-Xmx2G"
                                          "-Xss512K"
                                          "-XX:MetaspaceSize=256M"
                                          "-XX:MaxMetaspaceSize=256M"
                                          "-XX:InitialCodeCacheSize=128M"
                                          "-XX:ReservedCodeCacheSize=128M"
                                          "-XX:MaxDirectMemorySize=512M"
                                          "-XX:+UnlockExperimentalVMOptions"
                                          "-XX:+UseNUMA"
                                          "-XX:+UseZGC"
                                          "-XX:ConcGCThreads=4"
                                          "-XX:MaxGCPauseMillis=200"
                                          "-XX:+ZUncommit"
                                          "-XX:ZUncommitDelay=10"
                                          (concat "-javaagent:" lombok-jar-path))))
  (setq lsp-bridge-jdtls-default-file (expand-file-name "lsp-bridge/langserver/jdtls.json" doom-user-dir))

  ;; (setq lsp-bridge-get-multi-lang-server-by-project
  ;;       (lambda (_project-path filepath)
  ;;         ;; If typescript file include deno.land url, then use Deno LSP server.
  ;;         (save-excursion
  ;;           (when (string-equal (file-name-extension filepath) "ts")
  ;;             (cl-dolist (buf (buffer-list))
  ;;               (when (string-equal (buffer-file-name buf) filepath)
  ;;                 (with-current-buffer buf
  ;;                   (goto-char (point-min))
  ;;                   (when (search-forward-regexp (regexp-quote "from \"https://deno.land") nil t)
  ;;                     (cl-return "deno")))))))))

  (defadvice! bc/lsp-bridge--not-in-multiple-cursors-override-advice ()
    :override #'lsp-bridge--not-in-multiple-cursors
    (not (and (featurep 'multiple-cursors-core)
              multiple-cursors-mode)))

  (map! :leader
        (:prefix-map ("c" . "code")
         :desc "LSP Code actions"            "a"  #'lsp-bridge-code-action
         :desc "Jump to symbol in workspace" "j"  #'lsp-bridge-workspace-list-symbols
         :desc "LSP Rename"                  "r"  #'lsp-bridge-rename
         :desc "LSP Peek"                    "p"  #'lsp-bridge-peek))

  (defadvice! +format/region-or-buffer-a (fn &rest args)
    :around #'+format/region-or-buffer
    (if (and (not (memq major-mode '(java-mode java-ts-mode)))
             (bound-and-true-p lsp-bridge-mode)
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

  (defadvice! bc/lsp-bridge--better-jumper-jump-backward-around-advice (fn &rest args)
    :around #'better-jumper-jump-backward
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-find-def-return)
      (apply fn args))))

(use-package! flymake-bridge
  :when (modulep! :checkers syntax +flymake)
  :after lsp-bridge
  :config
  (add-hook! lsp-bridge-mode #'flymake-bridge-setup))
