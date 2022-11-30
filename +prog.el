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
      (if-let (project-root (doom-project-root))
          (sort (process-lines "gopkgs" "-workDir" project-root) #'string<)
        (sort (process-lines "gopkgs") #'string<)))
    (setq go-packages-function 'go-packages-gopkgs))

  (if (executable-find "gogetdoc")
      (setq godoc-at-point-function 'godoc-gogetdoc))

  (let ((command (or (executable-find "gofumpt")
                     (executable-find "goimports"))))
    (if command
        (setq gofmt-command command)))

  ;; (add-hook 'before-save-hook #'gofmt-before-save)

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
(after! java-mode
  (if (modulep! :editor format)
      (progn
        (set-formatter! 'google-java-format
          '("google-java-format" "-" "-a" "-" "--skip-sorting-imports")
          :modes 'java-mode)))

  (setq-hook! 'java-mode-hook
    tab-width 4
    fill-column 120))

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
  (setq lsp-bridge-enable-mode-line nil
        lsp-bridge-enable-log nil)
  :config
  (global-lsp-bridge-mode)

  (setq lsp-bridge-lookup-doc-tooltip-border-width 2
        lsp-bridge-lookup-doc-tooltip-max-height 30)

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

  (setq lsp-bridge-get-single-lang-server-by-project
        (lambda (_project-path filepath)
          ;; If typescript file include deno.land url, then use Deno LSP server.
          (save-excursion
            (when (string-equal (file-name-extension filepath) "ts")
              (dolist (buf (buffer-list))
                (when (string-equal (buffer-file-name buf) filepath)
                  (with-current-buffer buf
                    (goto-char (point-min))
                    (when (search-forward-regexp (regexp-quote "from \"https://deno.land") nil t)
                      (cl-return "deno")))))))))

  (defadvice! lsp-bridge-frame-background-color-a ()
    :override #'lsp-bridge-frame-background-color
    (doom-color 'modeline-bg))

  (defadvice! +lookup/definition-a (fn &rest args)
    :around #'+lookup/definition
    (if (and lsp-bridge-mode
             (lsp-bridge-has-lsp-server-p))
        (lsp-bridge-find-def)
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
