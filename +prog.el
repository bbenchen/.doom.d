;;; +prog.el -*- lexical-binding: t; -*-

(after! prog-mode
  (set-company-backend! 'prog-mode '(:separate company-yasnippet company-capf)))

(after! company
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (add-to-list #'company-transformers #'delete-dups))

(after! company-yasnippet
  (defadvice! company-yasnippet-disable-inline-a (fn cmd &optional arg &rest _ignore)
    "Enable yasnippet but disable it inline."
    :around #'company-yasnippet
    (if (eq cmd  'prefix)
        (when-let ((prefix (funcall fn 'prefix)))
          (unless (memq (char-before (- (point) (length prefix)))
                        '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
            prefix))
      (progn
        (when (and (bound-and-true-p lsp-mode)
                   arg (not (get-text-property 0 'yas-annotation-patch arg)))
          (let* ((name (get-text-property 0 'yas-annotation arg))
                 (snip (format "%s (Snippet)" name))
                 (len (length arg)))
            (put-text-property 0 len 'yas-annotation snip arg)
            (put-text-property 0 len 'yas-annotation-patch t arg)))
        (funcall fn cmd  arg)))))

(if (modulep! :editor file-templates)
    (set-file-template! "/pom\\.xml$" :trigger "__pom.xml" :mode 'nxml-mode))

(after! nxml-mode
  (setq nxml-auto-insert-xml-declaration-flag nil
        nxml-slash-auto-complete-flag nil)

  (unless (modulep! :lang web +lsp)
    (add-hook! 'nxml-mode-local-vars-hook :append #'lsp!))

  (if (modulep! :checkers spell)
      (add-hook! 'nxml-mode-hook
        (defun disable-spell-for-xml()
          (if (modulep! :checkers spell +flyspell)
              (flyspell-mode -1)
            (spell-fu-mode -1))))))

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

(if (modulep! :editor snippets)
    (setq +lsp-company-backends '(:separate company-yasnippet company-capf)))

;; lsp
(after! lsp-mode
  (setq lsp-response-timeout 10
        lsp-enable-snippet nil
        lsp-enable-folding nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil
        lsp-file-watch-threshold 5000
        lsp-eldoc-enable-hover nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-auto-activate nil
        lsp-headerline-breadcrumb-enable nil)

  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored-directories) ;; json

  ;; kotlin
  (setq lsp-clients-kotlin-server-executable (concat lsp-server-install-dir "kotlin/server/bin/kotlin-language-server")
        lsp-kotlin-debug-adapter-path (concat lsp-server-install-dir "kotlin/adapter/bin/kotlin-debug-adapter"))

  ;; xml
  (setq lsp-xml-jar-file (expand-file-name "org.eclipse.lemminx-0.22.0-uber.jar" lsp-server-install-dir))

  ;; Disable `lsp-mode' in `git-timemachine-mode'
  (defadvice! lsp--init-if-visible-a (fn &rest args)
    :around #'lsp--init-if-visible
    (unless (bound-and-true-p git-timemachine-mode)
      (apply fn args))))

(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil))

(after! lsp-treemacs
  (lsp-treemacs-sync-mode -1)

  (map! (:map lsp-mode-map
              ("C-<f8>" #'lsp-treemacs-java-deps-list)
              ("M-<f8>" #'lsp-treemacs-symbols)))

  (after! ace-window
    (when (boundp 'aw-ignored-buffers)
      (push lsp-treemacs-symbols-buffer-name aw-ignored-buffers)
      (push lsp-treemacs-deps-buffer-name aw-ignored-buffers)
      (push lsp-treemacs-errors-buffer-name aw-ignored-buffers)))

  (after! all-the-icons
    (treemacs-create-theme "lsp-colors"
                           :extends "doom-colors"
                           :config
                           (progn
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
                              :extensions (root))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                              :extensions (boolean-data))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
                              :extensions (class))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
                              :extensions (color-palette))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
                              :extensions (constant))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
                              :extensions (document))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
                              :extensions (enumerator))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                              :extensions (enumitem))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
                              :extensions (event))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                              :extensions (field))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
                              :extensions (indexer))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
                              :extensions (intellisense-keyword))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                              :extensions (interface))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                              :extensions (localvariable))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
                              :extensions (method))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                              :extensions (namespace))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
                              :extensions (numeric))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
                              :extensions (operator))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
                              :extensions (property))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
                              :extensions (snippet))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
                              :extensions (string))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
                              :extensions (structure))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
                              :extensions (template))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                              :extensions (collapsed) :fallback "+")
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                              :extensions (expanded) :fallback "-")
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
                              :extensions (classfile))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
                              :extensions (default-folder-opened))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
                              :extensions (default-folder))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
                              :extensions (default-root-folder-opened))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
                              :extensions (default-root-folder))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                              :extensions ("class"))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                              :extensions (file-type-jar))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                              :extensions (folder-open))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                              :extensions (folder))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
                              :extensions (folder-type-component-opened))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
                              :extensions (folder-type-component))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
                              :extensions (folder-type-library-opened))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
                              :extensions (folder-type-library))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
                              :extensions (folder-type-maven-opened))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
                              :extensions (folder-type-maven))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
                              :extensions (folder-type-package-opened))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
                              :extensions (folder-type-package))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                              :extensions (icon-create))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                              :extensions (icon-flat))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
                              :extensions (icon-hierarchical))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                              :extensions (icon-link))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                              :extensions (icon-refresh))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                              :extensions (icon-unlink))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
                              :extensions (jar))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
                              :extensions (library))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
                              :extensions (packagefolder-open))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                              :extensions (packagefolder))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                              :extensions (package))
                             (treemacs-create-icon
                              :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
                              :extensions (java-project))))
    (setq lsp-treemacs-theme "lsp-colors")))

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
                                                 go-staticcheck))))

  (when (modulep! :tools lsp)
    (defadvice! godef-describe-a (_point)
      :override #'godef-describe
      (lsp-describe-thing-at-point)))

  (map! (:when (modulep! :tools lsp)
          (:map go-mode-map
           :localleader
           (:prefix ("r" . "reflect")
            :desc "rename" "r" #'lsp-rename)))))

(after! lsp-mode
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t))))

(use-package! go-fill-struct
  :defer t
  :init
  (map! :map go-mode-map
        :localleader
        (:prefix ("r" . "reflect")
                 "s" #'go-fill-struct)))

;; java
(after! lsp-java
  (let ((lombok-jar-path (expand-file-name "lombok.jar" doom-user-dir)))
    (setq lsp-java-vmargs (list "-Dfile.encoding=utf8"
                                "-server"
                                "-noverify"
                                "-Xms6G"
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

  (setq lsp-java-configuration-runtimes (cond (IS-MAC '[(:name "JavaSE-17"
                                                         :path "/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home"
                                                         :default t)
                                                        (:name "JavaSE-11"
                                                         :path "/Library/Java/JavaVirtualMachines/temurin-11.jdk/Contents/Home")
                                                        (:name "JavaSE-1.8"
                                                         :path "/Library/Java/JavaVirtualMachines/temurin-8.jdk/Contents/Home")])
                                              (IS-LINUX '[(:name "JavaSE-17"
                                                           :path "/usr/lib/jvm/java-17-openjdk"
                                                           :default t)
                                                          (:name "JavaSE-11"
                                                           :path "/usr/lib/jvm/java-11-openjdk")
                                                          (:name "JavaSE-1.8"
                                                           :path "/usr/lib/jvm/java-8-openjdk")])
                                              (t nil)))

  (if-let ((java-home (getenv "JAVA_HOME")))
      (setq lsp-java-java-path (concat java-home "/bin/java")))

  ;; (require 'lsp-java-boot)

  ;; (add-hook! 'lsp-mode-hook #'lsp-lens-mode)
  ;; (add-hook! 'java-mode-hook #'lsp-java-boot-lens-mode)

  (setq lsp-java-import-gradle-enabled nil
        lsp-java-format-enabled nil
        lsp-java-format-comments-enabled nil
        lsp-java-format-on-type-enabled nil
        lsp-java-max-concurrent-builds 5
        lsp-java-completion-max-results 30
        lsp-java-folding-range-enabled nil
        lsp-java-signature-help-enabled nil
        lsp-java-selection-enabled nil
        lsp-java-trace-server "messages"
        lsp-java-maven-download-sources t
        lsp-java-sources-organize-imports-star-threshold 5
        lsp-java-sources-organize-imports-static-star-threshold 3
        ;; Support java decompiler
        lsp-java-content-provider-preferred "fernflower")

  (setq lsp-java-completion-favorite-static-members ["org.junit.Assert.*" "org.junit.Assume.*" "org.junit.jupiter.api.Assertions.*" "org.junit.jupiter.api.Assumptions.*" "org.junit.jupiter.api.DynamicContainer.*" "org.junit.jupiter.api.DynamicTest.*" "org.mockito.Mockito.*" "org.mockito.ArgumentMatchers.*" "org.mockito.Answers.*" "org.hamcrest.MatcherAssert.*" "org.hamcrest.Matchers.*"])

  (if (modulep! :editor format)
      (progn
        (set-formatter! 'google-java-format
          '("google-java-format" "-" "-a" "-" "--skip-sorting-imports")
          :modes 'java-mode)

        (setq-hook! 'java-mode-hook
          tab-width 4
          fill-column 120))
    (setq lsp-java-format-enabled t
          lsp-java-format-comments-enabled t)))

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

;; lsp-metals
(after! lsp-metals
  (if-let ((java-home (getenv "JAVA_HOME")))
      (setq lsp-metals-java-home java-home))

  (setq lsp-metals-sbt-script "sbt"
        lsp-metals-maven-script "mvn"
        lsp-metals-bloop-sbt-already-installed t
        lsp-metals-super-method-lenses-enabled t
        lsp-metals-show-implicit-arguments t
        lsp-metals-show-inferred-type t))

;; sql
(after! sql
  (setq sql-product 'oracle)

  ;; (after! lsp-sqls
  ;;   (defadvice! lsp-sqls--make-launch-cmd-a ()
  ;;     :override #'lsp-sqls--make-launch-cmd
  ;;     (-let [base `(,lsp-sqls-server "-l" "/tmp/sqls.log" "-t")]
  ;;       base)))
  ;; (setq lsp-sqls-workspace-config-path "root")
  ;; (add-hook! 'sql-mode-local-vars-hook :append #'lsp!)

  ;; (if (modulep! :checkers syntax)
  ;;   (add-hook! '(sql-mode-hook sql-interactive-mode-hook) :append
  ;;     (defun sql-disable-flycheck()
  ;;       "Disable `flycheck' for the current buffer."
  ;;       (flycheck-mode -1))))
  )

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
