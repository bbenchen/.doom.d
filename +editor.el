;;; +editor.el -*- lexical-binding: t; -*-

(setq-default fill-column 120)

;; iedit
(setq iedit-toggle-key-default nil)

;; multiple-cursors
(after! multiple-cursors-core
  (if (functionp 'counsel-M-x)
      (add-to-list 'mc/cmds-to-run-once 'counsel-M-x))
  (add-to-list 'mc/cmds-to-run-for-all 'hungry-delete-forward)
  (add-to-list 'mc/cmds-to-run-for-all 'hungry-delete-backward))

(after! smartparens
  (unbind-key "M-<left>" smartparens-mode-map)
  (unbind-key "M-<right>" smartparens-mode-map))

;; hungry-delete
(use-package! hungry-delete
  :config
  (add-hook! (prog-mode text-mode conf-mode) #'hungry-delete-mode)
  (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; centered-cursor
(use-package! centered-cursor-mode
  :commands (centered-cursor-mode
             global-centered-cursor-mode)
  :init
  ;; (add-hook! (prog-mode text-mode conf-mode) #'centered-cursor-mode)
  (map! :leader
        :desc "Centered point" "C--" #'centered-cursor-mode)
  :config
  (setq ccm-recenter-at-end-of-file t))

;; goto-line-preview
(use-package! goto-line-preview
  :defer t
  :init
  (global-set-key [remap goto-line] #'goto-line-preview))

;; auto-save
(use-package! auto-save
  :config
  (setq auto-save-idle 5
        auto-save-silent t)   ; quietly save

  ;; custom predicates if you don't want auto save.
  ;; disable auto save mode when current filetype is an gpg file.
  (setq auto-save-disable-predicates
        '((lambda ()
            (or (string-suffix-p
                 "gpg"
                 (file-name-extension (buffer-name)) t)
                (bound-and-true-p org-msg-mode)))))
  (auto-save-enable))

;; duplicate-line
(use-package! duplicate-line
  :when (<= emacs-major-version 28))

(map! "C-s" #'phi-search
      "s-F" #'phi-search
      "C-r" #'phi-search-backward
      "s-d" #'phi-search-backward
      "M-%" #'phi-replace)

;; treesit
(use-package! treesit
  :when (treesit-available-p)
  :config
  (setq treesit-extra-load-path (list (expand-file-name "tree-sitter" doom-cache-dir)))

  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (proto . ("https://github.com/mitchellh/tree-sitter-proto"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))

  (setq major-mode-remap-alist
        '((dockerfile-mode . dockerfile-ts-mode)
          (json-mode . json-ts-mode)
          (js-json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (conf-toml-mode . toml-ts-mode)))

  (add-hook! (emacs-lisp-mode ielm-mode)
    (treesit-parser-create 'elisp))

  (add-hook! markdown-mode
    (treesit-parser-create 'markdown))

  (add-hook! org-mode
    (treesit-parser-create 'org))

  (add-hook! protobuf-mode
    (treesit-parser-create 'proto))

  ;; (add-hook! scala-mode
  ;;   (treesit-parser-create 'scala))

  (add-hook! sql-mode
    (treesit-parser-create 'sql)))
