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
  (add-hook! (prog-mode text-mode conf-mode) #'centered-cursor-mode)
  (map! :leader
        :desc "Centered point" "C--" #'centered-cursor-mode)
  :config
  (setq ccm-recenter-at-end-of-file t))

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
