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

;; super-save
(use-package! super-save
  :config
  (setq super-save-auto-save-when-idle t)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg"))
  (super-save-mode +1))

(map! "C-s" #'phi-search
      "s-F" #'phi-search
      "C-r" #'phi-search-backward
      "s-d" #'phi-search-backward
      "M-%" #'phi-replace)
