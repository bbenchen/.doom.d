;;; +editor.el -*- lexical-binding: t; -*-

(setq-default fill-column 120)

;; narrow
(put 'narrow-to-region 'disabled nil)

;; iedit
(setq iedit-toggle-key-default nil)

;; multiple-cursors
(after! multiple-cursors-core
  (if (functionp 'counsel-M-x)
      (add-to-list 'mc--default-cmds-to-run-once 'counsel-M-x))
  (add-to-list 'mc--default-cmds-to-run-once 'pixel-scroll-precision)
  (add-to-list 'mc--default-cmds-to-run-once 'beginning-of-buffer)
  (add-to-list 'mc--default-cmds-to-run-once 'end-of-buffer)
  (add-to-list 'mc--default-cmds-to-run-for-all 'hungry-delete-forward)
  (add-to-list 'mc--default-cmds-to-run-for-all 'hungry-delete-backward))

(after! smartparens
  (unbind-key "M-<left>" smartparens-mode-map)
  (unbind-key "M-<right>" smartparens-mode-map))

;; hungry-delete
(use-package! hungry-delete
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v")

  (add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'hungry-delete-mode))

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

;; auto-save
(use-package! auto-save
  :config
  (setq auto-save-idle 5
        auto-save-silent t ; quietly save
        auto-save-delete-trailing-whitespace t)

  ;; custom predicates if you don't want auto save.
  ;; disable auto save mode when current filetype is an gpg file.
  (setq auto-save-disable-predicates
        '((lambda ()
            (or (string-suffix-p
                 "gpg"
                 (file-name-extension (buffer-name)) t)
                (bound-and-true-p org-msg-mode)))))
  (auto-save-enable))

;; thing-edit
(use-package! thing-edit)

;; region-occurrences-highlighter
(use-package! region-occurrences-highlighter
  :config
  (map! :map region-occurrences-highlighter-nav-mode-map
        "M-n" #'region-occurrences-highlighter-next
        "M-p" #'region-occurrences-highlighter-prev)

  (add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'region-occurrences-highlighter-mode))

;; visual-replace
(use-package! visual-replace
  :config
  (setq visual-replace-display-total t)

  (add-hook! 'doom-after-init-hook :append
    (visual-replace-global-mode 1)))

;; deno-bridge-jieba
(use-package! deno-bridge-jieba
  :load-path (lambda () (list (expand-file-name "site-lisp/deno-bridge-jieba" doom-user-dir)))
  :config
  (define-key!
    [remap forward-word]       #'deno-bridge-jieba-forward-word
    [remap backward-word]      #'deno-bridge-jieba-backward-word
    [remap kill-word]          #'deno-bridge-jieba-kill-word
    [remap backward-kill-word] #'deno-bridge-jieba-backward-kill-word
    [remap mark-word]          #'deno-bridge-jieba-mark-word))

;; treesit
(after! treesit
  (setq treesit-font-lock-level 4))
