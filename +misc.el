;;; +misc.el -*- lexical-binding: t; -*-

(use-cjk-char-width-table 'zh_CN)
(setq system-time-locale "zh_CN")

(setq confirm-kill-processes nil
      confirm-kill-emacs nil
      x-select-enable-clipboard-manager nil)

(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))

;; recentf
(after! recentf
  (add-to-list 'recentf-exclude "\\.mail")
  (add-to-list 'recentf-exclude "/var")
  (add-to-list 'recentf-exclude "/autosave"))

;; workspaces
(setq +workspaces-on-switch-project-behavior t)

;; projectile
(after! projectile
  (setq projectile-project-search-path '("~/"))

  (dolist (suffix '(".bak" ".exe"))
    (add-to-list 'projectile-globally-ignored-file-suffixes suffix)))

;; vertico
(if (modulep! :completion vertico +childframe)
    (after! vertico-posframe
      (custom-set-faces!
        `(vertico-posframe :foreground ,(doom-color 'modeline-fg) :background ,(doom-color 'modeline-bg))
        `(vertico-posframe-border :background ,(doom-color 'modeline-bg)))))

;; modeline
(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-persp-name t
        doom-modeline-enable-word-count t
        doom-modeline-indent-info t
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-gnus-idle 5
        doom-modeline-support-imenu t)

  (setq all-the-icons-scale-factor 1.1)

  (setq display-time-format "%D %R") ;；set modeline time format

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip mu4e gnus debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time "    ")))

;; which-key-posframe
(use-package! which-key-posframe
  :config
  (custom-set-faces!
    `(which-key-posframe :foreground ,(doom-color 'modeline-fg) :background ,(doom-color 'modeline-bg))
    `(which-key-posframe-border :background ,(doom-color 'modeline-bg)))

  (setq which-key-posframe-border-width 1
        which-key-posframe-poshandler 'posframe-poshandler-frame-center)

  (which-key-posframe-mode))

;; lookup
(when (and IS-MAC
           (featurep 'xwidget-internal))
  (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn
        browse-url-browser-function #'xwidget-webkit-browse-url)
  (if (boundp 'xwidget-webkit-enable-plugins)
      (setq xwidget-webkit-enable-plugins t)))

(after! xref
  (cond ((executable-find "ugrep") (setq xref-search-program 'ugrep))
        ((executable-find "rg") (setq xref-search-program 'ripgrep))))

;; vterm
(after! vterm
  (setq vterm-disable-underline t))

;; dired
(after! dired
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  (if (boundp 'dired-kill-when-opening-new-dired-buffer)
      (setq dired-kill-when-opening-new-dired-buffer t)))

;; spell
(after! ispell
  (setq ispell-dictionary "en_US")
  (setq ispell-alternate-dictionary (expand-file-name "english-words" doom-user-dir))

  (advice-add #'ispell-lookup-words :around #'doom-shut-up-a))

;; magit
(after! magit
  (setq magit-revision-show-gravatars nil)

  (remove-hook! 'server-switch-hook #'magit-commit-diff))

(after! magit-todos
  (setq magit-todos-submodule-list t
        magit-todos-exclude-globs (append magit-todos-exclude-globs '(".svn/" "node_modules/*"))))

(after! git-commit
  (if (modulep! :checkers spell +flyspell)
      (remove-hook! 'git-commit-mode-hook #'flyspell-mode)))

(when (modulep! :tools magit)
  ;; magit-todos
  (if (executable-find "nice")
      (setq magit-todos-nice t))
  (if (executable-find "rg")
      (setq magit-todos-scanner 'magit-todos--scan-with-rg))

  ;; blamer
  (use-package! blamer
    :defer 5
    :custom
    (blamer-idle-time 0.5)
    (blamer-min-offset 70)
    :init
    (map! (:prefix-map ("v" . "versioning")
           :desc "Blamer show commit" "b" #'blamer-show-posframe-commit-info
           :desc "Blamer" "B" #'blamer-mode))
    :config
    (custom-set-faces!
      `(blamer-face :foreground "#7a88cf" :background nil :italic t))))

;; exec-path-from-shell
(use-package! exec-path-from-shell
  :when IS-MAC
  :init
  (setq exec-path-from-shell-warn-duration-millis 2000)
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables '("LANG"
                                         "LC_ALL"
                                         "TERM"
                                         "PATH"
                                         "MANPATH"
                                         "JAVA_HOME"
                                         "JAVA_OPTS"
                                         "SBT_OPTS"
                                         "GOPATH"
                                         "GOBIN"
                                         "GO111MODULE"
                                         "GOPROXY"
                                         "LSP_USE_PLISTS"))
  ;; (setq exec-path-from-shell-debug t)
  (exec-path-from-shell-initialize))

;; command-log
(use-package! command-log-mode
  :commands global-command-log-mode
  :init
  (setq command-log-mode-auto-show nil)

  (defvar command-window-frame nil)

  (defun toggle-command-window ()
    "Show or hide the command window"
    (interactive)
    (if (posframe-workable-p)
        (progn
          (if command-window-frame
              (progn
                (global-command-log-mode -1)
                (posframe-delete-frame clm/command-log-buffer)
                (setq command-window-frame nil))
            (progn
              (global-command-log-mode 1)
              (with-current-buffer
                  (setq clm/command-log-buffer
                        (get-buffer-create " *command-log*"))
                (text-scale-set -0.5))
              (setq command-window-frame
                    (posframe-show
                     clm/command-log-buffer
                     :position `(,(- (x-display-pixel-width) 420) . 0)
                     :width 55
                     :height 5
                     :min-width 55
                     :min-height 5
                     :internal-border-width 10
                     :background-color (face-background 'mode-line)
                     :foreground-color (face-foreground 'mode-line)
                     :override-parameters '((parent-frame . nil)))))))
      (progn
        (if (not command-log-mode-auto-show)
            (setq command-log-mode-auto-show t))
        (if (bound-and-true-p global-command-log-mode)
            (global-command-log-mode -1)
          (global-command-log-mode 1))))))
