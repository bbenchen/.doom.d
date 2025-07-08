;;; +misc.el -*- lexical-binding: t; -*-

(menu-bar-mode -1)

(use-cjk-char-width-table 'zh_CN)
(setq system-time-locale "C")

(setq confirm-kill-processes nil
      confirm-kill-emacs nil)

(if (featurep :system 'linux)
    (setq x-select-enable-clipboard-manager nil))

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq mode-line-right-align-edge 'right-margin)

(setq doom-fd-executable "fd"
      doom-ripgrep-executable "rg")

;; docker
(defun bc/set-docker-host-from-podman ()
  (unless (file-exists-p "/var/run/docker.sock")
    (when (and (featurep :system 'macos)
               (executable-find "podman"))
      (let* ((res (cond
                   ((featurep :system 'linux) (doom-call-process "podman" "info" "--format" "{{.Host.RemoteSocket.Path"))
                   (t (doom-call-process "podman" "machine" "inspect" "--format" "{{.ConnectionInfo.PodmanSocket.Path}}"))))
             (code (car res)))
        (if (zerop code)
            (setenv "DOCKER_HOST" (format "unix://%s" (cdr res))))))))
(add-hook! 'doom-after-init-hook #'bc/set-docker-host-from-podman)

;; recentf
(after! recentf
  (add-to-list 'recentf-exclude "\\.cache")
  (add-to-list 'recentf-exclude "\\.local/straight")
  (add-to-list 'recentf-exclude "\\.mail")
  (add-to-list 'recentf-exclude "/var")
  (add-to-list 'recentf-exclude "/autosave"))

;; tramp
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(after! tramp
  (setq remote-file-name-inhibit-locks t
        remote-file-name-inhibit-auto-save-visited t
        tramp-use-scp-direct-remote-copying t
        tramp-copy-size-limit (* 1024 1023) ;; 1MB
        tramp-verbose 2)

  (after! compile
    (remove-hook! 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; epa
(after! epa
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

;; winner
(after! winner
  ;; fix: wrong type argument: frame-live-p, #<dead frame
  (defadvice! bc/winner-save-old-configurations-before-advice ()
    "Remove dead frames from `winner-modified-list`"
    :before #'winner-save-old-configurations
    (cl-dolist (frame winner-modified-list)
      (unless (frame-live-p frame)
        (delq! frame winner-modified-list)))))

;; workspaces
(setq +workspaces-on-switch-project-behavior t)
(after! persp-mode
  (add-hook! 'persp-filter-save-buffers-functions
    (defun bc/workdspace--temporary-buffer-p (buf)
      ;; "Ignore temporary buffers."
      (let ((bname (file-name-nondirectory (buffer-name buf))))
        (or (string-prefix-p ".newsrc" bname)
            (string-prefix-p "magit" bname)
            (string-prefix-p "Pfuture-Callback" bname)
            (and (string-prefix-p "*" bname)
                 (string-suffix-p "*" bname))
            (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
            (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)
            (eq (buffer-local-value 'major-mode buf) 'vterm-mode))))))

;; projectile
(after! projectile
  (setq projectile-project-root-files-bottom-up (delete ".project" projectile-project-root-files-bottom-up))
  (advice-remove 'projectile-dirconfig-file #'doom--projectile-dirconfig-file-a)
  (setq projectile-project-search-path
        '(("~/Projects/emacs" . 1)
          ("~/Projects/getech/sources" . 1)
          ("~/Projects/golang" . 1)
          ("~/Projects/java" . 1)
          ("~/Projects/scala" . 1)))

  (dolist (suffix '(".bak" ".exe"))
    (add-to-list 'projectile-globally-ignored-file-suffixes suffix)))

;; vertico
(when (modulep! :completion vertico)
  (if (modulep! :completion vertico +childframe)
      (after! vertico-posframe
        (add-hook! 'doom-load-theme-hook :append
          (set-face-foreground 'vertico-posframe (doom-color 'modeline-fg))
          (set-face-background 'vertico-posframe (doom-color 'modeline-bg))
          (set-face-background 'vertico-posframe-border (doom-color 'modeline-bg)))))

  ;; Support Pinyin
  (use-package! pinyinlib
    :after orderless
    :init
    (defun orderless-regexp-pinyin (str)
      (orderless-regexp (pinyinlib-build-regexp-string str)))
    (add-to-list 'orderless-matching-styles 'orderless-regexp-pinyin)))

;; golden-ratio-scroll-screen
(use-package! golden-ratio-scroll-screen
  :config
  (add-hook! 'doom-load-theme-hook :append
    (set-face-foreground 'golden-ratio-scroll-highlight-line-face 'unspecified)
    (set-face-background 'golden-ratio-scroll-highlight-line-face (doom-color 'modeline-bg)))

  (map! [remap scroll-down-command] #'golden-ratio-scroll-screen-down
        [remap scroll-up-command]   #'golden-ratio-scroll-screen-up))

;; modeline
(setq +modeline-height 24)
;; set modeline time format
(setq display-time-format "%D %R")

;; rainbow
(after! rainbow-mode
  (add-hook! 'rainbow-mode-hook
    (hl-line-mode (if rainbow-mode -1 +1))))

;; treemacs
(after! (:and treemacs ace-window)
  (delq! 'treemacs-mode aw-ignored-buffers))

;; lookup
(when (and (featurep :system 'macos) (featurep 'xwidget-internal))
  (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn
        browse-url-browser-function #'+lookup-xwidget-webkit-open-url-fn)
  (if (boundp 'xwidget-webkit-enable-plugins)
      (setq xwidget-webkit-enable-plugins t)))
(add-to-list '+lookup-provider-url-alist '("Maven Repository" "http://mvnrepository.com/search?q=%s&ref=opensearch"))

;; xref
(after! xref
  (cond ((executable-find "ugrep") (setq xref-search-program 'ugrep))
        ((executable-find "rg") (setq xref-search-program 'ripgrep))))

;; editorconfig
(when (modulep! :tools editorconfig)
  (unless (boundp 'editorconfig-exclude-regexps)
    (defvar editorconfig-exclude-regexps nil)))

;; vterm
(setq vterm-always-compile-module nil)
(after! vterm
  (setq vterm-disable-underline t
        vterm-timer-delay 0.01))

;; dired
(after! dired
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  (if (boundp 'dired-kill-when-opening-new-dired-buffer)
      (setq dired-kill-when-opening-new-dired-buffer t))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil))

;; flymake
(after! flymake-popon
  (setq flymake-popon-posframe-border-width 0)
  (add-hook! 'doom-load-theme-hook :append
    (set-face-foreground 'flymake-popon (doom-color 'modeline-fg))
    (set-face-background 'flymake-popon (doom-color 'modeline-bg))))

;; spell
(setq ispell-dictionary "en_US")
;; (setq ispell-alternate-dictionary (expand-file-name "english-words" doom-user-dir))
(after! ispell
  (advice-add #'ispell-lookup-words :around #'doom-shut-up-a))
(after! spell-fu
  (setq! spell-fu-idle-delay 0.5))

;; with-editor
(if (featurep :system 'macos)
    (if (string-prefix-p "aarch64" system-configuration)
        (setq with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient")
      (setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")))

;; magit
(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (if (featurep 'nerd-icons)
      (setq magit-format-file-function #'magit-format-file-nerd-icons)))

;; ghub
(after! ghub
  (setq ghub-json-use-jansson t))
;; code-review
(after! code-review
  (add-hook 'code-review-mode-hook
            (lambda ()
              ;; include *Code-Review* buffer into current workspace
              (persp-add-buffer (current-buffer)))))

;; magit-todos
(after! magit-todos
  (setq magit-todos-submodule-list t
        magit-todos-exclude-globs (append magit-todos-exclude-globs '(".svn/" "node_modules/*")))
  (if (executable-find "nice")
      (setq magit-todos-nice t))
  (if (executable-find "rg")
      (setq magit-todos-scanner 'magit-todos--scan-with-rg)))

(after! git-commit
  (setq git-commit-summary-max-length 60))

(use-package! git-commit-ts-mode
  :mode "\\COMMIT_EDITMSG\\'"
  :config
  (setq git-commit-major-mode #'git-commit-ts-mode))

;; blamer
(use-package! blamer
  :when (modulep! :tools magit)
  :config
  (setq blamer-idle-time 0.5
        blamer-min-offset 70)

  (add-hook! 'doom-load-theme-hook :append
    (plist-put! blamer-posframe-configurations :internal-border-color (doom-color 'modeline-bg))
    (set-face-foreground 'blamer-face (doom-color 'blue))))

;; magit-gerrit
(use-package! magit-gerrit
  :when (modulep! :tools magit)
  :after magit)

;; diff-hl
(after! diff-hl
  (setq diff-hl-disable-on-remote t
        diff-hl-update-async (not (featurep :system 'macos)))
  (setq diff-hl-global-modes '(not helpful-mode image-mode pdf-view-mode vterm-mode magit-status-mode)))

;; pinentry
;; (use-package! pinentry
;;   :config
;;   (setq epa-pinentry-mode 'loopback
;;         pinentry-prompt-window-height 8)
;;   (pinentry-start))

;; envrc
(use-package! envrc
  :config
  (set-popup-rule! "^\\*envrc\\*" :quit t :ttl 0)

  (add-hook! 'doom-after-init-hook :append
    (when (executable-find "direnv")
      (envrc-global-mode))))

;; mb-url
(use-package! mb-url-http
  :defer t
  :commands mb-url-http-around-advice
  :init
  (setq mb-url-http-backend 'mb-url-http-curl)
  (advice-add 'url-http :around #'mb-url-http-around-advice))

;; pass
(after! pass
  (defadvice! bc/pass-view-override-advice ()
    :override #'pass-view
    (pass--with-closest-entry entry
      (find-file-other-window (concat (f-join (password-store-dir) entry) ".gpg"))))

  (defun bc/pass-view-quit ()
    (interactive)
    (kill-current-buffer)
    (if (modulep! :ui workspaces)
        (+workspace/close-window-or-workspace)
      (quit-window t)))
  (map! :map pass-view-mode-map
        "C-c C-q" #'bc/pass-view-quit))

;; popweb
(use-package! popweb
  :defer 1
  :config
  (setq popweb-proxy-type "http"
        popweb-proxy-host "127.0.0.1"
        popweb-proxy-port "20122")

  (let ((dir (file-name-directory (locate-library "popweb"))))
    (dolist (sub-dir '("extension/dict" "extension/url-preview"))
      (add-to-list 'load-path (expand-file-name sub-dir dir) t)))

  (require 'popweb-url)

  (setq popweb-url-web-window-size-use-absolute t
        popweb-url-web-window-width-absolute 600
        popweb-url-web-window-height-absolute 480))

;; command-log
(use-package! command-log-mode
  :commands global-command-log-mode
  :init
  (setq command-log-mode-auto-show nil)

  (defvar command-window-frame nil)

  (defun bc/command-log-toggle-window ()
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
                     :position (point)
                     :poshandler #'posframe-poshandler-frame-top-left-or-right-other-corner
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

;; emacs-application-framework
(use-package! eaf
  :load-path (lambda () (list (expand-file-name "site-lisp/eaf" doom-user-dir)))
  :defer 2
  :init
  (setq +lookup-open-url-fn #'eaf-open-browser
        browse-url-browser-function #'eaf-open-browser
        eaf-start-python-process-when-require nil
        eaf-config-location (expand-file-name "eaf/" doom-cache-dir)
        eaf-proxy-type "http"
        eaf-proxy-host "127.0.0.1"
        eaf-proxy-port "20122"
        eaf-webengine-pc-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:101.0) Gecko/20100101 Firefox/101.0"
        eaf-webengine-font-family "Times New Roman"
        eaf-webengine-serif-font-family "Times New Roman"
        eaf-webengine-fixed-font-family "Courier New"
        eaf-browser-dark-mode nil
        eaf-browser-enable-adblocker t
        eaf-browser-enable-autofill t
        eaf-browser-aria2-proxy-host "127.0.0.1"
        eaf-browser-aria2-proxy-port "20122"
        eaf-browser-translate-language "zh-CN"
        eaf-browser-auto-import-chrome-cookies nil
        eaf-dired-advisor-enable nil)

  (if-let* ((bookmarks (cond ((featurep :system 'macos) "~/Library/Application Support/Google/Chrome/Default/Bookmarks")
                             ((featurep :system 'linux) (file-exists-p! (and (or "chromium/Default/Bookmarks"
                                                                                 "google-chrome/Default/Bookmarks"))
                                                                        "~/.config"))
                             (t nil))))
      (setq eaf-chrome-bookmark-file bookmarks))

  (if-let* ((history (cond ((featurep :system 'macos) "~/Library/Application Support/Google/Chrome/Default/History")
                           ((featurep :system 'linux) (file-exists-p! (and (or "chromium/Default/History"
                                                                               "google-chrome/Default/History"))
                                                                      "~/.config"))
                           (t nil))))
      (setq eaf-browser-chrome-history-file history))
  :config
  (require 'eaf-browser nil t)
  (require 'eaf-markdown-previewer nil t)
  (require 'eaf-markmap nil t)
  (require 'eaf-image-viewer nil t)
  (require 'eaf-org-previewer nil t)
  (require 'eaf-mind-elixir nil t)

  (defun eaf-translate-text (text)
    (cond ((featurep 'popweb-dict) (popweb-dict-bing-input text))
          ((modulep! :tools lookup +dictionary) (+lookup/dictionary-definition text))
          (t (message "Can't translate text"))))

  ;; ensure focus change function has been add
  (defadvice! bc/eaf-restart-process-after-advice ()
    :after #'eaf-restart-process
    (remove-function after-focus-change-function #'eaf--topmost-focus-change)
    (add-function :after after-focus-change-function #'eaf--topmost-focus-change))

  (when (modulep! :completion vertico +childframe)
    (after! vertico-posframe
      (defun bc/eaf-in-eaf-buffer-before-until-advice (&rest _args)
        (let ((has-eaf-buffer nil))
          (dolist (window (window-list))
            (if (eq (with-current-buffer (window-buffer window)
                      major-mode)  'eaf-mode)
                (setq has-eaf-buffer t)))
          has-eaf-buffer))
      (advice-add #'vertico-posframe--show :before-until #'bc/eaf-in-eaf-buffer-before-until-advice)
      (advice-add #'vertico-posframe--handle-minibuffer-window :before-until #'bc/eaf-in-eaf-buffer-before-until-advice))))

;; aidermacs
(use-package! aidermacs
  :defer t
  :commands aidermacs-run aidermacs-transient-menu
  :config
  (setq aidermacs-auto-commits nil
        aidermacs-default-model "openrouter/deepseek/deepseek-r1-0528:free"
        aidermacs-default-chat-mode 'architect
        aidermacs-architect-model "openrouter/anthropic/claude-sonnet-4"
        aidermacs-editor-model "openrouter/deepseek/deepseek-r1-0528"
        aidermacs-weak-model "openrouter/deepseek/deepseek-chat-v3-0324"
        aidermacs-extra-args '("--no-check-update" "--chat-language" "Chinese")
        aidermacs-watch-files t
        aidermacs-backend 'vterm
        aidermacs-comint-multiline-newline-key "S-<return>"
        aidermacs-vterm-multiline-newline-key "S-<return>")

  (add-hook! 'aidermacs-before-run-backend-hook
    (cl-dolist (pair '(("deepseek" . "api.deepseek.com")
                       ("openrouter" . "openrouter.ai")))
      (let* ((env-name (concat (upcase (car pair)) "_API_KEY")))
        (if-let* ((apikey (bc/lookup-password :host (cdr pair) :user "apikey")))
            (setenv env-name apikey))))))

;; gptel
(use-package! gptel
  :defer t
  :commands bc/start-gptel
  :config
  (setq gptel-default-mode 'markdown-mode
        gptel-model 'gpt-4o-mini
        gptel-backend (gptel-make-openai "ChatGPT"
                        :key 'gptel-api-key
                        :stream t
                        :host "openkey.cloud"
                        :models gptel--openai-models))

  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :size 0.3
    :quit nil
    :ttl nil)

  (add-hook! 'gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook! 'gptel-post-response-hook #'gptel-end-of-response)

  (defun bc/start-gptel ()
    (interactive)
    (gptel "*ChatGPT*" nil nil t)))

;; gptel-quick
(use-package! gptel-quick
  :defer t)

;; gptel-magit
(use-package! gptel-magit
  :when (modulep! :tools magit)
  :after gptel
  :config
  (add-hook! 'magit-mode-hook #'gptel-magit-install))
