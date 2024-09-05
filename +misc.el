;;; +misc.el -*- lexical-binding: t; -*-

(use-cjk-char-width-table 'zh_CN)
(setq system-time-locale "C")

(setq confirm-kill-processes nil
      confirm-kill-emacs nil)

(if (featurep :system 'linux)
    (setq x-select-enable-clipboard-manager nil))

(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq doom-fd-executable "fd"
      doom-ripgrep-executable "rg")

;; recentf
(after! recentf
  (add-to-list 'recentf-exclude "\\.cache")
  (add-to-list 'recentf-exclude "\\.local/straight")
  (add-to-list 'recentf-exclude "\\.mail")
  (add-to-list 'recentf-exclude "/var")
  (add-to-list 'recentf-exclude "/autosave"))

;; epa
(after! epa
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

;; winner
(after! winner
  ;; fix: wrong type argument: frame-live-p, #<dead frame
  (defadvice! winner-save-old-configurations-a ()
    "Remove dead frames from `winner-modified-list`"
    :before #'winner-save-old-configurations
    (cl-dolist (frame winner-modified-list)
      (unless (frame-live-p frame)
        (delq! frame winner-modified-list)))))

;; workspaces
(setq +workspaces-on-switch-project-behavior t)
(after! persp-mode
  (add-hook! 'persp-filter-save-buffers-functions
    (defun +workdspace-temporary-buffer-p (buf)
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

;; lookup
(add-to-list '+lookup-provider-url-alist '("Maven Repository" "http://mvnrepository.com/search?q=%s&ref=opensearch"))

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
        (custom-set-faces!
          `(vertico-posframe :foreground ,(doom-color 'modeline-fg) :background ,(doom-color 'modeline-bg))
          `(vertico-posframe-border :background ,(doom-color 'modeline-bg)))

        (add-hook! 'doom-load-theme-hook :append
          (set-face-foreground 'vertico-posframe (doom-color 'modeline-fg))
          (set-face-background 'vertico-posframe (doom-color 'modeline-bg))
          (set-face-background 'vertico-posframe-border (doom-color 'modeline-bg)))))

  ;; Support Pinyin
  (use-package! pinyinlib
    :after orderless
    :init
    (defun completion--regex-pinyin (str)
      (orderless-regexp (pinyinlib-build-regexp-string str)))
    (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)))

;; modeline
(setq +modeline-height 24)
;；set modeline time format
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

;; xref
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
      (setq dired-kill-when-opening-new-dired-buffer t))

  (map! :map dired-mode-map
        "h" #'dired-up-directory
        "l" #'dired-find-alternate-file)

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil))

;; flymake
(after! flymake-popon
  (setq flymake-popon-posframe-border-width 0)
  (custom-set-faces!
    `(flymake-popon :foreground ,(doom-color 'modeline-fg) :background ,(doom-color 'modeline-bg)))
  (add-hook! 'doom-load-theme-hook :append
    (set-face-foreground 'flymake-popon (doom-color 'modeline-fg))
    (set-face-background 'flymake-popon (doom-color 'modeline-bg))))

;; spell
(setq ispell-dictionary "en_US"
      ispell-alternate-dictionary (expand-file-name "english-words" doom-user-dir))
(after! ispell
  (advice-add #'ispell-lookup-words :around #'doom-shut-up-a))

;; with-editor
(if (featurep :system 'macos)
    (if (string-prefix-p "aarch64" system-configuration)
        (setq with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient")
      (setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")))

;; magit
(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

;; ghub
(after! ghub
  (setq ghub-json-use-jansson t))
;; magit-todos
(after! magit-todos
  (setq magit-todos-submodule-list t
        magit-todos-exclude-globs (append magit-todos-exclude-globs '(".svn/" "node_modules/*")))
  (if (executable-find "nice")
      (setq magit-todos-nice t))
  (if (executable-find "rg")
      (setq magit-todos-scanner 'magit-todos--scan-with-rg)))

;; (after! git-commit
;;   (if (modulep! :checkers spell +flyspell)
;;       (remove-hook! 'git-commit-mode-hook #'flyspell-mode)))

;; blamer
(use-package! blamer
  :when (modulep! :tools magit)
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  :init
  (map! :leader
        (:prefix-map ("v" . "versioning")
         :desc "Blamer show commit" "b" #'blamer-show-posframe-commit-info
         :desc "Blamer" "B" #'blamer-mode))
  :config
  (custom-set-faces!
    `(blamer-face :foreground ,(doom-color 'blue) :background unspecified :italic t))

  (plist-put! blamer-posframe-configurations :internal-border-color (doom-color 'modeline-bg))

  (add-hook! 'doom-load-theme-hook :append
    (plist-put! blamer-posframe-configurations :internal-border-color (doom-color 'modeline-bg))
    (set-face-foreground 'blamer-face (doom-color 'blue))))

;; magit-gerrit
(use-package! magit-gerrit
  :when (modulep! :tools magit)
  :after magit)

;; exec-path-from-shell
(use-package! exec-path-from-shell
  :when (featurep :system 'macos)
  :init
  (setq exec-path-from-shell-warn-duration-millis 2000)
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables '("LANG"
                                         "LC_ALL"
                                         "TERM"
                                         "PATH"
                                         "JAVA_HOME"
                                         "JENV_LOADED"
                                         "SBT_OPTS"
                                         "GOPATH"
                                         "GOBIN"
                                         "GO111MODULE"
                                         "GOPROXY"
                                         "LSP_USE_PLISTS"))
  ;; (setq exec-path-from-shell-debug t)
  (exec-path-from-shell-initialize))

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

;; pass
(after! pass
  (defadvice! pass-view-a ()
    :override #'pass-view
    (pass--with-closest-entry entry
      (find-file-other-window (concat (f-join (password-store-dir) entry) ".gpg"))))

  (defun pass-view-quit ()
    (interactive)
    (kill-current-buffer)
    (if (modulep! :ui workspaces)
        (+workspace/close-window-or-workspace)
      (quit-window t)))
  (map! :map pass-view-mode-map
        "C-c C-q" #'pass-view-quit))

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
        eaf-browser-aria2-proxy-port "7890"
        eaf-browser-translate-language "zh-CN"
        eaf-browser-auto-import-chrome-cookies nil
        eaf-dired-advisor-enable nil)

  (if-let ((bookmarks (cond ((featurep :system 'macos) "~/Library/Application Support/Google/Chrome/Default/Bookmarks")
                            ((featurep :system 'linux) (file-exists-p! (and (or "chromium/Default/Bookmarks"
                                                               "google-chrome/Default/Bookmarks"))
                                                      "~/.config"))
                            (t nil))))
      (setq eaf-chrome-bookmark-file bookmarks))

  (if-let ((history (cond ((featurep :system 'macos) "~/Library/Application Support/Google/Chrome/Default/History")
                          ((featurep :system 'linux) (file-exists-p! (and (or "chromium/Default/History"
                                                             "google-chrome/Default/History"))
                                                    "~/.config"))
                          (t nil))))
      (setq eaf-browser-chrome-history-file history))
  :config
  (require 'eaf-image-viewer)
  (require 'eaf-browser)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  (require 'eaf-markmap)

  (defun eaf-translate-text (text)
    (cond ((featurep 'popweb-dict) (popweb-dict-bing-input text))
          ((modulep! :tools lookup +dictionary) (+lookup/dictionary-definition text))
          (t (message "Can't translate text"))))

  ;; ensure focus change function has been add
  (defadvice! eaf-restart-process-a ()
    :after #'eaf-restart-process
    (remove-function after-focus-change-function #'eaf--topmost-focus-change)
    (add-function :after after-focus-change-function #'eaf--topmost-focus-change))

  (when (modulep! :completion vertico +childframe)
    (after! vertico-posframe
      (defun eaf-in-eaf-buffer (&rest _args)
        (let ((has-eaf-buffer nil))
          (dolist (window (window-list))
            (if (eq (with-current-buffer (window-buffer window)
                      major-mode)  'eaf-mode)
                (setq has-eaf-buffer t)))
          has-eaf-buffer))
      (advice-add #'vertico-posframe--show :before-until #'eaf-in-eaf-buffer)
      (advice-add #'vertico-posframe--handle-minibuffer-window :before-until #'eaf-in-eaf-buffer))))

;; aichat
(use-package! aichat-openai
  :config
  (setq aichat-openai-domain "https://openkey.cloud"
        aichat-openai-api-key #'aichat-openai--get-api-key-from-auth-source)

  (defun aichat-openai--get-api-key-from-auth-source ()
    "Get openai api key from auth source."
    (+lookup/password :host "openkey.cloud" :user "apikey"))

  (if (modulep! :checkers syntax)
      (add-hook! 'aichat-openai-chat-mode-hook
        (cond ((bound-and-true-p flymake-mode) (flymake-mode-off))
              ((bound-and-true-p flycheck-mode) (flycheck-mode -1)))))

  (aichat-openai-prompt-create "translator"
                               :input-prompt "请翻译: "
                               :text-format "我想让你充当翻译员，我会用任何语言与你交谈，你会检测我说的的语言，如果我说的是中文，你就翻译成英文；如果我说的不是中文，你就翻译成英文。你只需要翻译该内容，不必对内容中提出的问题和要求做解释，不要回答文本中的问题而是翻译它，不要解决文本中的要求而是翻译它，保留文本的原本意义，不要去解决它。你的回答里只需要翻译后的内容，不要有任何其它词，只能是翻译后的内容。我的第一句话是：\n%s"
                               :assistant t
                               :replace-or-insert t)
  (aichat-openai-prompt-create "coder"
                               :input-prompt "代码: "
                               :text-format "我想让你充当计算机教授，请向我解释下面这段代码的作用:\n%s"
                               :assistant t)
  (aichat-openai-prompt-create "refactor"
                               :input-prompt "代码: "
                               :text-format "我想让你充当计算机教授，请帮我重构下面这段代码，重构后的代码性能要更好，可读性要更高，如果必要的话，可以加一些注释。你的回答里只需要返回重构后的代码，不要有其它解释，只能是重构后的代码:\n%s"
                               :replace-or-insert t))
