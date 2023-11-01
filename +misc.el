;;; +misc.el -*- lexical-binding: t; -*-

(use-cjk-char-width-table 'zh_CN)
(setq system-time-locale "zh_CN")

(setq confirm-kill-processes nil
      confirm-kill-emacs nil)

(if IS-LINUX
    (setq x-select-enable-clipboard-manager nil))

(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; recentf
(after! recentf
  (add-to-list 'recentf-exclude "\\.mail")
  (add-to-list 'recentf-exclude "/var")
  (add-to-list 'recentf-exclude "/autosave"))

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

;; lookup
(appendq! +lookup-provider-url-alist
          '(("Maven Repository" "http://mvnrepository.com/search?q=%s&ref=opensearch")))

;; projectile
(after! projectile
  (setq projectile-project-search-path
        '(("~/Projects/emacs" . 1)
          ("~/Projects/fa/sources" . 1)
          ("~/Projects/golang" . 1)
          ("~/Projects/java" . 1)
          ("~/Projects/scala" . 1)))

  (dolist (suffix '(".bak" ".exe"))
    (add-to-list 'projectile-globally-ignored-file-suffixes suffix)))

;; vertico
(if (modulep! :completion vertico +childframe)
    (after! vertico-posframe
      (custom-set-faces!
        `(vertico-posframe :foreground ,(doom-color 'modeline-fg) :background ,(doom-color 'modeline-bg))
        `(vertico-posframe-border :background ,(doom-color 'modeline-bg)))

      (add-hook! 'doom-load-theme-hook :append
        (set-face-foreground 'vertico-posframe (doom-color 'modeline-fg))
        (set-face-background 'vertico-posframe (doom-color 'modeline-bg))
        (set-face-background 'vertico-posframe-border (doom-color 'modeline-bg)))))

;; modeline
(setq +modeline-height 24)
;；set modeline time format
(setq display-time-format "%D %R")

;; rainbow
(after! rainbow-mode
  (add-hook! 'rainbow-mode-hook
    (hl-line-mode (if rainbow-mode -1 +1))))

;; lookup
(when (and IS-MAC (featurep 'xwidget-internal))
  (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn
        browse-url-browser-function #'+lookup-xwidget-webkit-open-url-fn)
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
      (setq dired-kill-when-opening-new-dired-buffer t))

  (map! :map dired-mode-map
        "h" #'dired-up-directory
        "l" #'dired-find-alternate-file)

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil))

;; spell
(setq ispell-dictionary "en_US"
      ispell-alternate-dictionary (expand-file-name "english-words" doom-user-dir))
(after! ispell
  (advice-add #'ispell-lookup-words :around #'doom-shut-up-a))

;; with-editor
;; (setq with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient")

;; magit
(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

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

(when (modulep! :tools magit)
  ;; blamer
  (use-package! blamer
    :defer 5
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
  (use-package! magit-gerrit))

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
        popweb-proxy-port "7890")

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
        eaf-proxy-port "7890"
        eaf-webengine-pc-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:101.0) Gecko/20100101 Firefox/101.0"
        eaf-webengine-font-family "Times New Roman"
        eaf-webengine-serif-font-family "Times New Roman"
        eaf-webengine-fixed-font-family "Courier New"
        eaf-browser-dark-mode nil
        eaf-browser-enable-adblocker t
        eaf-browser-enable-autofill t
        eaf-browser-aria2-proxy-host "127.0.0.1"
        eaf-browser-aria2-proxy-port "7890"
        eaf-browser-auto-import-chrome-cookies nil)

  (if-let ((bookmarks (cond (IS-MAC "~/Library/Application Support/Google/Chrome/Default/Bookmarks")
                            (IS-LINUX (file-exists-p! (and (or "chromium/Default/Bookmarks"
                                                               "google-chrome/Default/Bookmarks"))
                                                      "~/.config"))
                            (t nil))))
      (setq eaf-chrome-bookmark-file bookmarks))

  (if-let ((history (cond (IS-MAC "~/Library/Application Support/Google/Chrome/Default/History")
                          (IS-LINUX (file-exists-p! (and (or "chromium/Default/History"
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

  (when (modulep! :emacs dired)
    (advice-remove #'dired-find-file #'eaf--dired-find-file-advisor)
    (advice-remove #'dired-find-alternate-file #'eaf--dired-find-file-advisor))

  (defvar eaf-office-preview-list nil)

  (defadvice! eaf-open-office-a (file)
    :override #'eaf-open-office
    (if-let* ((command (or (executable-find "libreoffice")
                           (executable-find "soffice")))
              (file-md5 (eaf-get-file-md5 file))
              (basename (file-name-base file))
              (pdf-file (format "%s/%s.pdf" temporary-file-directory file-md5))
              (pdf-argument (format "%s.%s_office_pdf" basename (file-name-extension file))))
        (if (file-exists-p pdf-file)
            (progn
              (pushnew! eaf-office-preview-list (cons pdf-file file))
              (eaf-open pdf-file "pdf-viewer" pdf-argument))
          (message "Converting %s to PDF, EAF will start shortly..." file)
          (make-process
           :name ""
           :buffer " *eaf-open-office*"
           :command (list command "--headless" "--convert-to" "pdf" (file-truename file) "--outdir" temporary-file-directory)
           :sentinel (lambda (_ event)
                       (when (string= (substring event 0 -1) "finished")
                         (rename-file (format "%s/%s.pdf" temporary-file-directory basename) pdf-file)
                         (pushnew! eaf-office-preview-list (cons pdf-file file))
                         (eaf-open pdf-file "pdf-viewer" pdf-argument)))))
      (error "[EAF/office] libreoffice is required convert Office file to PDF!")))

  (defadvice! eaf-open-external-a ()
    :override #'eaf-open-external
    (let* ((path-or-url (eaf-get-path-or-url))
           (office-path-or-url (cdr (assoc path-or-url eaf-office-preview-list)))
           (final-path-or-url (if office-path-or-url
                                  (expand-file-name office-path-or-url)
                                path-or-url)))
      (cond ((memq system-type '(cygwin windows-nt ms-dos))
             (w32-shell-execute "open" final-path-or-url))
            ((eq system-type 'darwin)
             (shell-command (concat "open " (shell-quote-argument final-path-or-url))))
            ((eq system-type 'gnu/linux)
             (let ((process-connection-type nil))
               (start-process "" nil "xdg-open" final-path-or-url))))))

  (defadvice! eaf-py-proxy-close_buffer-a ()
    :before #'eaf-py-proxy-close_buffer
    (if-let* ((file (eaf-get-path-or-url)))
        (delq! file eaf-office-preview-list 'assoc)))

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

;; mind-wave
(use-package! mind-wave
  :config
  (setq mind-wave-lang "zh_CN"
        mind-wave-api-key-path (expand-file-name "mind-wave/chatgpt_api_key.txt" doom-data-dir)
        mind-wave-api-base "https://openkey.cloud/v1"
        mind-wave-chat-model "gpt-3.5-turbo-16k-0613"
        mind-wave-async-text-model "gpt-3.5-turbo-0613"
        mind-wave-action-code-model "gpt-3.5-turbo-0613"
        mind-wave-explain-word-model "gpt-3.5-turbo-0613"
        mind-wave-parse-title-model "gpt-3.5-turbo-0613"
        mind-wave-git-commit-model "gpt-3.5-turbo-0613"))
