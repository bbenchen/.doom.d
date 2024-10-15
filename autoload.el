;;; autoload.el -*- lexical-binding: t; -*-

;; https://emacs-china.org/t/topic/2119
;;;###autoload
(defun bc/diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  "在org文件中使用下面的方式来调用该函数
%%(bc/diary-chinese-anniversary 9 23 1993) 这是农历 1993 年 9 月 23 日生人的第 %d%s 个生日"
  (if year
      (let* ((d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))

;;;###autoload
(defun bc/lookup-password (&rest keys)
  (if-let ((result (apply #'auth-source-search keys)))
    (auth-info-password (car result))))

;;;###autoload
(defun bc/pinentry-emacs (desc prompt _ok _error)
  "Read gnupg password"
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

;;;###autoload
(defun bc/find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun bc/browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun bc/update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/.dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating dotfiles...done"))
      (message "\"%s\" doesn't exist" dir))))

;;;###autoload
(defun bc/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;;;###autoload
(defun bc/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun bc/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;;###autoload
(defun bc/toggle-frame-transparency (&optional frame)
  "Toggle between transparent and opaque state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (pcase (frame-parameter frame 'alpha-background)
    (50 (modify-frame-parameters frame '((alpha-background . 100))))
    (_ (modify-frame-parameters frame '((alpha-background . 50))))))

;; Network Proxy
;;;###autoload
(defun bc/show-proxy-http ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (message "Current HTTP proxy is `127.0.0.1:20122'")
    (message "No HTTP proxy")))

;;;###autoload
(defun bc/enable-proxy-http ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . "127.0.0.1:20122")
          ("https" . "127.0.0.1:20122")
          ("no_proxy" . "^\\(localhost\\|127.0.0.1\\|192.168.*\\|172.16.*\\|10.0.*\\)")))
  (bc/show-proxy-http))

;;;###autoload
(defun bc/disable-proxy-http ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (bc/show-proxy-http))

;;;###autoload
(defun bc/toggle-proxy-http ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (bc/disable-proxy-http)
    (bc/enable-proxy-http)))
