;;; +markdown.el -*- lexical-binding: t; -*-

;; markdown-mode
(after! markdown-mode
  (add-hook! 'markdown-mode-hook
    (defun flycheck-enable-markdownlint ()
      "Set the `mardkownlint' config file for the current buffer."
      (let* ((md-lint ".markdownlint.json")
             (md-file buffer-file-name)
             (md-lint-dir (and md-file
                               (locate-dominating-file md-file md-lint))))
        (setq-local flycheck-markdown-markdownlint-cli-config
                    (expand-file-name md-lint md-lint-dir))))))

(after! grip-mode
  (if (featurep 'xwidget-internal)
      ;; Use embedded webkit to previe
      (setq grip-preview-use-webkit t))
  ;; Setup xwidget window popup rule
  (set-popup-rule! "*xwidget" :side 'right :size .50 :select nil :quit t)
  ;; Setup github username and token for API auth
  (after! auth-source
    (let ((credentials (auth-source-user-and-password "mygrip")))
      (setq grip-github-user (car credentials)
            grip-github-password (cadr credentials)))))
