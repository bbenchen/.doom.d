;;; +mail.el -*- lexical-binding: t; -*-

(setenv "XAPIAN_CJK_NGRAM" "1")

(set-email-account! "qq.com"
                    '((mu4e-sent-folder       . "/qq.com/Sent")
                      (mu4e-drafts-folder     . "/qq.com/Drafts")
                      (mu4e-trash-folder      . "/qq.com/Trash")
                      (mu4e-refile-folder     . "/qq.com/Archive")
                      (mu4e-update-interval   . 600)
                      (user-full-name         . "Ben Chen")
                      (user-mail-address      . "517926804@qq.com"))
                    t)

(after! mu4e
  (define-key! [remap compose-mail] #'+mu4e/compose)

  (map! :map mu4e-headers-mode-map
        "l" #'+mu4e/capture-msg-to-agenda)

  (when (version<= "1.6" mu4e-mu-version)
    (map! :map mu4e-view-mode-map
          "A" #'+mu4e-view-select-mime-part-action
          "o" #'+mu4e-view-open-attachment))

  (setq mu4e-modeline-support t
        mu4e-notification-support t
        message-dont-reply-to-names t
        mu4e-headers-time-format "%T"
        mu4e-headers-date-format "%D"
        mu4e-headers-long-date-format "%D %T"
        ;; mu4e-search-full t
        mu4e-search-results-limit 1000
        mu4e-attachment-dir "~/Downloads"
        mm-text-html-renderer 'gnus-w3m)

  (if (modulep! :email mu4e +mbsync)
      (setq mu4e-get-mail-command "mbsync --all"))

  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-send-mail-function #'message-send-mail-with-sendmail)

  (add-hook! 'message-send-mail-hook
    (defun +mu4e-set-msmtp-account ()
      (if (message-mail-p)
          (save-excursion
            (let*
                ((from (save-restriction
                         (message-narrow-to-headers)
                         (message-fetch-field "from")))
                 (account
                  (cond
                   ((string-match "517926804@qq.com" from) "qq"))))
              (setq message-sendmail-extra-arguments (list '"-a" account '"--read-envelope-from")))))))

  (unless (functionp 'mu4e--view-gather-mime-parts)
      (defun mu4e--view-gather-mime-parts ()
        "Gather all MIME parts as an alist.
The alist uniquely maps the number to the gnus-part."
        (let ((parts '()))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let ((part (get-text-property (point) 'gnus-data))
                    (index (get-text-property (point) 'gnus-part)))
                (when (and part (numberp index) (not (assoc index parts)))
                  (push `(,index . ,part) parts))
                (goto-char (or (next-single-property-change (point) 'gnus-part)
                               (point-max))))))
          parts)))

  ;;
  ;; Xapian, the search engine of mu has a poor support of CJK characters,
  ;; which causes only query contains no more than 2 CJK characters works.
  ;;
  ;; https://researchmap.jp/?page_id=457
  ;;
  ;; This workaroud breaks any CJK words longer than 2 characters into
  ;; combines of bi-grams. Example: 我爱你 -> (我爱 爱你)
  ;;
  (defun mu4e~break-cjk-word (word)
    "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
    (if (or (<= (length word) 2)
            (equal (length word) (string-bytes word))) ; only ascii chars
        word
      (let ((pos nil)
            (char-list nil)
            (br-word nil))
        (if (setq pos (string-match ":" word))     ; like: "s:abc"
            (concat (substring word 0 (+ 1 pos))
                    (mu4e~break-cjk-word (substring word (+ 1 pos))))
          (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abcあいう
              word
            (progn
              (setq char-list (split-string word "" t))
              (while (cdr char-list)
                (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
                (setq char-list (cdr char-list)))
              br-word))))))

  (defun mu4e~break-cjk-query (expr)
    "Break CJK strings into bi-grams in query."
    (let ((word-list (split-string expr " " t))
          (new ""))
      (dolist (word word-list new)
        (setq new (concat new (mu4e~break-cjk-word word) " ")))))

  (setq mu4e-query-rewrite-function #'mu4e~break-cjk-query)

  (setq mu4e-bookmarks
        `((:name  "Unread messages"
           :query "flag:unread AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?u)
          (:name "Today's messages"
           :query "date:today..now AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?t)
          (:name "Last week"
           :query "date:1w..now AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?w)
          (:name "Last month"
           :query "date:1m..now AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?m)
          (:name "Last season"
           :query "date:3m..now AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?s)
          (:name "Last half year"
           :query "date:6m..now AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?h)
          (:name "Last year"
           :query "date:1y..now AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?y)
          (:name "All in inbox"
           :query "maildir:/Inbox/ AND NOT flag:trash"
           :key ?a)
          (:name "Important"
           :query "prio:high AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?i)
          (:name "Mailing lists"
           :query "flag:list AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?l)
          (:name "With attachments"
           :query "flag:attach AND maildir:/Inbox/ AND NOT flag:trash"
           :key ?p)))

  (defun +mu4e/open-mail-as-html ()
    "Open the HTML mail in Browser."
    (interactive)
    (if-let ((msg (mu4e-message-at-point t)))
        (mu4e-action-view-in-browser msg)
      (user-error "No message at point")))

  (add-hook! 'mu4e-compose-mode-hook
    (defun +mu4e-add-cc--header ()
      (save-excursion (message-add-header "Cc: \n"))))

  (defun mu4e-headers-mark-all-unread-read ()
    "Put a ! \(read) mark on all visible unread messages."
    (interactive)
    (mu4e-headers-mark-for-each-if
     (cons 'read nil)
     (lambda (msg _param)
       (memq 'unread (mu4e-msg-field msg :flags)))))

  (defun mu4e-headers-flag-all-read ()
    "Flag all visible messages as \"read\"."
    (interactive)
    (mu4e-headers-mark-all-unread-read)
    (mu4e-mark-execute-all t))

  (defun mu4e-headers-mark-all ()
    "Mark all headers for some action.
Ask user what action to execute."
    (interactive)
    (mu4e-headers-mark-for-each-if
     (cons 'something nil)
     (lambda (_msg _param) t))
    (mu4e-mark-execute-all)))

(when (modulep! :email mu4e +org)
  (after! org-msg
    (add-hook! 'org-msg-edit-mode-local-vars-hook (when (bound-and-true-p lsp-bridge-mode)
                                                    (lsp-bridge-mode -1)))))
