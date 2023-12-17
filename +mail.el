;;; +mail.el -*- lexical-binding: t; -*-

(setenv "XAPIAN_CJK_NGRAM" "1")

(setq +mu4e-backend 'mbsync)

(set-email-account! "tcl.com"
                    '((mu4e-sent-folder       . "/tcl.com/Sent")
                      (mu4e-drafts-folder     . "/tcl.com/Drafts")
                      (mu4e-trash-folder      . "/tcl.com/Trash")
                      (mu4e-refile-folder     . "/tcl.com/Archive")
                      (mu4e-update-interval   . 600)
                      (user-full-name         . "Ben Chan")
                      (user-mail-address      . "ben.chen@tcl.com"))
                    t)

(set-email-account! "qq.com"
                    '((mu4e-sent-folder       . "/qq.com/Sent")
                      (mu4e-drafts-folder     . "/qq.com/Drafts")
                      (mu4e-trash-folder      . "/qq.com/Trash")
                      (mu4e-refile-folder     . "/qq.com/Archive")
                      (mu4e-update-interval   . 600)
                      (user-full-name         . "Ben Chan")
                      (user-mail-address      . "517926804@qq.com")))

(after! mu4e
  (define-key! [remap compose-mail] #'+mu4e/compose)

  (map! :map mu4e-headers-mode-map
        "l" #'+mu4e/capture-msg-to-agenda)

  (when (version<= "1.6" mu4e-mu-version)
    (map! :map mu4e-view-mode-map
          "A" #'+mu4e-view-select-mime-part-action
          "o" #'+mu4e-view-open-attachment))

  (setq mu4e-compose-dont-reply-to-self t
        mu4e-headers-time-format "%T"
        mu4e-headers-date-format "%D"
        mu4e-headers-long-date-format "%D %T"
        ;; mu4e-search-full t
        mu4e-search-results-limit 1000
        mu4e-attachment-dir "~/Downloads"
        mm-text-html-renderer 'gnus-w3m
        mu4e-view-open-program 'find-file)

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
                   ((string-match "ben.chen@tcl.com" from) "tcl")
                   ((string-match "517926804@qq.com" from) "qq"))))
              (setq message-sendmail-extra-arguments (list '"-a" account '"--read-envelope-from")))))))

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

  (when (modulep! :email mu4e +org)
    (defun +mu4e-set-signature-for-org-msg ()
      (setq org-msg-greeting-fmt
            "\n\n#+begin_signature\n--\n\nThanks and Best Regards\n\nBC（Ben Chan）\n#+end_signature\n"))

    (add-hook! 'mu4e-compose-pre-hook #'+mu4e-set-signature-for-org-msg))

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
    (mu4e-mark-execute-all))

  (defun mu4e--view-mime-part-to-temp-file (handle)
    "Write MIME-part HANDLE to a temporary file and return the file name.
The filename is deduced from the MIME-part's filename, or
otherwise random; the result is placed in a temporary directory
with a unique name. Returns the full path for the file created.
The directory and file are self-destructed."
    (let* ((tmpdir (make-temp-file "mu4e-temp-" t))
           (fname (mm-handle-filename handle))
           (fname (and fname
                       (gnus-map-function mm-file-name-rewrite-functions
                                          (file-name-nondirectory fname))))
           (fname (if fname
                      (concat tmpdir "/" (replace-regexp-in-string "/" "-" fname))
                    (let ((temporary-file-directory tmpdir))
                      (make-temp-file "mimepart")))))
      (unless (file-exists-p fname)
        (mm-save-part-to-file handle fname))
      ;; (run-at-time "30 sec" nil
      ;;              (lambda () (ignore-errors (delete-directory tmpdir t))))
      fname)))

(after! mu4e-alert
  (if IS-MAC
      (mu4e-alert-set-default-style 'notifier))

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (setq mu4e-alert-interesting-mail-query "flag:unread AND maildir:/Inbox/ AND NOT flag:trash"))

(when (modulep! :email mu4e +org)
  (after! org-msg
    (add-hook! 'org-msg-edit-mode-local-vars-hook (when (bound-and-true-p lsp-bridge-mode)
                                                    (lsp-bridge-mode -1)))))
