;;; +mail.el -*- lexical-binding: t; -*-

(setenv "XAPIAN_CJK_NGRAM" "1")

(setq +mu4e-backend 'mbsync)

(set-email-account! "fa-software.com"
                    '((mu4e-sent-folder       . "/fa-software.com/已发送")
                      (mu4e-drafts-folder     . "/fa-software.com/草稿")
                      (mu4e-trash-folder      . "/fa-software.com/已删除")
                      (mu4e-refile-folder     . "/fa-software.com/INBOX")
                      (mu4e-update-interval   . 600)
                      (smtpmail-smtp-user     . "xianbin.chen@fa-software.com")
                      (smtpmail-smtp-server   . "smtp.qiye.aliyun.com")
                      (smtpmail-smtp-service  . 465)
                      (smtpmail-stream-type   . ssl)
                      (user-full-name         . "Mike Chen")
                      (user-mail-address      . "xianbin.chen@fa-software.com"))
                    t)

(after! mu4e
  ;; load mu4e-contrib
  (require 'mu4e-contrib)

  (setq mu4e-headers-time-format "%T"
        mu4e-headers-date-format "%D"
        mu4e-headers-long-date-format "%D %T"
        ;; mu4e-search-full t
        mu4e-search-results-limit 1000)

  (map! :map mu4e-headers-mode-map
        "l" #'+mu4e/capture-msg-to-agenda)

  (when (version<= "1.6" mu4e-mu-version)
    (map! :map mu4e-view-mode-map
          "A" #'+mu4e-view-select-mime-part-action
          "o" #'+mu4e-view-open-attachment))
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
           :query "flag:unread AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?u)
          (:name "Today's messages"
           :query "date:today..now AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?t)
          (:name "Last week"
           :query "date:1w..now AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?w)
          (:name "Last month"
           :query "date:1m..now AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?m)
          (:name "Last season"
           :query "date:3m..now AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?s)
          (:name "Last half year"
           :query "date:6m..now AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?h)
          (:name "Last year"
           :query "date:1y..now AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?y)
          (:name "All in inbox"
           :query "maildir:/INBOX/ AND NOT flag:trash"
           :key ?a)
          (:name "Important"
           :query "prio:high AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?i)
          (:name "Mailing lists"
           :query "flag:list AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?l)
          (:name "With attachments"
           :query "flag:attach AND maildir:/INBOX/ AND NOT flag:trash"
           :key ?p))))

(after! mu4e-alert
  (if IS-MAC
      (mu4e-alert-set-default-style 'notifier))

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (setq mu4e-alert-interesting-mail-query "flag:unread AND maildir:/INBOX/ AND NOT flag:trash"))

(when (modulep! :email mu4e +org)
  (after! org-msg
    (setq org-msg-greeting-fmt "\n\n#+begin_signature\n--\nThanks and Best Regards\n陈显彬（Mike Chen）\n#+end_signature\n")
    (add-hook! 'org-msg-edit-mode-local-vars-hook (when (bound-and-true-p lsp-bridge-mode)
                                                    (lsp-bridge-mode -1))))

  (when (modulep! :lang web +lsp)
    (remove-hook! 'web-mode-local-vars-hook #'lsp!)

    (add-hook! 'web-mode-local-vars-hook (when (and (string-match-p "^org-msg" (buffer-name))
                                                    (not (bound-and-true-p org-msg-mode)))
                                           (lsp!)))))
