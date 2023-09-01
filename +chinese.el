;;; +chinese.el -*- lexical-binding: t; -*-

(use-package! cal-china-x
  :after calendar
  :commands cal-china-x-setup
  :init (cal-china-x-setup)
  :config
  (setq calendar-location-name "Chengdu")
  (setq calendar-latitude 30.67)
  (setq calendar-longitude 104.06)
  ;; Holidays
  (setq calendar-mark-holidays-flag t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays
        '((holiday-lunar 1 15 "元宵节")
          (holiday-lunar 7 7 "七夕节")
          (holiday-lunar 9 9 "重阳节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 12 "植树节")
          (holiday-fixed 5 4 "青年节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-fixed 9 10 "教师节")))
  (setq holiday-other-holidays
        '((holiday-fixed 2 14 "情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 12 25 "圣诞节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-float 11 4 4 "感恩节")
          ;; 农历节日
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))

;; rime
(use-package! rime
  :defer t
  :init
  (setq default-input-method "rime"
        rime-librime-root (if IS-MAC (expand-file-name "librime/dist/" doom-data-dir))
        rime-user-data-dir (expand-file-name "rime/" doom-data-dir)
        rime-show-candidate 'posframe
        rime-inline-ascii-trigger 'shift-l
        rime-disable-predicates '(rime-predicate-after-alphabet-char-p
                                  rime-predicate-prog-in-code-p
                                  rime-predicate-ace-window-p
                                  rime-predicate-hydra-p))
  (if (and IS-MAC
           (string-prefix-p "aarch64" system-configuration))
      (setq rime-emacs-module-header-root (concat "/opt/homebrew/opt/emacs-plus@" (number-to-string emacs-major-version) "/include")))
  :config
  (custom-set-faces!
    `(rime-default-face :foreground ,(doom-color 'modeline-fg) :background ,(doom-color 'modeline-bg)))

  (add-hook! 'doom-load-theme-hook :append
    (set-face-foreground 'rime-default-face (doom-color 'modeline-fg))
    (set-face-background 'rime-default-face (doom-color 'modeline-bg)))

  (add-hook! '(after-init-hook kill-emacs-hook) :append
    (when (fboundp 'rime-lib-sync-user-data)
      (ignore-errors (rime-sync))))

  (add-hook! 'scala-mode-hook :append
    (add-hook! 'post-command-hook :local
      (if (fboundp 'rime--redisplay)
          (rime--redisplay))))

  (when IS-LINUX
    (defadvice! +rime--posframe-display-content-filter-a (args)
      "给 `rime--posframe-display-content' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
      :filter-args #'rime--posframe-display-content
      (cl-destructuring-bind (content) args
        (let ((newresult (if (string-blank-p content)
                             content
                           (concat content "　"))))
          (list newresult)))))

  (map! (:map rime-mode-map
         (:when IS-MAC
          "s-j" #'rime-force-enable)
         (:unless IS-MAC
          "C-j" #'rime-force-enable)
         "C-`" #'rime-send-keybinding)))

;;; Hacks
(defadvice! +chinese--org-html-paragraph-a (args)
  "Join consecutive Chinese lines into a single long line without unwanted space
when exporting org-mode to html."
  :filter-args #'org-html-paragraph
  (++chinese--org-paragraph args))

(defadvice! +chinese--org-hugo-paragraph-a (args)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
  :filter-args #'org-hugo-paragraph
  (++chinese--org-paragraph args))

(defun ++chinese--org-paragraph (args)
  (cl-destructuring-bind (paragraph contents info) args
    (let* ((fix-regexp "[[:multibyte:]]")
           (origin-contents
            (replace-regexp-in-string
             "<[Bb][Rr] */>"
             ""
             contents))
           (fixed-contents
            (replace-regexp-in-string
             (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
             "\\1\\2"
             origin-contents)))
      (list paragraph fixed-contents info))))

;; google-translate
(use-package! go-translate
  :defer t
  :commands gts-do-translate-prompt
  :init
  (map! :leader
        (:prefix-map ("y" . "translate")
         :desc "Google translate" "g" #'gts-do-translate
         :desc "Google translate prompt" "G" #'gts-do-translate-prompt))
  (add-hook! 'doom-load-theme-hook :append
    (setq gts-pop-posframe-backcolor (face-background 'mode-line)
          gts-pop-posframe-forecolor (face-foreground 'mode-line)))
  :config
  (set-popup-rule! "^\\*Go-Translate\\*" :side 'right :size 0.4 :select t)
  (setq gts-translate-list '(("en" "zh"))
        gts-buffer-follow-p t
        gts-posframe-pop-render-timeout nil
        gts-pop-posframe-backcolor (face-background 'mode-line)
        gts-pop-posframe-forecolor (face-foreground 'mode-line))

  (setq gts-default-translator
        (gts-translator
         :picker (gts-noprompt-picker :texter (gts-current-or-selection-texter))
         :engines (list (gts-google-rpc-engine :parser (gts-google-rpc-parser)))
         :render (gts-posframe-pop-render)
         :splitter (gts-paragraph-splitter)))

  (defun gts-do-translate-prompt ()
    "Do the translation of this prompt"
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-prompt-picker)
                    :engines (list (gts-google-rpc-engine :parser (gts-google-rpc-parser)))
                    :render (gts-posframe-pop-render)
                    :splitter (gts-paragraph-splitter)))))

;; insert-translated-name
(use-package! insert-translated-name
  :config
  (setq insert-translated-name-default-style 'origin)

  (defvar insert-translated-name-is-open-ws-butler nil)
  (defadvice! insert-translated-name-retrieve-translation-a (&rest _)
    :before #'insert-translated-name-retrieve-translation
    (setq insert-translated-name-is-open-ws-butler ws-butler-mode)
    (when insert-translated-name-is-open-ws-butler
      (ws-butler-mode -1)))

  (defadvice! insert-translated-name-update-translation-in-buffer-a (&rest _)
    :after #'insert-translated-name-update-translation-in-buffer
    (when insert-translated-name-is-open-ws-butler
      (ws-butler-mode +1))
    (setq insert-translated-name-is-open-ws-butler nil))

  (map! :leader
        (:prefix-map ("y" . "translate")
         :desc "Insert translated name" "i" #'insert-translated-name-insert)))

;; dictionary-overlay
(use-package! websocket-bridge)
(use-package! dictionary-overlay
  :defer 2
  :after websocket-bridge
  :init
  (setq dictionary-overlay-translators '("local" "darwin" "sdcv" "web")
        dictionary-overlay-user-data-directory (expand-file-name "dictionary-overlay-data" doom-data-dir))
  :config
  (dictionary-overlay-start))

;; popweb-dict
(use-package! popweb-dict
  :after popweb
  :config
  (setq popweb-dict-say-word-p nil)
  (map! :leader
        (:prefix-map ("y" . "translate")
         :desc "Bing translate"         "b" #'popweb-dict-bing-pointer
         :desc "Bing translate input"   "B" #'popweb-dict-bing-input
         :desc "Youdao translate"       "y" #'popweb-dict-youdao-pointer
         :desc "Youdao translate input" "Y" #'popweb-dict-youdao-input
         :desc "Play voice"             "p" #'popweb-dict-say-word))

  (defadvice! popweb-dict-region-or-word-a ()
    :override #'popweb-dict-region-or-word
    (cond ((derived-mode-p 'pdf-view-mode) (car (pdf-view-active-region-text)))
          ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
          (t (thing-at-point 'word t)))))
