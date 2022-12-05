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
  (setq calendar-mark-holidays-flag nil)
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
  :config
  (custom-set-faces!
    `(rime-default-face :foreground ,(doom-color 'modeline-fg) :background ,(doom-color 'modeline-bg)))

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
(use-package! google-translate
  :defer t
  :commands (google-translate-chinese-at-point++
             google-translate-chinese-at-point)
  :init
  (setq google-translate-base-url "https://translate.google.com/translate_a/single"
        google-translate-listen-url "https://translate.google.com/translate_tts"
        ;; google-translate-backend-method 'curl
        google-translate-default-source-language "en"
        google-translate-default-target-language "zh-CN")

  (map! :leader
        (:prefix-map ("y" . "translate")
         "g" #'google-translate-chinese-at-point++
         "G" #'google-translate-chinese-at-point))
  :config
  (setq url-automatic-caching t
        google-translate-listen-program (executable-find "mpv"))

  ;; (defun google-translate--get-b-d1 ()
  ;;   ;; TKK='427110.1469889687'
  ;;   (list 427110 1477889687))

  (set-popup-rule! "^\\*Google Translate\\*" :side 'right :size 0.4 :select t)

  (defcustom google-translate-tooltip-name "*google-translate-posframe*"
    "The name of google translate tooltip name."
    :type 'string
    :group 'google-translate)

  (defvar google-translate-tooltip-last-point 0
    "Hold last point when show tooltip, use for hide tooltip after move point.")

  (defvar google-translate-tooltip-last-scroll-offset 0
    "Hold last scroll offset when show tooltip, use for hide tooltip after window scroll.")

  (defun google-translate-hide-tooltip-after-move ()
    (ignore-errors
      (when (get-buffer google-translate-tooltip-name)
        (unless (and
                 (equal (point) google-translate-tooltip-last-point)
                 (equal (window-start) google-translate-tooltip-last-scroll-offset))
          (posframe-delete google-translate-tooltip-name)
          (kill-buffer google-translate-tooltip-name)))))

  (defun google-translate-show-posframe-tooltip (text)
    "Show string on posframe buffer."
    ;; Show tooltip at point if word fetch from user cursor.
    (unless (and (require 'posframe nil t) (posframe-workable-p))
      (error "Posframe not workable"))
    (posframe-show google-translate-tooltip-name
                   :string text
                   :position (point)
                   :timeout 30
                   :background-color (face-background 'mode-line)
                   :foreground-color (face-foreground 'mode-line)
                   :internal-border-width 10)
    (add-hook 'post-command-hook 'google-translate-hide-tooltip-after-move)
    (setq google-translate-tooltip-last-point (point))
    (setq google-translate-tooltip-last-scroll-offset (window-start)))

  (defun -region-or-word ()
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'word t)))

  (defun -chinese-word-p (word)
    (if (and word (string-match "\\cc" word)) t nil))

  (defun -translate-request (source-language target-language text)
    (let* ((json (google-translate-request source-language
                                           target-language
                                           text)))
      (if (null json)
          (message "Nothing to translate.")
        (let* ((detailed-translation
                (google-translate-json-detailed-translation json))
               (detailed-definition
                (google-translate-json-detailed-definition json))
               (gtos
                (make-gtos
                 :source-language source-language
                 :target-language target-language
                 :auto-detected-language (aref json 2)
                 :text text
                 :text-phonetic (google-translate-json-text-phonetic json)
                 :translation (google-translate-json-translation json)
                 :translation-phonetic (google-translate-json-translation-phonetic json)
                 :detailed-translation detailed-translation
                 :detailed-definition detailed-definition
                 :suggestion (when (null detailed-translation)
                               (google-translate-json-suggestion json)))))
          (google-translate-posframe-output-translation gtos)))))

  (defun google-translate-posframe-output-translation (gtos)
    "Output translation to the popup tooltip using `popup' package."
    (google-translate-show-posframe-tooltip
     (with-temp-buffer
       (google-translate-buffer-insert-translation gtos)
       (google-translate--trim-string
        (buffer-substring (point-min) (point-max))))))

  (defun %google-translate-at-point++ (override-p reverse-p)
    (let* ((langs (google-translate-read-args override-p reverse-p))
           (source-language (car langs))
           (target-language (cadr langs))
           (bounds nil))
      (-translate-request
       source-language target-language
       (if (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end))
         (or (and (setq bounds (bounds-of-thing-at-point 'word))
                  (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (error "No word at point."))))))

  (defun google-translate-at-point++ (&optional override-p)
    "Translate at point and show result with posframe."
    (interactive "P")
    (%google-translate-at-point++ override-p nil))

  (defun google-translate-at-point-reverse++ (&optional override-p)
    "Translate reverse at point and show result with posframe."
    (interactive "P")
    (%google-translate-at-point++ override-p t))

  (defun google-translate-chinese-at-point++ (&optional override-p)
    "如果当前位置是中文，则自动调用反向进行中转英翻译，否则进行正向
英转中翻译。并在posframe提示框里显示结果。此方法只能用于点词翻译，
不能用于划词翻译，至于原因，我还没弄明白。"
    (interactive "P")
    (if (-chinese-word-p(-region-or-word))
        (%google-translate-at-point++ override-p t)
      (%google-translate-at-point++ override-p nil)))

  (defun google-translate-chinese-at-point (&optional override-p)
    "如果当前位置是中文，则自动调用反向进行中转英翻译，否则进行正向
英转中翻译。并在另一个buffer里显示结果。此方法既可用于点词翻译，
也可用于划词翻译。"
    (interactive "P")
    (if (-chinese-word-p(-region-or-word))
        (%google-translate-at-point override-p t)
      (%google-translate-at-point override-p nil))))

;; insert-translated-name
(use-package! insert-translated-name
  :defer 2
  :init
  (map! :leader
        (:prefix-map ("y" . "translate")
         :desc "Insert translated name" "i" #'insert-translated-name-insert))
  :config
  (setq insert-translated-name-default-style 'origin))

;; dictionary-overlay
(use-package! dictionary-overlay
  :defer 2
  :init
  (setq dictionary-overlay-translators '("local" "darwin" "sdcv" "web")
        dictionary-overlay-user-data-directory (expand-file-name "dictionary-overlay-data" doom-cache-dir))
  :config
  (if (<= emacs-major-version 28)
      (defalias 'string-split #'split-string))
  (dictionary-overlay-start))
