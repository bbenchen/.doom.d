;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ben Chan"
      user-mail-address "517926804@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(let ((font "Sarasa Fixed SC Nerd Font"))
  (cond (IS-LINUX (setq doom-font (font-spec :family font :size 11 :weight 'Regular)))
        (IS-MAC (setq doom-font (font-spec :family font :size 12 :weight 'Regular))))
  (setq doom-variable-pitch-font doom-font
        doom-serif-font doom-font
        ;; doom-unicode-font doom-font
        doom-big-font doom-font)
  (add-hook! 'after-setting-font-hook :append
    (set-fontset-font t 'cjk-misc font nil 'prepend)
    (set-fontset-font t 'han font nil 'prepend)))

(if (modulep! :ui zen)
    (setq writeroom-width 120
          +zen-text-scale 1))

;; no broder
(when (> emacs-major-version 28)
  (cond (IS-MAC
         (set-frame-parameter nil 'undecorated-round t)
         (add-to-list 'default-frame-alist '(undecorated-round . t)))
        (t
         (set-frame-parameter nil 'undecorated t)
         (add-to-list 'default-frame-alist '(undecorated . t)))))

;; transparency
(set-frame-parameter nil 'alpha 85)
(add-to-list 'default-frame-alist '(alpha . 85))

;; maximize the window
(when (and (not (> emacs-major-version 28))
           (not IS-MAC))
  (set-frame-parameter nil 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(cond (IS-MAC (pcase (frame-parameter nil 'ns-appearance)
                ('light (setq doom-theme 'doom-nord-light))
                (_ (setq doom-theme 'doom-nord-aurora)))
              (if (boundp 'ns-system-appearance-change-functions)
                  (add-hook! 'ns-system-appearance-change-functions
                             #'(lambda (appearance)
                                 (mapc #'disable-theme custom-enabled-themes)
                                 (pcase appearance
                                   ('light (load-theme 'doom-nord-light t))
                                   ('dark (load-theme 'doom-nord-aurora t)))))))
      (t (setq doom-theme 'doom-nord-aurora)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "+misc")
(load! "+chinese")
(load! "+editor")
(load! "+org")
(load! "+mail")
(load! "+prog")
(load! "+keybindings")
