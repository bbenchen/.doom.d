;;; +keybindings.el -*- lexical-binding: t; -*-

;; (global-set-key [escape] #'doom/escape)

(map! "C-z" nil
      "M-z" nil
      "C-x C-z" nil

      [escape] #'doom/escape

      "M-u"     #'upcase-dwim
      "C-x C-u" #'upcase-dwim
      "M-l"     #'downcase-dwim
      "C-x C-l" #'downcase-dwim
      "M-c"     #'capitalize-dwim

      (:when (featurep :system 'macos)
        (:when (modulep! :completion vertico)
          "s-F" #'consult-line-multi)
        "s-k" #'kill-current-buffer
        "s-x" #'kill-region
        "s-+" #'text-scale-increase
        "s-=" #'text-scale-increase
        "s--" #'text-scale-decrease
        "<s-up>"    #'shrink-window
        "<s-down>"  #'enlarge-window
        "<s-left>"  #'shrink-window-horizontally
        "<s-right>" #'enlarge-window-horizontally))

(map! (:map dirvish-mode-map
            "a"   #'dirvish-quick-access
            "z"   #'dirvish-history-jump
            "TAB" #'dirvish-subtree-toggle))

(map! (:map vterm-mode-map
            "C-\\" #'toggle-input-method))

(map! :map scala-mode-map
      :localleader
      (:prefix ("b" . "sbt")
               "." #'sbt-hydra
               "b" #'sbt-command))

(map! :map dockerfile-ts-mode-map
      :localleader
      :desc "Build"           "b"   #'dockerfile-build-buffer
      :desc "Build not cache" "M-b" #'dockerfile-build-no-cache-buffer)

(map! :leader
      (:prefix ("!" . "checkers")
               (:when (modulep! :checkers syntax +flymake)
                 :desc "Goto next error"     "n" #'flymake-goto-next-error
                 :desc "Goto prev error"     "p" #'flymake-goto-prev-error
                 :desc "Show error at point" "s" #'flymake-show-diagnostic
                 :desc "Show buffer errors"  "v" #'flymake-show-diagnostic
                 :desc "Show project errors" "V" #'flymake-show-project-diagnostics))

      (:prefix-map ("e" . "envrc")
                   "a" #'envrc-allow
                   "d" #'envrc-deny
                   "r" #'envrc-reload
                   "l" #'envrc-show-log)

      (:prefix-map ("f" . "file")
       :desc "Find file in dotfiles" "t" #'bc/find-in-dotfiles
       :desc "Browse dotfiles"       "T" #'bc/browse-dotfiles)

      (:prefix-map ("i" . "insert")
       :desc "Duplicate" "d" #'duplicate-dwim)

      (:prefix-map ("o" . "open")
       :desc "Aider"     "a" #'aider-run-aider
       :desc "ChatGPT"   "c" #'bc/start-gptel
       :desc "View undo" "u" #'vundo)

      (:prefix-map ("t" . "toggle")
       :desc "Command window"     "C" #'bc/command-log-toggle-window
       :desc "Frame transparency" "T" #'bc/toggle-frame-transparency
       :desc "Window maximize"    "m" #'doom/window-maximize-buffer
       :desc "Proxy"              "p" #'bc/toggle-proxy-http
       :desc "Rainbow mode"       "R" #'rainbow-mode)

      (:prefix-map ("w" . "workspaces/windows")
                   "<up>"    #'shrink-window
                   "<down>"  #'enlarge-window
                   "<left>"  #'shrink-window-horizontally
                   "<right>" #'enlarge-window-horizontally))

(map! (:when (modulep! :tools make)
        (:map makefile-mode-map
         :localleader
         :desc "Run task" "r" #'+make/run)))

(map! (:when (featurep 'with-editor)
        [remap async-shell-command] #'with-editor-async-shell-command
        [remap shell-command]       #'with-editor-shell-command))
