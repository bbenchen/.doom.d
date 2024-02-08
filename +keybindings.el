;;; +keybindings.el -*- lexical-binding: t; -*-

;; (global-set-key [escape] #'doom/escape)

(map! "C-z" nil
      "M-z" nil
      "C-x C-z" nil

      [escape] #'doom/escape

      "<M-up>"    #'drag-stuff-up
      "<M-down>"  #'drag-stuff-down
      "<M-left>"  #'drag-stuff-left
      "<M-right>" #'drag-stuff-right

      "<M-u>"   #'upcase-dwim
      "C-x C-u" #'upcase-dwim
      "<M-l>"   #'downcase-dwim
      "C-x C-l" #'downcase-dwim
      "<M-c>"   #'capitalize-dwim

      (:when (featurep :system 'macos)
        (:when (modulep! :completion vertico)
          "s-F" #'consult-line-multi)
        "s-k" #'doom/save-and-kill-buffer
        "s-x" #'kill-region
        "s-+" #'text-scale-increase
        "s-=" #'text-scale-increase
        "s--" #'text-scale-decrease
        "<s-up>" #'shrink-window
        "<s-down>" #'enlarge-window
        "<s-left>" #'shrink-window-horizontally
        "<s-right>" #'enlarge-window-horizontally))

(map! (:map vterm-mode-map
            "C-\\" #'toggle-input-method))

(map! :leader
      (:prefix-map ("f" . "file")
       :desc "Find file in dotfiles" "t" #'find-in-dotfiles
       :desc "Browse dotfiles" "T" #'browse-dotfiles)

      (:prefix-map ("t" . "toggle")
       :desc "Command window" "C" #'toggle-command-window
       :desc "Frame transparency" "T" #'toggle-frame-transparency
       :desc "Window maximize" "m" #'doom/window-maximize-buffer
       :desc "Proxy" "p" #'toggle-proxy-http
       :desc "Rainbow mode" "R" #'rainbow-mode)

      (:prefix-map ("w" . "workspaces/windows")
                   "<up>" #'shrink-window
                   "<down>" #'enlarge-window
                   "<left>" #'shrink-window-horizontally
                   "<right>" #'enlarge-window-horizontally))

(map! (:when (modulep! :tools make)
        (:map makefile-mode-map
         :localleader
         :desc "Run task" "r" #'+make/run)))

(map! (:when (featurep 'with-editor)
        [remap async-shell-command] #'with-editor-async-shell-command
        [remap shell-command] #'with-editor-shell-command))
