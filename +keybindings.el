;;; +keybindings.el -*- lexical-binding: t; -*-

(map! "C-z" nil
      "M-z" nil
      "C-x C-z" nil

      (:when IS-MAC
        (:when (modulep! :completion vertico)
          "s-F" #'consult-line-multi)
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
