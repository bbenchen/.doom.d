;;; +keybindings.el -*- lexical-binding: t; -*-

;; (global-set-key [escape] #'doom/escape)

(map! "C-z" nil
      "M-z" nil
      "C-x C-z" nil
      "C-<left>" nil
      "C-<right>" nil

      [escape] #'doom/escape

      "M-u"     #'upcase-dwim
      "C-x C-u" #'upcase-dwim
      "M-l"     #'downcase-dwim
      "C-x C-l" #'downcase-dwim
      "M-c"     #'capitalize-dwim

      (:when (modulep! :completion vertico)
        "M-g m" #'consult-mark
        "M-g k" #'consult-global-mark)

      (:when (featurep :system 'macos)
        (:when (modulep! :completion vertico)
          "s-F" #'consult-line-multi)
        "s-k" #'kill-current-buffer
        "s-x" #'kill-region
        "s-=" #'text-scale-increase
        "s--" #'text-scale-decrease
        "s-+" #'shift-number-up
        "s-_" #'shift-number-down
        "<s-up>"    #'shrink-window
        "<s-down>"  #'enlarge-window
        "<s-left>"  #'shrink-window-horizontally
        "<s-right>" #'enlarge-window-horizontally))

(map! :map dirvish-mode-map
      "?"   #'dirvish-dispatch
      "a"   #'dirvish-quick-access
      "F"   #'dirvish-layout-toggle
      "h"   #'dired-up-directory
      "l"   #'dired-find-file
      "y"   #'dirvish-yank
      "z"   #'dirvish-history-jump
      "TAB" #'dirvish-subtree-toggle)

(map! :map vterm-mode-map
      "C-\\"       #'toggle-input-method
      "M-<return>" #'vterm--self-insert
      "M-RET"      #'vterm--self-insert)

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
      (:when (modulep! :checkers syntax +flymake)
        (:prefix-map ("!" . "checkers")
         :desc "Goto next error"     "n" #'flymake-goto-next-error
         :desc "Goto prev error"     "p" #'flymake-goto-prev-error
         :desc "Show buffer errors"  "l" #'flymake-show-buffer-diagnostics
         :desc "Show project errors" "L" #'flymake-show-project-diagnostics))

      (:prefix-map ("c" . "code")
       :desc "Separedit"                   "'"  #'separedit
       :desc "LSP Code actions"            "a"  #'lsp-bridge-code-action
       :desc "Jump to symbol in workspace" "j"  #'lsp-bridge-workspace-list-symbols
       :desc "LSP Rename"                  "r"  #'lsp-bridge-rename
       :desc "LSP Peek"                    "p"  #'lsp-bridge-peek)

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
                   (:prefix ("l" . "llm")
                    :desc "Start ai-code"        "a" #'ai-code-cli-start
                    :desc "Open ai-code menu"    "A" #'ai-code-menu
                    :desc "Add text to gptel"    "c" #'gptel-add
                    :desc "Explain"              "e" #'gptel-quick
                    :desc "Add file to gptel"    "f" #'gptel-add-file
                    :desc "Open gptel"           "g" #'bc/start-gptel
                    :desc "Open gptel menu"      "m" #'gptel-menu
                    :desc "Rewrite"              "r" #'gptel-rewrite
                    :desc "Send to gptel"        "s" #'gptel-send)
                   :desc "Open ghostel"            "t" #'ghostel
                   :desc "Open ghostel in project" "T" #'ghostel-project
                   :desc "View undo"               "u" #'vundo)

      (:prefix-map ("t" . "toggle")
       :desc "Command window"     "C" #'bc/command-log-toggle-window
       :desc "Frame transparency" "T" #'bc/toggle-frame-transparency
       :desc "Window maximize"    "m" #'doom/window-maximize-buffer
       :desc "Proxy"              "p" #'bc/toggle-proxy-http
       :desc "Rainbow mode"       "R" #'rainbow-mode)

      (:prefix-map ("v" . "versioning")
       :desc "Blamer show commit" "b" #'blamer-show-posframe-commit-info
       :desc "Blamer" "B" #'blamer-mode)

      (:prefix-map ("w" . "workspaces/windows")
                   "<up>"    #'shrink-window
                   "<down>"  #'enlarge-window
                   "<left>"  #'shrink-window-horizontally
                   "<right>" #'enlarge-window-horizontally)

      (:prefix-map ("y" . "translate")
       :desc "Google translate"        "g" #'gt-translate
       :desc "Google translate prompt" "G" #'bc/gt-do-translate-prompt
       :desc "Insert translated name"  "i" #'insert-translated-name-insert
       :desc "Bing translate"          "b" #'popweb-dict-bing-pointer
       :desc "Bing translate input"    "B" #'popweb-dict-bing-input
       :desc "Youdao translate"        "y" #'popweb-dict-youdao-pointer
       :desc "Youdao translate input"  "Y" #'popweb-dict-youdao-input
       :desc "Play voice"              "p" #'popweb-dict-say-word))

(map! (:when (modulep! :tools make)
        (:map makefile-mode-map
         :localleader
         :desc "Run task" "r" #'+make/run)))

(map! (:when (featurep 'with-editor)
        [remap async-shell-command] #'with-editor-async-shell-command
        [remap shell-command]       #'with-editor-shell-command))
