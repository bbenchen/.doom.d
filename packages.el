;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! posframe :pin "dace2dcf105e9685b4085836645b3392dc7e2211")

(package! cal-china-x :pin "94005e678a1d2522b7a00299779f40c5c77286b8")

(package! rime :pin "6438abacace7d94f05fabc45b82d619677fc5fca")

(package! google-translate :pin "e60dd6eeb9cdb931d9d8bfbefc29a48ef9a21bd9")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "e830b3c5f54227ae57117c7262b5d0c9fcc73334")

(package! websocket :pin "2c923eba75f77af5a20154776053beaf4b8ad56c")
(package! websocket-bridge
  :recipe (:host github :repo "ginqi7/websocket-bridge")
  :pin "99c1dab933d8394ecbab1a0e07a1b940b8441ecc")
(if IS-MAC
    (package! insert-translated-name
      :recipe (:host github :repo "cxb811201/insert-translated-name" :files ("*.el" "*ts"))
      :pin "f067c888e6280a96e435c840ebe4d0760b8d9df9")
  (progn
    (package! deno-bridge
      :recipe (:host github :repo "manateelazycat/deno-bridge")
      :pin "facbe2b7e517d32a6df0645e6da14f23a51636e8")
    (package! insert-translated-name
      :recipe (:host github :repo "manateelazycat/insert-translated-name" :files ("*.el" "*ts"))
      :pin "79975d06a2fc620372706ab5aac711219b163f4c")))
(package! dictionary-overlay
  :recipe (:host github :repo "ginqi7/dictionary-overlay" :files ("*.el" "*.py" "resources"))
  :pin "faba18b289b10a26190379085ea09b6d2ae28559")

(package! phi-search :pin "c34f5800968922d1f9e7b10092b8705d6640ad18")
(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "ebaeb80fba0bafdf6f95706308123dec2cf4b99f")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "512fd791902a96aa60bc432c347f850cde2fc1ac")
(if (<= emacs-major-version 28)
    (package! duplicate-line
      :recipe (:host github :repo "manateelazycat/duplicate-line")
      :pin "03b8aa8c4ab094b59881421d34a2028c6b5ce2b1"))
(package! goto-line-preview :pin "c6db484cf401351f7f2f57496b0466b774435947")

(if IS-MAC
    (package! exec-path-from-shell :pin "ddd24dc823de9a94435b4d8ea7569161657f31e2"))
(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")

(package! which-key-posframe :pin "421cbfbe5d43ca8a48ecb18ea6d3d95f9ca6e9e6")

(if (modulep! :tools magit)
    (package! blamer :pin "d1d5f2dc4d9cd5a47c47b55abb1f3b38911cc2d0"))

(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources"))
  :pin "738324be7fbbe8608bec51cd5e352b9a21d625b4")

(when (modulep! :lang go)
  (package! go-fill-struct :pin "a613d0b378473eef39e8fd5724abe790aea84321")
  (disable-packages! go-eldoc))
(package! pkgbuild-mode :pin "9525be8ecbd3a0d0bc7cc27e6d0f403e111aa067")
(package! protobuf-mode
  :recipe (:host github :repo "emacsmirror/protobuf-mode" :files ("*.el"))
  :pin "745044422c9062e5cffbfbba33c030c30f9c0694")
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")
(package! sql-indent
  :recipe (:host github :repo "alex-hhh/emacs-sql-indent")
  :pin "d6f451dc09fb1a6f38a8327cf46ef246431afbd9")
(package! ob-sql-mode :pin "2eaf436a6ac2178b94442d80f84fc6c02aa644d8")
