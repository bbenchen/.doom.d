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

(package! tide :disable t)
(package! tree-sitter-indent :disable t)

(package! posframe :pin "017deece88360c7297265680d78a0bb316470716")

(package! cal-china-x
  :recipe (:host github :repo "cnsunyour/cal-china-x" :files ("*.el"))
  :pin "fa50ae0c5cf992e95288a3da9c02948a9b42c009")

(package! rime :pin "6438abacace7d94f05fabc45b82d619677fc5fca")

(package! google-translate :pin "e60dd6eeb9cdb931d9d8bfbefc29a48ef9a21bd9")
(package! insert-translated-name
  :recipe (:host github :repo "cxb811201/insert-translated-name" :branch "google")
  :pin "4263968c2bcaf6ecc6c66c9ca43770f39aa09b8b")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "b374b4a5da44dde82c06b59dfd99caedc6291824")

(package! websocket :pin "a39ea953f27f6ae105ac5b1343852ddf13438048")
(package! websocket-bridge
  :recipe (:host github :repo "ginqi7/websocket-bridge")
  :pin "40676478e3b2dd5af068b1f7f7024776e7557446")
(package! dictionary-overlay
  :recipe (:host github :repo "ginqi7/dictionary-overlay" :files ("*.el" "*.py" "resources"))
  :pin "a1146e47da22ed43d439bb820527ebf59c1bee4c")
(package! deno-bridge
  :recipe (:host github :repo "manateelazycat/deno-bridge")
  :pin "ad0d7a9626f81509b9b3087723010a2a9a0fca37")
(package! deno-bridge-jieba
  :recipe (:host github :repo "ginqi7/deno-bridge-jieba" :files ("*.el" "*.ts"))
  :pin "9f03c09db96064964905991a51ce4c664c44dcc5")

(package! phi-search :pin "c34f5800968922d1f9e7b10092b8705d6640ad18")
(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "ebaeb80fba0bafdf6f95706308123dec2cf4b99f")
(package! goto-line-preview :pin "ac672c7d556e70b05eef499fb62c3f5093e4819e")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "e0a6f2e85e2aae115321e2cdfee892d48f0d9597")
(if (<= emacs-major-version 28)
    (package! duplicate-line
      :recipe (:host github :repo "manateelazycat/duplicate-line")
      :pin "03b8aa8c4ab094b59881421d34a2028c6b5ce2b1"))

(if IS-MAC
    (package! exec-path-from-shell :pin "03fc0a38af9e396c98f5a30c392cf757b3a34feb"))
(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
(package! mind-wave
  :recipe (:host github :repo "manateelazycat/mind-wave" :files ("*.el" "*.py"))
  :pin "994618abcd2c6a09af49b486d270ad4fd2d5b4a4")

(when (modulep! :tools magit)
  (package! magit-gerrit :pin "a97521574c5b7d4b7ab89e25c358c87fd5b1887f")
  (package! blamer :pin "ab00e6a3ea482c342d918add9c20bfd3fa740aaa"))

(package! lsp-bridge
  :recipe (:host github
           :repo "cxb811201/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources"))
  :pin "c75c1b2ebc78b3af8c506309ed270b086bcff570")

(when (modulep! :lang go)
  (package! go-impl :pin "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")
  (package! go-fill-struct :pin "9e2e4be5af716ecadba809e73ddc95d4c772b2d9")
  (disable-packages! go-eldoc))
(package! pkgbuild-mode :pin "9525be8ecbd3a0d0bc7cc27e6d0f403e111aa067")
(package! protobuf-ts-mode :pin "65152f5341ea4b3417390b3e60b195975161b8bc")
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")
(package! sql-indent
  :recipe (:host github :repo "alex-hhh/emacs-sql-indent")
  :pin "d6f451dc09fb1a6f38a8327cf46ef246431afbd9")
(package! ob-sql-mode :pin "2eaf436a6ac2178b94442d80f84fc6c02aa644d8")
(package! auto-rename-tag :pin "74dc96fafe63ff81a44e67ceceb6e0a23b6277b2")
