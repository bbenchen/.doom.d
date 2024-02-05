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

(unpin! (:editor snippets))

(package! all-the-icons :disable t)
(package! tide :disable t)
(package! tree-sitter-indent :disable t)

(package! nerd-icons :pin "c6a4acf19454b415cba1c43daf4bfca8fccdd9ba")

(when (modulep! :tools tree-sitter)
  (package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
  (package! tree-sitter-langs :pin "20fbbb85735a9196ba3e7fb33f99b3a904b363ba")
  (package! ts-fold
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")
    :pin "0627723e5f962fc72b238d4cf99a7f217e72aa3e"))

(package! posframe :pin "017deece88360c7297265680d78a0bb316470716")

(package! doom-themes
  :recipe (:host github :repo "bbenchan/doom-themes")
  :pin "db3cf01e6d0687ff1593cebc759e32052ce938c1")

(package! cal-china-x
  :recipe (:host github :repo "cnsunyour/cal-china-x" :files ("*.el"))
  :pin "fa50ae0c5cf992e95288a3da9c02948a9b42c009")

(package! pinyinlib :pin "1772c79b6f319b26b6a394a8dda065be3ea4498d")

(package! rime :pin "d8c0a99b0282d3e0aca53146789f6864181228e7")

(package! go-translate :pin "e5680e294f0e18d945c9f18ff7f4e6610c1ed50b")
(package! insert-translated-name
  :recipe (:host github :repo "bbenchan/insert-translated-name" :branch "google")
  :pin "4263968c2bcaf6ecc6c66c9ca43770f39aa09b8b")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "ca6262b0f0a44076526457e57056ffb92340e984")

(package! websocket :pin "40c208eaab99999d7c1e4bea883648da24c03be3")
(package! websocket-bridge
  :recipe (:host github :repo "ginqi7/websocket-bridge")
  :pin "40676478e3b2dd5af068b1f7f7024776e7557446")
(package! dictionary-overlay
  :recipe (:host github :repo "ginqi7/dictionary-overlay" :files ("*.el" "*.py" "resources"))
  :pin "a1146e47da22ed43d439bb820527ebf59c1bee4c")
(package! deno-bridge
  :recipe (:host github :repo "manateelazycat/deno-bridge")
  :pin "ad0d7a9626f81509b9b3087723010a2a9a0fca37")
(package! wraplish
  :recipe (:host github :repo "manateelazycat/wraplish" :files ("*.el" "*.py"))
  :pin "4f62da9631b825ea948406063df4f4407e82c142")

(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! goto-line-preview :pin "cc8d28d8b0a6609a2f4986c374e6ff7a151b632c")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "0fb3c0f38191c0e74f00bae6adaa342de3750e83")

(if (featurep :system 'macos)
    (package! exec-path-from-shell :pin "6c10a9d4a38425f2b494013b6bdff91537a6b6da"))
(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
(package! mind-wave
  :recipe (:host github :repo "manateelazycat/mind-wave" :files ("*.el" "*.py"))
  :pin "b787803ff745dde28727c10833b397d846fc1f7f")

(package! verb :pin "d211f066b90d714e19783cd2ea20ac96ad25e740")

(when (modulep! :tools magit)
  (package! magit-gerrit
    :recipe (:host github :repo "darcylee/magit-gerrit")
    :pin "9ea3f65918c6cb8ffdb6500b97be15cc1a15a887")
  (package! blamer :pin "f79a7422e5976eb3bee1c73bfcb33d701cc932a6"))

(package! lsp-bridge
  :recipe (:host github
           :repo "bbenchan/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources"))
  :pin "02c2adc0ceb3887db0ce4e72cecfd1c98d204468")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-bridge
    :recipe (:host github :repo "liuyinz/flymake-bridge")
    :pin "30f7ee8c5234b32c6d5acac850bb97c13ee90128"))
(package! dape
  :recipe (:host github :repo "svaante/dape")
  :pin "1cb2327202badb903b53997f133db4985c2f5214")

(when (modulep! :lang go)
  (package! go-impl :pin "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")
  (package! go-fill-struct :pin "9e2e4be5af716ecadba809e73ddc95d4c772b2d9")
  (disable-packages! go-eldoc))
(package! pkgbuild-mode :pin "9525be8ecbd3a0d0bc7cc27e6d0f403e111aa067")
(package! protobuf-ts-mode
  :recipe (:host github :repo "emacsmirror/protobuf-ts-mode")
  :pin "65152f5341ea4b3417390b3e60b195975161b8bc")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-sqlfluff :pin "598dff268231f74ba902e2c607c85dd014fbee28"))
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")
(package! sql-indent
  :recipe (:host github :repo "alex-hhh/emacs-sql-indent")
  :pin "323ece64acaac7f27b7806db9dba0757d6e57885")
(package! ob-sql-mode :pin "2eaf436a6ac2178b94442d80f84fc6c02aa644d8")
(package! auto-rename-tag :pin "288c708e5c88113a5c8c5c44361f1d3c3e334a2e")
