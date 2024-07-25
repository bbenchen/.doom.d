;; -*- no-byte-compile: t; lexical-binding: t; -*-
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

(unpin! (:editor snippets) (:tools editorconfig))

(package! ws-butler :disable t)
(package! tide :disable t)
(package! tree-sitter-indent :disable t)
(package! mu4e-alert :disable t)

(package! project :pin "c0fb3da8b90735fb280ab157f4a8033b42105125" :built-in (>= emacs-major-version 30))
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500" :built-in 'prefer)

(package! diff-hl :pin "57d9d4e3e17397bf178c3aa5c369b5edd24523e0")

(package! vterm :pin "d9ea29fb10aed20512bd95dc5b8c1a01684044b1")

(when (modulep! :tools tree-sitter)
  (package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
  (package! tree-sitter-langs :pin "0e603504b8d4f9865d149105dff619072ab0ed40")
  (package! ts-fold
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")
    :pin "f6fc276e73ebaf2928450a71f2c4609e372473b6"))

(package! posframe :pin "493b17f74ad41104d0c4d648e8c96dc495279ac8")

(package! nerd-icons :pin "4322290303f2e12efd5685a0d22d76ed76ec7349")
(package! doom-themes
  :recipe (:host github :repo "bbenchan/doom-themes")
  :pin "df6af5c10757e14459d47c3050cd5814e1de5763")

(when (modulep! :email mu4e +org)
  (package! org-msg
    :recipe (:host github :repo "danielfleischer/org-msg" :branch "1.12")
    :pin "4dcd70f14c0046e316dcab54dde0eb7b082ce632"))

(package! cal-china-x
  :recipe (:host github :repo "cnsunyour/cal-china-x" :files ("*.el"))
  :pin "fa50ae0c5cf992e95288a3da9c02948a9b42c009")

(package! pinyinlib :pin "1772c79b6f319b26b6a394a8dda065be3ea4498d")

(package! rime :pin "d8c0a99b0282d3e0aca53146789f6864181228e7")

(package! go-translate :pin "c8c45f02e8d3b035da3104e4e248970b123def89")
(package! immersive-translate :pin "1d00d558363985fa988fc40cd5093bfc6926d83e")
(package! llm :pin "32e0a71150062b815213a9bba07666f15580401a")
(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name" :files ("*.el" "*.py"))
  :pin "a2bc301f6e99a6f965f7612c5666546e3d1a8fe3")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "ddb3422ba45523c5408bbfc3caf6d5453ef805c1")

(package! websocket :pin "40c208eaab99999d7c1e4bea883648da24c03be3")
(package! websocket-bridge
  :recipe (:host github :repo "ginqi7/websocket-bridge")
  :pin "40676478e3b2dd5af068b1f7f7024776e7557446")
(package! dictionary-overlay
  :recipe (:host github :repo "ginqi7/dictionary-overlay" :files ("*.el" "*.py" "resources"))
  :pin "9ef93d1de071de40b71644e3dd7a83342399c24b")
(package! deno-bridge
  :recipe (:host github :repo "manateelazycat/deno-bridge")
  :pin "ad0d7a9626f81509b9b3087723010a2a9a0fca37")

(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! goto-line-preview :pin "2d17b10252f3dc8b9737b86e901fc56016ee0f8a")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "0fb3c0f38191c0e74f00bae6adaa342de3750e83")

(if (featurep :system 'macos)
    (package! exec-path-from-shell :pin "72ede29a0e0467b3b433e8edbee3c79bab005884"))
(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
;; (package! pinentry :pin "91d51af8a1cb970743fb4841d7876c10ceb487e8")
(package! envrc :pin "532ecdab48d073963ba5b581c06f3c0b83db05a6")

(package! aichat
  :recipe (:host github :repo "xhcoding/emacs-aichat" :files ("aichat-util.el" "aichat-openai.el"))
  :pin "1b3270c96080a67389f6089d489e883c8367e6f8")
(package! async-await :pin "e0d15e8057ed7520100bc50c5552278292ebcb07")

(package! easydraw
  :recipe (:host github :repo "misohena/el-easydraw")
  :pin "6f93e744d5f62de2176d3d0f0aa1f9e8d84ccefd")
(package! verb :pin "ff9f9b4b441fef596db0a7dbcf761414e2c469bd")
(package! ox-gfm :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")

(when (modulep! :tools magit)
  (package! magit-gerrit
    :recipe (:host github :repo "darcylee/magit-gerrit")
    :pin "9ea3f65918c6cb8ffdb6500b97be15cc1a15a887")
  (package! blamer :pin "a6526820ac37a2ab041c4abda57b78f133299bf2"))

(package! auto-rename-tag :pin "fb0a38e644354f38578fee8e0bf5fe7f124aa8a7")

(when (modulep! :lang go)
  (package! go-impl :pin "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")
  (package! go-fill-struct :pin "9e2e4be5af716ecadba809e73ddc95d4c772b2d9")
  (disable-packages! go-eldoc))
(package! pkgbuild-mode :pin "8ef396d8fa9187b65c065a6bc2ca15dfaf3255df")
(package! protobuf-ts-mode
  :recipe (:host github :repo "emacsattic/protobuf-ts-mode")
  :pin "65152f5341ea4b3417390b3e60b195975161b8bc")
(package! nginx-mode :pin "c4ac5de975d65c84893a130a470af32a48b0b66c")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-sqlfluff :pin "0a836d7a919723ae5897fce01c3c7d651a30e8c6"))
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")
(package! sql-indent
  :recipe (:host github :repo "alex-hhh/emacs-sql-indent")
  :pin "c3dd49ccd1f0655ed1699058c16a777ac1cb1419")
(package! ob-sql-mode :pin "2eaf436a6ac2178b94442d80f84fc6c02aa644d8")

;; (package! topsy :pin "8b6c6d5026ac72b4c3704ed7bb8fafe1ea343699")

(package! lsp-bridge
  :recipe (:host github
           :repo "bbenchan/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not compile))
  :pin "4a934857454a74fa5c608829b49ac59b9452ba1a")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-bridge
    :recipe (:host github :repo "liuyinz/flymake-bridge")
    :pin "30f7ee8c5234b32c6d5acac850bb97c13ee90128"))
