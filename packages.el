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

(package! anzu :disable t)
(package! ws-butler :disable t)
(package! tide :disable t)
(package! tree-sitter-indent :disable t)
(package! mu4e-alert :disable t)

(package! project :pin "f8123159622f09c60c4d0be6c4c773b57c2a010d" :built-in (>= emacs-major-version 30))
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500" :built-in 'prefer)

(package! diff-hl :pin "79c46da33b249eb59a2f82fe7ad46fc9621eab0b")

(package! vterm :pin "056ad74653704bc353d8ec8ab52ac75267b7d373")

(when (modulep! :tools tree-sitter)
  (package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
  (package! tree-sitter-langs
    :recipe (:nonrecursive t)
    :pin "2ff446b4b813543b7a90015808d38f362f039b10")
  (package! ts-fold
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")
    :pin "0b2e87ea5369c96e436d909204624923afab7ab4"))

(package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82")

(package! nerd-icons :pin "43178575201e3d2ef8c4a507ed4c281b0936f39a")
(package! doom-themes
  :recipe (:host github :repo "bbenchan/doom-themes")
  :pin "187b7e7454e210d08308a42ccffe7f44b8d0ef58")

(when (modulep! :email mu4e +org)
  (package! org-msg :pin "59e2042e5f23e25f31c6aef0db1e70c6f54f117d"))

(package! cal-china-x
  :recipe (:host github :repo "cnsunyour/cal-china-x" :files ("*.el"))
  :pin "fa50ae0c5cf992e95288a3da9c02948a9b42c009")

(package! pinyinlib :pin "1772c79b6f319b26b6a394a8dda065be3ea4498d")

(package! rime :pin "80f09ed36d9f0ca7ce4e1a3ca1020dc4c80ba335")

(package! go-translate :pin "55efeac0f99f8eff3f9017e62229212e4876f09b")
(package! immersive-translate :pin "1d00d558363985fa988fc40cd5093bfc6926d83e")
(package! llm :pin "a97543d0ded21845bf2d3a437854a738b162818a")
(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name" :files ("*.el" "*.py"))
  :pin "a2bc301f6e99a6f965f7612c5666546e3d1a8fe3")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "86d72c6281a59248a6bf95fca7a031f4fa8b5b3c")

(package! websocket :pin "40c208eaab99999d7c1e4bea883648da24c03be3")
(package! websocket-bridge
  :recipe (:host github :repo "ginqi7/websocket-bridge")
  :pin "40676478e3b2dd5af068b1f7f7024776e7557446")
(package! dictionary-overlay
  :recipe (:host github :repo "ginqi7/dictionary-overlay" :files ("*.el" "*.py" "resources"))
  :pin "a488bb7cf3823bf5a0f04cc896f811a68892a448")
(package! deno-bridge
  :recipe (:host github :repo "bbenchen/deno-bridge")
  :pin "eb36e3da4d784c391a277a0da82d3f8cd3b4a96b")

(package! editorconfig
  :recipe (:nonrecursive t)
  :pin "1a9942746cf5b10daae8962f380b5f2a459086f3"
  :built-in (>= emacs-major-version 30))
(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "0fb3c0f38191c0e74f00bae6adaa342de3750e83")
(package! thing-edit
  :recipe (:host github :repo "manateelazycat/thing-edit")
  :pin "49b6a7cbc6a45d1aa9977a3f8f0e5d06b343f100")
(package! region-occurrences-highlighter :pin "7921b749b641c6682536d58139a48ab25ee2d171")
(package! visual-replace :pin "8c9321fce17e98703ba9ec03c69209defbac2723")

(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
;; (package! pinentry :pin "91d51af8a1cb970743fb4841d7876c10ceb487e8")
(package! envrc :pin "2b818ca6e4a2f723e7cab70cd0101c2728581c3a")
(package! mb-url :pin "0cfc4e1ac6dd2b9e4fa2d7114d2cc5f33657f180")

(package! aidermacs
  :recipe (:host github :repo "MatthewZMD/aidermacs")
  :pin "96e732d770b9dd5f109aa606a72c1d62ebad6fc5")
(package! gptel :pin "748fd85493889079f0fe102010c816ee75e802ec")
(package! corsair :pin "f750a435d6be68f0d75dc5a90f8aa3cb58e8c16a")

(package! easydraw
  :recipe (:host github :repo "misohena/el-easydraw")
  :pin "8ccc58b23754fcf5c3669345c7460e6d5ae32211")
(package! verb :pin "151ed6bbfff71939a1481b60b9cfd2d720d56785")
(package! ox-gfm :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")

(when (modulep! :tools magit)
  ;; Due to the bugs in the latest version of ghub limited to an available version
  (if (modulep! :tools magit +forge)
      (package! ghub :pin "af663777c47a3dce64b2144b4409587b35521e47"))
  (package! git-commit-ts-mode
    :recipe (:host github :repo "danilshvalov/git-commit-ts-mode")
    :pin "6eb42a3c08c5c6a1a610d433b93590b88a71f63e")
  (package! magit-gerrit
    :recipe (:host github :repo "darcylee/magit-gerrit")
    :pin "617d1b239942af62adaedfe4ce248f09a3a67029")
  (package! blamer :pin "8a79c1f370f7c5f041c980e0b727960462c192ba"))

(package! auto-rename-tag :pin "ceda8dcb436cc7f2a7b99d33bdeb152a242745a1")

(when (modulep! :lang go)
  (package! go-impl :pin "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")
  (package! go-fill-struct :pin "9e2e4be5af716ecadba809e73ddc95d4c772b2d9")
  (disable-packages! go-eldoc))
(package! pkgbuild-mode :pin "aadf3d1d19c5eb9b52c15c5b73b1a46faac5b7d5")
(package! protobuf-ts-mode
  :recipe (:host github :repo "emacsattic/protobuf-ts-mode")
  :pin "65152f5341ea4b3417390b3e60b195975161b8bc")
(package! nginx-mode :pin "c4ac5de975d65c84893a130a470af32a48b0b66c")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-sqlfluff :pin "0a836d7a919723ae5897fce01c3c7d651a30e8c6"))
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")
(package! sql-indent
  :recipe (:host github :repo "alex-hhh/emacs-sql-indent")
  :pin "2ed4c6a26b8f3d651ac6231eaafb2565d77c918b")
(package! ob-sql-mode :pin "2eaf436a6ac2178b94442d80f84fc6c02aa644d8")

;; (package! topsy :pin "8b6c6d5026ac72b4c3704ed7bb8fafe1ea343699")

(package! lsp-bridge
  :recipe (:host github
           :repo "bbenchan/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not compile))
  :pin "1eeba54ed4ee7be2de3afb94b1c02426bed631f0")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-bridge
    :recipe (:host github :repo "liuyinz/flymake-bridge")
    :pin "e387f43230da9c214be297b0e9393323a67e4b73"))
