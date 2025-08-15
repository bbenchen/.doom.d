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

(package! project :pin "30acfc49e54e2bc310be1b1bf99484d75408b6e2" :built-in (>= emacs-major-version 30))
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500" :built-in 'prefer)

(when (modulep! :ui workspaces)
  (package! persp-mode :pin "82680795b3dbb9f9fb023b1754902f38519d9875"))

(package! vterm :pin "056ad74653704bc353d8ec8ab52ac75267b7d373")

(when (modulep! :tools tree-sitter)
  (package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
  (package! tree-sitter-langs
    :recipe (:nonrecursive t)
    :pin "f8c4248410e0c1b3288437d63495cf52706158a8")
  (package! ts-fold
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")
    :pin "3439756b5bbab83f65914d86b093d8c237eb7275"))

(package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82")

(package! nerd-icons :pin "f3e7ba37642455e5627968b1031faeefbcac1245")
(package! doom-themes
  :recipe (:host github :repo "bbenchan/doom-themes")
  :pin "ee8a013098f2c2c2dec92ba2c237b25587212169")
(package! rainbow-mode :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")
(package! golden-ratio-scroll-screen :pin "60eb00ed7e51c0875a38cff25c9a87fe79296484")

(when (modulep! :email mu4e +org)
  (package! org-msg :pin "59e2042e5f23e25f31c6aef0db1e70c6f54f117d"))

(package! cal-china-x
  :recipe (:host github :repo "cnsunyour/cal-china-x" :files ("*.el"))
  :pin "841e7d80e950865dfffa89bfde969c1d39aebcb1")

(package! pinyinlib :pin "1772c79b6f319b26b6a394a8dda065be3ea4498d")

(package! rime :pin "1b70d9cfbac9b11a934007f103b1abc9a034268f")

(package! gt
  :recipe (:host github :repo "lorniu/gt.el")
  :pin "f9febd8583ea482f72139e02f440f3972502f5a2")
(package! immersive-translate :pin "1d00d558363985fa988fc40cd5093bfc6926d83e")
(package! llm :pin "5ce1d6bd1359a3204a65e02d5035e05205c6d411")
(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name" :files ("*.el" "*.py"))
  :pin "a2bc301f6e99a6f965f7612c5666546e3d1a8fe3")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "86d72c6281a59248a6bf95fca7a031f4fa8b5b3c")

(package! websocket :pin "40c208eaab99999d7c1e4bea883648da24c03be3")
(package! deno-bridge
  :recipe (:host github :repo "bbenchen/deno-bridge")
  :pin "eb36e3da4d784c391a277a0da82d3f8cd3b4a96b")

(package! editorconfig
  :recipe (:nonrecursive t)
  :pin "f85ec9724b01fb144159b472daad136f0941631f"
  :built-in (>= emacs-major-version 30))
(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "0fb3c0f38191c0e74f00bae6adaa342de3750e83")
(package! thing-edit
  :recipe (:host github :repo "manateelazycat/thing-edit")
  :pin "49b6a7cbc6a45d1aa9977a3f8f0e5d06b343f100")
(package! region-occurrences-highlighter :pin "c8c352655a07911a82507be61bb220010a2c262c")
(package! visual-replace :pin "f45bcb7f6663e2390f871c47ddb5ae13d3d8c8ac")

(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
;; (package! pinentry :pin "91d51af8a1cb970743fb4841d7876c10ceb487e8")
(package! envrc :pin "107dae065df857271cd3371d9f520ff13df695cf")
(package! mb-url :pin "873ba6cbb1cf1a82d6328f5cb9718fccdeb98027")

(package! aidermacs
  :recipe (:host github :repo "MatthewZMD/aidermacs")
  :pin "6b33f2ceb49b1832325e2fbfbbcb72e9df5e9128")
(package! gptel :recipe (:nonrecursive t) :pin "2e3865289aaa7abdc5e4d67798db0d9e1edcb26d")
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "495b5e0b5348dbced1448bd12cbf8847e30b5175")

(package! easydraw
  :recipe (:host github :repo "misohena/el-easydraw")
  :pin "8007f50c1c1734325c47939904f486753c7dd8ee")
(package! verb :pin "e818377f2ceddf5670dcd9a32d3de0e8bf82a8f1")
(package! ox-gfm :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")

(when (modulep! :tools magit)
  (package! gptel-magit :pin "f27c01821b67ed99ddf705c2b995f78b71394d8b")
  (package! git-commit-ts-mode
    :recipe (:host github :repo "danilshvalov/git-commit-ts-mode")
    :pin "6eb42a3c08c5c6a1a610d433b93590b88a71f63e")
  (package! magit-gerrit
    :recipe (:host github :repo "darcylee/magit-gerrit")
    :pin "64b3af92166c957bc37a37be5ce18b31f21379f7")
  (package! blamer :pin "8a79c1f370f7c5f041c980e0b727960462c192ba"))

(package! auto-rename-tag :pin "281e87ebd2f738acc5ab5e3f7e37774c1f157a8d")

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

(package! separedit :pin "5cb46a65fc6e12b753dce8f581fbfa144d011a80")

(package! lsp-bridge
  :recipe (:host github
           :repo "bbenchan/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not compile)
           :post-build
           (progn
             (ignore-errors (delete-file "~/.local/bin/python-lsp-bridge"))
             (when (executable-find "uv")
               (make-symbolic-link
                (concat (straight--repos-dir "lsp-bridge") "pyproject.toml")
                (concat (straight--build-dir "lsp-bridge") "pyproject.toml"))
               (make-symbolic-link
                (concat (straight--repos-dir "lsp-bridge") "python-lsp-bridge")
                (concat (straight--build-dir "lsp-bridge") "python-lsp-bridge"))
               (make-symbolic-link
                (concat (straight--build-dir "lsp-bridge") "python-lsp-bridge")
                "~/.local/bin/python-lsp-bridge"))))
  :pin "0751696c6d33e97a1f9deddaea87514a245b2369")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-bridge
    :recipe (:host github :repo "liuyinz/flymake-bridge")
    :pin "e387f43230da9c214be297b0e9393323a67e4b73"))
