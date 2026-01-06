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

(package! project :pin "a66b0e0987ce5b81a36d81cc43eed2c9dc0bdeeb" :built-in (> emacs-major-version 30))
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500" :built-in 'prefer)

(package! vterm :pin "a01a2894a1c1e81a39527835a9169e35b7ec5dec")

(package! posframe :pin "d93828bf6c36383c365bd564ad3bab5a4403804c")

(package! nerd-icons :pin "d41902fe68a94fcf4394f25a89ba2d510bec4461")
(package! doom-themes
  :recipe (:host github :repo "bbenchan/doom-themes")
  :pin "c5b52efe05d67db93409fe4dadac53d5c2332446")
(package! rainbow-mode :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")
(package! golden-ratio-scroll-screen :pin "60eb00ed7e51c0875a38cff25c9a87fe79296484")

(when (modulep! :email mu4e +org)
  (package! org-msg :pin "327768e2c38020f6ea44730e71f2a62f3f0ce3bd"))

(package! cal-china-x
  :recipe (:host github :repo "cnsunyour/cal-china-x" :files ("*.el"))
  :pin "841e7d80e950865dfffa89bfde969c1d39aebcb1")

(package! pinyinlib :pin "1772c79b6f319b26b6a394a8dda065be3ea4498d")

(package! rime :pin "f927d26e471e7d63de65ffa92897944242f2fd92")

(package! gt
  :recipe (:host github :repo "lorniu/gt.el")
  :pin "f9febd8583ea482f72139e02f440f3972502f5a2")
(package! immersive-translate :pin "1d00d558363985fa988fc40cd5093bfc6926d83e")
(package! llm :pin "5c876ce3d7958b99c0980cb2f53ad55385146a3e")
(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name" :files ("*.el" "*.py"))
  :pin "a2bc301f6e99a6f965f7612c5666546e3d1a8fe3")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "86d72c6281a59248a6bf95fca7a031f4fa8b5b3c")

(package! websocket :pin "40c208eaab99999d7c1e4bea883648da24c03be3")
(package! deno-bridge
  :recipe (:host github :repo "manateelazycat/deno-bridge")
  :pin "d85e517c025f6ba74f2d19c00a4898bc69c87572")

(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "0fb3c0f38191c0e74f00bae6adaa342de3750e83")
(package! thing-edit
  :recipe (:host github :repo "manateelazycat/thing-edit")
  :pin "49b6a7cbc6a45d1aa9977a3f8f0e5d06b343f100")
(package! region-occurrences-highlighter :pin "98fc1020c68f339810beb753a29daba93ade57b5")

(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
;; (package! pinentry :pin "91d51af8a1cb970743fb4841d7876c10ceb487e8")
(package! envrc :pin "06d72d141ac2e2990d80cdb8bb84f6cb54c628a5")
(package! mb-url :pin "873ba6cbb1cf1a82d6328f5cb9718fccdeb98027")

(package! aidermacs
  :recipe (:host github :repo "MatthewZMD/aidermacs")
  :pin "6d0c41d1cfd24821fb32933edf8c0c2a9bb8c847")
(package! gptel :recipe (:nonrecursive t) :pin "f36ee0a750344768b02d08716f176cc20fd712c3")
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "018ff2be8f860a1e8fe3966eec418ad635620c38")

(package! easydraw
  :recipe (:host github :repo "misohena/el-easydraw")
  :pin "1caace5c9b25b659d64f96c0c44015a9514e960f")
(package! verb :pin "f45e31b2bcdea2a859bb28cbb1819469978457c9")
(package! ox-gfm :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")

(when (modulep! :tools magit)
  (package! gptel-magit
    ;; REVIEW: Revert to upstream if ragnard/gptel-magit#7 is merged.
    :recipe (:host github
             :repo "ArthurHeymans/gptel-magit")
    :pin "4a40c3fc201d60d2f0589c2e1a6693fd94bb4c98")
  (package! git-commit-ts-mode
    :recipe (:host github :repo "danilshvalov/git-commit-ts-mode")
    :pin "6eb42a3c08c5c6a1a610d433b93590b88a71f63e")
  (package! magit-gerrit
    :recipe (:host github :repo "darcylee/magit-gerrit")
    :pin "8f1082d55f3b71589463adaeab3413924383a9c9")
  (package! blamer :pin "aa9b22d4e847d15a5c4659c0407aa8bf4242cc94"))

(package! auto-rename-tag :pin "ace6de8bc8200aa9c9f37c8266d0e1b51627b559")

(package! markdown-inline-images
  :recipe (:host github :repo "domschl/markdown-inline-images.el")
  :pin "925d58b92d13d18e1569df591d415ec15a7d6a1e")
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
  :pin "144ebce793c96d7bc86bad5feb143c8678bc3c8f")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-bridge
    :recipe (:host github :repo "liuyinz/flymake-bridge")
    :pin "e387f43230da9c214be297b0e9393323a67e4b73"))
