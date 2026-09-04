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

(package! project :pin "ffb38d7798d86c7fa6623db0f64b461abb6572c2" :built-in (> emacs-major-version 30))

(package! ghostel
  :recipe (:host github :repo "dakra/ghostel" :files ("lisp/*.el" "etc"))
  :pin "2bea18f3b52bf97d8222fea706da6fabdfc2cbb8")

(package! posframe :pin "6f89c0acd29306cb2cd023418d18134cfc507800")

(package! nerd-icons :pin "17faac7977242b470732efd417d3bcc8eb5a830e")
(package! doom-themes
  :recipe (:host github :repo "bbenchan/doom-themes")
  :pin "2fba9bf42340d35cdb5a2df4828cfc2e219a684e")
(package! rainbow-mode :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")
(package! golden-ratio-scroll-screen :pin "60eb00ed7e51c0875a38cff25c9a87fe79296484")

(when (modulep! :email mu4e +org)
  (package! org-msg :pin "7b45df759340f3e388e84f497052b7cf3a41698c"))

(package! cal-china-x
  :recipe (:host github :repo "cnsunyour/cal-china-x" :files ("*.el"))
  :pin "841e7d80e950865dfffa89bfde969c1d39aebcb1")

(package! pinyinlib :pin "1772c79b6f319b26b6a394a8dda065be3ea4498d")

(package! rime :pin "f927d26e471e7d63de65ffa92897944242f2fd92")

(package! gt
  :recipe (:host github :repo "lorniu/gt.el")
  :pin "f9febd8583ea482f72139e02f440f3972502f5a2")
(package! immersive-translate :pin "1d00d558363985fa988fc40cd5093bfc6926d83e")
(package! llm :pin "6326518018f776af2fa65b36f4bbfac07b94d345")
(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name" :files ("*.el" "*.py"))
  :pin "a2bc301f6e99a6f965f7612c5666546e3d1a8fe3")

(package! popweb
  :recipe (:host github :repo "manateelazycat/popweb" :files ("*.el" "*.py" "*.js" "extension"))
  :pin "86d72c6281a59248a6bf95fca7a031f4fa8b5b3c")

(package! websocket :pin "2195e1247ecb04c30321702aa5f5618a51c329c5")
(package! deno-bridge
  :recipe (:host github :repo "manateelazycat/deno-bridge")
  :pin "d85e517c025f6ba74f2d19c00a4898bc69c87572")

(package! hungry-delete :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save")
  :pin "515a0f5b1c5d3c331a195811521414221d6f0bbe")
(package! thing-edit
  :recipe (:host github :repo "manateelazycat/thing-edit")
  :pin "49b6a7cbc6a45d1aa9977a3f8f0e5d06b343f100")
(package! shift-number
  :recipe (:host codeberg :repo "ideasman42/emacs-shift-number")
  :pin "52f4d32080cca50da0f88b2141d597827c7341cf")
(package! region-occurrences-highlighter :pin "444789ea3567fc85ba32c7349050024ba8e5b9f8")

(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
;; (package! pinentry :pin "99480adc192f657d7d9f2eb3ed4e568df3de8613")
(package! envrc :pin "d8988cfdf85dfc5759be043567822ab40f84f316")
(package! mb-url :pin "873ba6cbb1cf1a82d6328f5cb9718fccdeb98027")

(package! ai-code :pin "f743c958dc062943e8ca77392d81c158ff7ca538")
(package! agent-shell :pin "d027d8de2b74019ae83e36b1edcd0b968295ac31")
(if (featurep :system 'macos)
    (package! agent-shell-macext
      :recipe (:host github :repo "cxa/agent-shell-macext")
      :pin "41e0a7d31434a0f3fe08c83d9acc45b5402bd3b7"))
(package! gptel :recipe (:nonrecursive t) :pin "cf0eb7d2289581e7d1f0eed52b29870d8739d4f2")
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "36fe296e016449433fa1213f4b89cb8dc7d4db5e")
(package! mcp :pin "2d172809cbdb2a40d86b28ad73bd65547cefe0e1")

(package! easydraw
  :recipe (:host github :repo "misohena/el-easydraw")
  :pin "2f05683f1636040387470f8fa051f22b22106267")
(package! verb :pin "8eca8cdb9eaebc49a7da068c74cfe52f2d37d76e")
(package! ox-gfm :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")

(when (modulep! :tools magit)
  (package! gptel-magit
    :recipe (:host github
             :repo "roife/gptel-magit")
    :pin "a0958609526ace45b8195422c42e467d5f6b65b9")
  (package! git-commit-ts-mode
    :recipe (:host github :repo "danilshvalov/git-commit-ts-mode")
    :pin "6eb42a3c08c5c6a1a610d433b93590b88a71f63e")
  (package! magit-gerrit
    :recipe (:host github :repo "darcylee/magit-gerrit")
    :pin "e3eeb34fea782c2a88fe280fa633aea3376190de")
  (package! blamer :pin "aa9b22d4e847d15a5c4659c0407aa8bf4242cc94"))

(package! auto-rename-tag :pin "56169eb7c3ba021de6696dc228148bff02120c08")

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
(package! mysql :pin "a59dd867884938b2c6d7ea528ffb2a8b093674f5")
(package! pgsql :pin "8a282e565f139f3794ff219237b0f962f2a0a9b5")
(package! clutch
  :recipe (:host github :repo "LuciusChen/clutch")
  :pin "89d1d3442c8670dc7aa938b05bab907137f10615")
(package! ob-clutch
  :recipe (:host github :repo "LuciusChen/ob-clutch")
  :pin "c0b67548b5a6ad2e228f6b0c3dc684b9fb7b85bc")

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
  :pin "e11d8e95a10d9b07e6ccc8b367abf8240946418f")
(when (modulep! :checkers syntax +flymake)
  (package! flymake-bridge
    :recipe (:host github :repo "liuyinz/flymake-bridge")
    :pin "e387f43230da9c214be297b0e9393323a67e4b73"))
