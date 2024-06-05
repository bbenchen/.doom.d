;;; +org.el -*- lexical-binding: t; -*-

;; org-mode
(after! org
  (setq org-tags-column -80
        org-image-actual-width 600)

  (add-to-list 'org-modules 'org-tempo t)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-to-list 'org-structure-template-alist '("verb" . "src verb"))

  (define-key org-mode-map (kbd "C-c M-r") verb-command-map))

(after! ob-ditaa
  (setq org-ditaa-jar-path (expand-file-name "ditaa.jar" doom-user-dir)
        org-ditaa-eps-jar-path (expand-file-name "DitaaEps.jar" doom-user-dir)))

(after! ox-latex
  ;; @see https://yuchi.me/post/export-org-mode-in-chinese-to-pdf-with-custom-latex-class
  ;; download elegantpaper.cls and place it in the same level as the org document
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-src-block-backend 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted")))

(use-package! ox-gfm
  :after ox)
