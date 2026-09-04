;;; esrh-org.el --- Org authoring and export -*- lexical-binding: t; -*-

(when (file-directory-p "~/org/")
  (setq org-directory "~/org/"
        org-agenda-files '("~/org/")))

(setq org-list-allow-alphabetical t
      org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
      org-edit-src-content-indentation 0
      org-deadline-warning-days 2
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t
      org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf %f"))

(defun esrh-org-mode-setup ()
  "Enable the preferred visual and authoring behavior in Org buffers."
  (org-indent-mode 1)
  (electric-quote-local-mode -1)
  (auto-fill-mode 1))

(use-package org
  :ensure nil
  :defer t
  :hook (org-mode . esrh-org-mode-setup)
  :bind (:map org-mode-map
              ("s-<return>" . org-meta-return)))

(defun org-inside-latex-block ()
  "Return non-nil when point is inside a LaTeX environment."
  (eq (car (org-element-at-point)) 'latex-environment))

(use-package org-fragtog
  :after org
  :commands org-fragtog-mode
  :custom
  (org-fragtog-ignore-predicates
   '(org-at-table-p org-inside-latex-block)))

(use-package ivy-bibtex
  :commands ivy-bibtex
  :custom
  (bibtex-completion-bibliography '("~/docs/library.bib")))

(use-package org-ref
  :after org
  :commands (org-ref-insert-link
             org-ref-insert-link-menu
             org-ref-citation-menu)
  :bind (:map org-mode-map
              ("C-c ]" . org-ref-insert-link)
              ("S-]" . org-ref-insert-link-menu)
              ("C-c r" . org-ref-citation-menu))
  :custom
  (org-ref-insert-link-function 'org-ref-insert-link-menu)
  (org-ref-insert-cite-function 'org-ref-cite-insert-ivy)
  (org-ref-insert-label-function 'org-ref-insert-label-link)
  (org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (org-ref-cite-onclick-function
   (lambda (_key) (org-ref-citation-menu))))

(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("IEEEtran"
     "\\documentclass{IEEEtran}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(provide 'esrh-org)
;;; esrh-org.el ends here
