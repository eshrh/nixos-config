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
