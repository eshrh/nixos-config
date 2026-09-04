;;; esrh-languages.el --- Programming language support -*- lexical-binding: t; -*-

(require 'seq)

(defconst esrh-treesit-available-p
  (and (fboundp 'treesit-available-p) (treesit-available-p))
  "Whether this Emacs build supports tree-sitter.")

(defun esrh-treesit-language-available-p (language)
  "Return non-nil when the Nix-provided grammar for LANGUAGE is available."
  (and esrh-treesit-available-p
       (treesit-language-available-p language)))

(defun esrh-eglot-ensure ()
  "Load Eglot and start it for the current language buffer."
  (require 'eglot)
  (eglot-ensure))

(when (esrh-treesit-language-available-p 'c)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

(when (esrh-treesit-language-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(when (esrh-treesit-language-available-p 'tsx)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(use-package eglot
  :ensure nil
  :defer t
  :hook ((python-mode . esrh-eglot-ensure)
         (python-ts-mode . esrh-eglot-ensure)
         (tsx-ts-mode . esrh-eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio"))))

(custom-set-faces
 '(eglot-highlight-symbol-face ((t (:inherit nil)))))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c C-n") #'flymake-goto-next-error))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode)
  :custom
  (haskell-interactive-popup-errors t))

(use-package daml-ts-mode
  :mode ("\\.daml\\'" . daml-ts-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-mode-treesitter-derive
   (esrh-treesit-language-available-p 'rust)))

(defun esrh-common-lisp-setup ()
  "Enable SLIME, Company integration, and Flycheck for Common Lisp."
  (require 'slime)
  (require 'slime-company)
  (slime-setup '(slime-fancy slime-company))
  (flycheck-mode 1))

(use-package slime
  :commands slime
  :custom
  (inferior-lisp-program "sbcl")
  :hook (lisp-mode . esrh-common-lisp-setup))

(use-package slime-company
  :after slime)

(use-package flycheck
  :commands flycheck-mode)

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :hook (lisp-data-mode . aggressive-indent-mode))

(defun esrh-racket-mode-setup ()
  "Enable structural editing helpers for Racket."
  (rainbow-delimiters-mode 1)
  (aggressive-indent-mode 1)
  (when (executable-find "racket")
    (racket-xp-mode 1)))

(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :hook (racket-mode . esrh-racket-mode-setup)
  :init
  (dolist (form '(switch switch-lambda on π try))
    (put form 'racket-indent-function 1)))

(use-package scribble-mode
  :mode ("\\.scrbl\\'" . scribble-mode))

(require 'quail)
(quail-define-package
 "racket-qi" "English" "QI" nil "qi symbols"
 nil t t t t nil nil nil nil nil t)
(quail-define-rules
 ("\\>" "~>")
 ("\\q" ?☯))

(use-package clojure-mode
  :mode "\\.clj[sc]?\\'")

(use-package cider
  :commands (cider-jack-in cider-connect))

(use-package elisp-format
  :commands elisp-format-buffer
  :custom
  (elisp-format-column 80))

(defconst esrh-pdf-viewer-executables
  '((sioyek . "Sioyek")
    (zathura . "Zathura")
    (evince . "evince")
    (okular . "Okular"))
  "PDF viewers in preference order, paired with their AUCTeX names.")

(defvar esrh-pdf-viewer
  (cdr (seq-find (lambda (viewer)
                   (executable-find (symbol-name (car viewer))))
                 esrh-pdf-viewer-executables))
  "AUCTeX name of the first available PDF viewer.")

(defvar esrh-LaTeX-no-autofill-environments
  '("equation" "equation*" "tabular" "table")
  "LaTeX environments in which automatic filling is disabled.")

(defun esrh-LaTeX-auto-fill-function ()
  "Fill text unless point is in an excluded LaTeX environment."
  (let ((fill-p t)
        (environment "")
        (level 0))
    (while (and fill-p (not (string= environment "document")))
      (setq level (1+ level)
            environment (LaTeX-current-environment level)
            fill-p (not (member environment
                                esrh-LaTeX-no-autofill-environments))))
    (when fill-p
      (do-auto-fill))))

(defun esrh-LaTeX-mode-setup ()
  "Configure authoring, folding, references, and PDF viewing for LaTeX."
  (setq-local TeX-view-program-selection
              `((output-pdf ,esrh-pdf-viewer)
                (output-dvi ,esrh-pdf-viewer)
                (output-html "xdg-open")))
  (auto-fill-mode 1)
  (setq-local auto-fill-function #'esrh-LaTeX-auto-fill-function)
  (turn-on-reftex)
  (TeX-fold-mode 1)
  (outline-minor-mode 1))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . esrh-LaTeX-mode-setup)
  :custom
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t))

(use-package outline-magic
  :after tex
  :bind (:map outline-minor-mode-map
              ("<tab>" . outline-cycle)))

(defun esrh-use-dvipdfmx (&rest _)
  "Select Dvipdfmx after entering Japanese LaTeX mode."
  (setq TeX-PDF-from-DVI "Dvipdfmx"))

(advice-add 'japanese-latex-mode :after #'esrh-use-dvipdfmx)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args
      "-i --simple-prompt --InteractiveShell.display_page=True")

(defun esrh-python-describe-at-point (symbol process)
  "Ask PROCESS for Python help about SYMBOL."
  (interactive (list (python-info-current-symbol)
                     (python-shell-get-process)))
  (comint-send-string process (concat "help(" symbol ")\n")))

(defun python-clear-matplotlib ()
  "Clear the current Matplotlib figure in the inferior Python process."
  (interactive)
  (python-shell-send-string-no-output "plt.clf()")
  (message "Matplotlib plot cleared."))

(defun esrh-python-mode-setup ()
  "Add Python-specific symbols and Campus key bindings."
  (push '("None" . ?∅) prettify-symbols-alist)
  (push '("return" . ?») prettify-symbols-alist))

(use-package python
  :ensure nil
  :defer t
  :hook ((python-mode . esrh-python-mode-setup)
         (python-ts-mode . esrh-python-mode-setup))
  :bind (:map python-base-mode-map
              ("C-c C-l" . python-shell-send-buffer)
              ("C-c + +" . campus-make-partition)
              ("C-c + -" . campus-remove-partition-forward)
              ("C-c C-c" . campus-send-region)
              ("C-c C-," . python-clear-matplotlib))
  :config
  (advice-add 'python-describe-at-point
              :override #'esrh-python-describe-at-point))

(use-package campus
  :commands (campus-make-partition
             campus-remove-partition-forward
             campus-send-region))

(use-package matsurika-mode
  :mode "\\.matsurika\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package agda2-mode
  :mode "\\.agda\\'")

(provide 'esrh-languages)
;;; esrh-languages.el ends here
