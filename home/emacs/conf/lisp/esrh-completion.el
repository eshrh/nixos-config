;;; esrh-completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-

(use-package vertico
  :demand t
  :config
  (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :demand t
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

(use-package vertico-sort
  :after vertico
  :demand t
  :custom
  (vertico-sort-function #'vertico-sort-history-length-alpha))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

(use-package vertico-posframe
  :if (display-graphic-p)
  :after vertico
  :demand t
  :config
  (vertico-posframe-mode 1))

(provide 'esrh-completion)
;;; esrh-completion.el ends here
