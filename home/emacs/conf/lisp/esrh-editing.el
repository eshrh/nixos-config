;;; esrh-editing.el --- Editing and navigation behavior -*- lexical-binding: t; -*-

(use-package nyaatouch
  :demand t
  :config
  (turn-on-nyaatouch))

(defun just-exchange-point-and-mark ()
  "Exchange point and mark without retaining an active region."
  (interactive)
  (call-interactively #'exchange-point-and-mark)
  (deactivate-mark))

(global-set-key (kbd "C-x C-x") #'just-exchange-point-and-mark)
(global-set-key (kbd "C-x 9 1") #'exchange-point-and-mark)
(setq meow--kbd-exchange-point-and-mark "C-x 9 1")

(use-package far
  :demand t
  :bind (:map meow-normal-state-keymap
              ("`" . far-fill-paragraph)))

(use-package undo-tree
  :demand t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))

(use-package ace-window
  :demand t
  :bind (([remap other-window] . ace-window)
         ("C-x w" . ace-swap-window))
  :custom
  (aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (aw-scope 'frame)
  (aw-ignore-current t)
  (aw-background nil))

(use-package helpful
  :demand t
  :bind (("C-x h f" . helpful-callable)
         ("C-x h v" . helpful-variable)
         ("C-x h k" . helpful-key)))

(use-package rainbow-mode
  :demand t
  :hook (prog-mode . rainbow-mode))

(add-hook 'prog-mode-hook #'auto-revert-mode)

(defun esrh-enable-whitespace-cleanup ()
  "Clean whitespace after saving the current programming buffer."
  (add-hook 'after-save-hook #'whitespace-cleanup nil t))

(add-hook 'prog-mode-hook #'esrh-enable-whitespace-cleanup)

(defun esrh-enable-lisp-highlighting ()
  "Enable syntax highlighting shared by Lisp modes."
  (highlight-numbers-mode 1)
  (highlight-defined-mode 1)
  (rainbow-delimiters-mode 1))

(use-package highlight-numbers
  :demand t)

(use-package highlight-defined
  :demand t)

(use-package rainbow-delimiters
  :demand t
  :hook ((lisp-data-mode . esrh-enable-lisp-highlighting)
         (clojure-mode . esrh-enable-lisp-highlighting)))

(use-package highlight-quoted
  :demand t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1))

(global-hl-line-mode 1)
(global-prettify-symbols-mode 1)

(defun esrh-add-elisp-prettify-symbols ()
  "Add the custom nullary-lambda glyph to Emacs Lisp buffers."
  (push '("fn" . ?∅) prettify-symbols-alist))

(add-hook 'emacs-lisp-mode-hook #'esrh-add-elisp-prettify-symbols)

(use-package ligature
  :demand t
  :config
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "~~>" "***" "||=" "||>" "://"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"))
  (global-ligature-mode 1))

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package yasnippet
  :demand t
  :custom
  (yas-indent-line 'fixed)
  :config
  (yas-global-mode 1))

(provide 'esrh-editing)
;;; esrh-editing.el ends here
