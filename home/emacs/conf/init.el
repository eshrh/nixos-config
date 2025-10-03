;; -*- lexical-binding: t; -*-
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
    (url-retrieve-synchronously
     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-built-in-pseudo-packages '(project))

(defalias 'sup 'straight-use-package)

(setq comp-deferred-compilation t)
(setq warning-suppress-log-types '((comp)))

(defmacro add-fs-to-hook (hook &rest funcs)
  "Add functions to hook. A function is either an unquoted token, or a form.
If it's a token, then its treated as a function and enabled. Otherwise, the form is run."
  `(add-hook ,hook
             (fn ,@(mapcar (lambda (el)
                             (if (listp el)
                                 el
                               (list el 1)))
                           funcs))))

(defmacro add-to-hooks (f &rest hooks)
  "Add a single function to many quoted hooks"
  `(progn ,@(mapcar (lambda (hook)
                      `(add-hook ,hook ,f))
                    hooks)))

(defmacro fn (&rest forms)
  (declare (indent 0))
  `(lambda () ,@forms))

(defmacro -< (expr &rest forms)
  (declare (indent defun))
  (let ((var (gensym)))
    `(let ((,var ,expr))
       (list ,@(--map (pcase it
                        ((pred symbolp) (list it var))
                        ((pred listp) (-snoc it var)))
                      forms)))))

(defmacro -<< (expr &rest forms)
  (declare (indent defun))
  (let ((var (gensym)))
    `(let ((,var ,expr))
       (list ,@(--map (pcase it
                        ((pred symbolp) (list it var))
                        (`(,first . ,rest) `(,first ,var ,@rest)))
                      forms)))))

(sup 's)
(sup 'dash)

(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?h] 'help-command)

(global-set-key [?\C-z] #'kill-whole-line)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

(setq vc-handled-backends '(Git))
(setq remote-file-name-inhibit-locks t)
(setq remote-file-name-inhibit-cache nil)

(setq ring-bell-function 'ignore)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(scroll-bar-mode -1)
(setq use-dialog-box nil)

(column-number-mode)
(show-paren-mode)
(defun show-paren--locate-near-paren-ad ()
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (let* ((before (show-paren--categorize-paren (point))))
    (when (or
       (eq (car before) 1)
       (eq (car before) -1))
      before)))

(advice-add 'show-paren--locate-near-paren
            :override #'show-paren--locate-near-paren-ad)

(sup 'rainbow-mode)
(add-hook 'prog-mode #'rainbow-mode)

(add-fs-to-hook 'prog-mode-hook
                (add-hook 'after-save-hook
                          (fn (whitespace-cleanup))))

(defvar emacs-english-font "monospace")
(defvar emacs-cjk-font "IPAGothic")
(setq my-font (concat emacs-english-font "-12"))

(add-to-list 'default-frame-alist `(font . ,my-font))
(set-face-attribute 'default t :font my-font)

(sup 'gruvbox-theme)

(load-theme 'gruvbox-dark-hard t nil)

(setq-default frame-title-format '("emacs: %b"))

(sup 'telephone-line)

(require 'telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24)

(setq telephone-line-evil-use-short-tag t)

(telephone-line-defsegment* telephone-line-simpler-major-mode-segment ()
  (concat "["
          (if (listp mode-name)
              (car mode-name)
            mode-name)
          "]"))

(telephone-line-defsegment* telephone-line-simple-pos-segment ()
  (concat "%c : " "%l/" (number-to-string (count-lines (point-min) (point-max)))))

(setq telephone-line-lhs
      '((nil . (telephone-line-projectile-buffer-segment))
        (accent . (telephone-line-simpler-major-mode-segment))
        (nil . (telephone-line-meow-tag-segment
                telephone-line-misc-info-segment)))
      telephone-line-rhs
      '((nil . (telephone-line-simple-pos-segment))
        (accent . (telephone-line-buffer-modified-segment))))

(telephone-line-mode 1)

(defun pixel-scroll-setup ()
  (interactive)
  (setq pixel-scroll-precision-large-scroll-height 1)
  (setq pixel-scroll-precision-interpolation-factor 1))

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-setup)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)
  (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))

(sup '(nyaatouch
       :repo "https://github.com/eshrh/nyaatouch"
       :fetcher github))
(turn-on-nyaatouch)

(defun just-exchange-point-and-mark ()
  (interactive)
  (call-interactively #'exchange-point-and-mark)
  (deactivate-mark))

(global-set-key (kbd "C-x C-x") #'just-exchange-point-and-mark)
(global-set-key (kbd "C-x 9 1") #'exchange-point-and-mark) ; unused key
(setq meow--kbd-exchange-point-and-mark "C-x 9 1")

(straight-use-package
 '(far :type git
       :repo "https://github.com/eshrh/far.el"))

(meow-normal-define-key
 '("`" . far-fill-paragraph))

(sup 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

(sup 'ace-window)
(global-set-key [remap other-window] 'ace-window)
(global-set-key (kbd "C-x w") 'ace-swap-window)

(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

(setq aw-scope 'frame)

(setq aw-ignore-current t)
(setq aw-background nil)

(sup 'dashboard)
(dashboard-setup-startup-hook)

(setq recentf-exclude '("~/org/"))
(setq dashboard-agenda-release-buffers t)

(setq initial-buffer-choice (get-buffer "*dashboard*"))

(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-set-footer nil)

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))
(setq dashboard-agenda-sort-strategy '(time-up))
(setq dashboard-item-names '(("Recent Files:" . "recent:")
                             ("Projects:" . "projects:")
                             ("Agenda for the coming week:" . "agenda:")))

(setq dashboard-banner-logo-title "GNU emacsへようこそ。")

(defmacro set-dashboard-banner (name)
  `(setq dashboard-startup-banner
         (expand-file-name ,name user-emacs-directory)))
(if (or (display-graphic-p) (daemonp))
    (set-dashboard-banner "hiten_render_rsz.png")
  (set-dashboard-banner "gnu.txt"))

(sup 'corfu)
(setq corfu-auto t)
(add-hook 'after-init-hook #'global-corfu-mode)

(sup 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun find-file-or-projectile ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

(global-set-key (kbd "C-x C-f") 'find-file-or-projectile)
;; just in case i need to use standard find file, probably to make a file.
(meow-leader-define-key '("U" . find-file))

(sup '(vertico :files (:defaults "extensions/*")
               :includes (vertico-directory)))
(vertico-mode)

(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)

(sup 'marginalia)
(marginalia-mode)

(when (display-graphic-p)
  (sup 'vertico-posframe)
  (vertico-posframe-mode 1))

(when (display-graphic-p)
  (set-face-background 'vertico-posframe-border
                       (face-attribute 'region :background)))

(sup 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(sup 'helpful)

(-map (lambda (pair) (global-set-key
                 (kbd (concat "C-x h " (car pair))) (cdr pair)))
      (-zip-pair '("f" "v" "k")
            '(helpful-callable helpful-variable helpful-key)))

(global-set-key (kbd "C-x d") #'dired-jump)
(global-set-key (kbd "C-x C-j") #'dired)

(setq dired-dwim-target t)

(add-fs-to-hook 'dired-mode-hook (dired-hide-details-mode 1))

(setq dired-kill-when-opening-new-dired-buffer t)

  (add-fs-to-hook 'dired-mode-hook
                  (define-key dired-mode-map (kbd "-") #'swiper)
                  (define-key dired-mode-map (kbd "<") #'beginning-of-buffer)
                  (define-key dired-mode-map (kbd ">") #'end-of-buffer))

(setq treesit-available (and (fboundp 'treesit-available-p)
                             (treesit-available-p)))

(when treesit-available
  (defun treesitter-grammar-url (lang)
    (concat "https://github.com/tree-sitter/tree-sitter-" lang))
  (setq treesit-langs
        '(bash c cpp haskell html java javascript julia rust python))
  (setq treesit-language-source-alist
        (--map `(,it . (,(treesitter-grammar-url (symbol-name it)))) treesit-langs)))

(defun treesit-ensure (lang)
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(sup 'highlight-defined)
(sup 'highlight-numbers)
(sup 'rainbow-delimiters)
(sup 'highlight-quoted)
(defun highlight-lisp-things-generic ()
  (highlight-numbers-mode)
  (highlight-defined-mode)
  (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
(add-to-hooks #'highlight-lisp-things-generic 'lisp-data-mode-hook 'clojure-mode-hook)

(sup 'hl-todo)
(global-hl-todo-mode)

(global-hl-line-mode)

(sup 'vterm)
(sup 'fish-mode)

(add-fs-to-hook 'vterm-mode-hook (setq-local global-hl-line-mode
                                             (null global-hl-line-mode)))

(setq vterm-kill-buffer-on-exit t)

(setq vterm-buffer-name-string "vt")

(add-to-list 'meow-mode-state-list '(vterm-mode . insert))

(sup 'vterm-toggle)
(setq vterm-toggle-hide-method 'delete-window)
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                 (let ((buffer (get-buffer buffer-or-name)))
                   (equal major-mode 'vterm-mode)))
               (display-buffer-reuse-window display-buffer-at-bottom)
               (dedicated . t)
               (reusable-frames . visible)
               (window-height . 0.3)))

(defun vterm--kill-vterm-buffer-and-window (process event)
  "Kill buffer and window on vterm process termination."
  (when (not (process-live-p process))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (kill-buffer)
          (ignore-errors (delete-window))
          (message "VTerm closed."))))))

(add-fs-to-hook 'vterm-mode-hook
                (set-process-sentinel (get-buffer-process (buffer-name))
                                      #'vterm--kill-vterm-buffer-and-window))

(meow-leader-define-key
 '("d" . vterm-toggle-cd))

(when (file-exists-p "~/org/")
  (setq org-directory "~/org/")
  (setq org-agenda-files '("~/org/")))

(setq org-list-allow-alphabetical t)

(add-fs-to-hook 'org-mode-hook
                org-indent-mode
                (electric-quote-mode -1)
                auto-fill-mode)

(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(sup 'ox-pandoc)
(setq org-export-backends '(latex beamer md html odt ascii pandoc))

(setq org-edit-src-content-indentation 0)

(setq org-deadline-warning-days 2)

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)

(sup 'org-fragtog)

(defun org-inside-latex-block ()
  (eq (nth 0 (org-element-at-point)) 'latex-environment))
(setq org-fragtog-ignore-predicates '(org-at-table-p org-inside-latex-block))

(sup 'org-ref)
(sup 'ivy-bibtex)

(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-<return>") 'org-meta-return)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "S-]") 'org-ref-insert-link-hydra/body)
  (define-key org-mode-map (kbd "C-c r") 'org-ref-citation-hydra/body))
(setq bibtex-completion-bibliography '("~/docs/library.bib"))
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(sup 'org-roam)
(setq org-roam-v2-ack t)

(unless (file-directory-p "~/roam")
  (make-directory "~/roam"))
(setq org-roam-directory (file-truename "~/roam"))

(setq org-return-follows-link t)

(global-set-key (kbd "C-c c i") #'org-roam-node-insert)
(global-set-key (kbd "C-c c f") #'org-roam-node-find)
(global-set-key (kbd "C-c c s") #'org-roam-db-sync)
(global-set-key (kbd "C-c c p") (fn (interactive) (load-file "~/roam/publish.el")))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq erc-default-server "irc.libera.chat")

(add-hook 'erc-before-connect (lambda (SERVER PORT NICK)
                                (when (file-exists-p "ircconfig.elc")
                                  (load-file
                                   (expand-file-name
                                    "ircconfig.elc"
                                    user-emacs-directory)))))

(sup 'yasnippet)
(yas-global-mode)
(setq yas-indent-line 'fixed)

(sup 'magit)

(setq ediff-diff-options "")
(setq ediff-custom-diff-options "-u")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)

(global-prettify-symbols-mode)
(add-fs-to-hook 'emacs-lisp-mode-hook
                (push '("fn" . ?∅) prettify-symbols-alist))

(sup 'ligature)
(ligature-set-ligatures
 'prog-mode
 '(  "|||>" "<|||" "<==>" "<!--" "~~>" "***" "||=" "||>"   "://"
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
(global-ligature-mode)

(unless (boundp 'eglot)
  (sup 'eglot))


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio"))))
(add-to-hooks #'eglot-ensure 'python-mode-hook 'python-ts-mode-hook)

(custom-set-faces
 '(eglot-highlight-symbol-face ((t (:inherit nil)))))

(add-fs-to-hook 'flymake-mode-hook (define-key flymake-mode-map (kbd "C-c C-n") #'flymake-goto-next-error))

(when (executable-find "rg")
  (sup 'deadgrep))

(sup 'gptel)
(with-eval-after-load 'gptel
  (setcdr (assoc 'default gptel-directives)
          (cdr (assoc 'programming gptel-directives))))

(sup 'haskell-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)

(setq haskell-interactive-popup-errors t)

(when treesit-available
  (treesit-ensure 'c)
  (treesit-ensure 'cpp)
  (add-to-list 'major-mode-remap-alist
               '(c-mode . c-ts-mode)))

(setq-default c-basic-offset 4
              kill-whole-line t
              indent-tabs-mode nil)

(sup 'slime)
(setq inferior-lisp-program "sbcl")
(sup 'slime-company)
(add-fs-to-hook 'common-lisp-mode-hook (slime-setup '(slime-fancy slime-company)))
(add-hook 'lisp-mode-hook #'flycheck-mode)

(sup 'aggressive-indent-mode)
(add-hook 'lisp-data-mode-hook #'aggressive-indent-mode 1)

(smartparens-global-mode)

(defun sp-disable (mode str)
  (sp-local-pair mode str nil :actions nil))

(sp-disable 'lisp-data-mode "'")

(sup 'racket-mode)
(sup 'scribble-mode)

(sup 'clojure-mode)
(sup 'cider)
(sp-disable 'clojure-mode "'")

(sup 'elisp-format)
(setq elisp-format-column 80)
(sp-disable 'emacs-lisp-mode "'")
(sp-disable 'emacs-lisp-mode "`")
(sp-disable 'org-mode "'")

(sup 'auctex)
(setq TeX-parse-self t)

(setq pdf-viewer-exec-alist '((sioyek . "Sioyek")
                              (zathura . "Zathura")
                              (evince . "evince")
                              (okular . "Okular")))

(setq my-pdf-viewer (->> pdf-viewer-exec-alist
                         (-first (-compose #'executable-find #'symbol-name #'car))
                         cdr))

(add-fs-to-hook 'LaTeX-mode-hook
                (setq TeX-view-program-selection
                      `((output-pdf ,my-pdf-viewer)
                        (output-dvi ,my-pdf-viewer)
                        (output-html "xdg-open")))
                auto-fill-mode)

(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook #'TeX-fold-mode)

(sup 'outline-magic)
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)
(add-fs-to-hook 'LaTeX-mode-hook (define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle))

(defvar my-LaTeX-no-autofill-environments
  '("equation" "equation*", "tabular", "table")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun my-LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`my-LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment my-LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun my-LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `my-LaTeX-auto-fill-function'."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'my-LaTeX-auto-fill-function))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-setup-auto-fill)

(advice-add #'japanese-latex-mode :after
            (lambda () (setq TeX-PDF-from-DVI "Dvipdfmx")))

(when treesit-available
  (treesit-ensure 'python)
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))
(setq python-mode-hook-alias
      (if treesit-available
          'python-ts-mode-hook
        'python-mode-hook))
(setq python-mode-map-alias
      (if treesit-available
          'python-ts-mode-map
        'python-mode-map))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(sup 'conda)

(sup '(campus
       :type git
       :fetcher github
       :repo "https://github.com/eshrh/campus-emacs"
       :files ("*.el")))

(if treesit-available
    (add-fs-to-hook 'python-ts-mode-hook
                    (define-key python-ts-mode-map (kbd "C-c C-l")
                                #'python-shell-send-buffer)
                    (define-key python-ts-mode-map (kbd "C-c + +")
                                #'campus-make-partition)
                    (define-key python-ts-mode-map (kbd "C-c + -")
                                #'campus-remove-partition-forward)
                    (define-key python-ts-mode-map (kbd "C-c C-c")
                                #'campus-send-region))
    (add-fs-to-hook 'python-mode-hook
                    (define-key python-mode-map (kbd "C-c C-l")
                                #'python-shell-send-buffer)
                    (define-key python-mode-map (kbd "C-c + +")
                                #'campus-make-partition)
                    (define-key python-mode-map (kbd "C-c + -")
                                #'campus-remove-partition-forward)
                    (define-key python-mode-map (kbd "C-c C-c")
                                #'campus-send-region)))

(defun python-describe-at-point1 (symbol process)
  (interactive (list (python-info-current-symbol)
                     (python-shell-get-process)))
  (comint-send-string process (concat "help(" symbol ")\n")))

(advice-add #'python-describe-at-point :override #'python-describe-at-point1)

(defun python-clear-matplotlib ()
  (interactive)
  (python-shell-send-string-no-output "plt.clf()")
  (message "Matplotlib plot cleared."))

(if treesit-available
    (add-fs-to-hook 'python-ts-mode-hook
                (define-key python-ts-mode-map (kbd "C-c C-,")
                  #'python-clear-matplotlib))
  (add-fs-to-hook 'python-mode-hook
                (define-key python-mode-map (kbd "C-c C-,")
                  #'python-clear-matplotlib)))

(add-fs-to-hook (if treesit-available 'python-ts-mode-hook 'python-mode-hook)
                (push '("None" . ?∅) prettify-symbols-alist)
                (push '("return" . ?») prettify-symbols-alist)) ;❱)

(sup '(kbd-mode
       :type git
       :repo "https://github.com/kmonad/kbd-mode"))
(add-hook 'kbd-mode-hook (fn (aggressive-indent-mode -1)))

(sup 'rust-mode)
(when treesit-available
    (treesit-ensure 'rust)
    (setq rust-mode-treesitter-derive t))


(sup '(matsurika-mode
       :type git
       :host github
       :repo "eshrh/matsurika-mode"
       :files ("*.el" "docs.txt")))

(sup 'nix-mode)

(sup 'julia-snail)
(add-hook 'julia-mode-hook #'julia-snail-mode)

(sup 'markdown-mode)

(add-fs-to-hook 'java-mode-hook
                flycheck-mode
                (setq c-basic-offset 4)
                (setq tab-width 4))


(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-hooks #'eglot-ensure 'tsx-ts-mode-hook)
(setq js-indent-level 4)

(setq user-full-name "Eshan Ramesh"
      user-mail-address "esrh@esrh.me")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks nil)

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defconst emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun maybe-delete-frame-buffer (frame)
  "When a dedicated FRAME is deleted, also kill its buffer.
  A dedicated frame contains a single window whose buffer is not
  displayed anywhere else."
  (let ((windows (window-list frame)))
    (when (eq 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (eq 1 (length (get-buffer-window-list buffer nil t)))
          (kill-buffer buffer))))))
(add-hook 'delete-frame-functions #'maybe-delete-frame-buffer)

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(global-set-key (kbd "C-c /") #'comment-or-uncomment-region)

(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(defun load-init ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun load-this-file ()
  (interactive)
  (load-file (buffer-file-name)))

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'load-this-file)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(setq mode-require-final-newline t)

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "")

(setq confirm-kill-processes nil)

(define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-n") #'comint-next-input)
(define-key comint-mode-map (kbd "C-w") #'backward-kill-word)
