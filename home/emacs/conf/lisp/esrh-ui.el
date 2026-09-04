;;; esrh-ui.el --- Frames, faces, and presentation -*- lexical-binding: t; -*-

(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)

(defun esrh-show-paren-locate-near-paren ()
  "Locate only a parenthesis immediately before point."
  (let ((before (show-paren--categorize-paren (point))))
    (when (memq (car before) '(1 -1))
      before)))

(advice-add 'show-paren--locate-near-paren
            :override #'esrh-show-paren-locate-near-paren)

(defvar esrh-monospace-font
  (if-let ((fc-match (executable-find "fc-match")))
      (car (split-string
            (string-trim
             (shell-command-to-string
              (concat fc-match " -f '%{family}' monospace")))
            ","))
    "monospace")
  "Concrete monospace family resolved through fontconfig.")

(let ((font (concat esrh-monospace-font "-12")))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font))

(use-package gruvbox-theme
  :demand t
  :config
  (load-theme 'gruvbox-dark-hard t nil)
  (when (facep 'vertico-posframe-border)
    (set-face-background 'vertico-posframe-border
                         (face-attribute 'region :background))))

(use-package telephone-line
  :demand t
  :custom
  (telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-height 24)
  (telephone-line-evil-use-short-tag t)
  :config
  (telephone-line-defsegment* telephone-line-simpler-major-mode-segment ()
    (concat "["
            (if (listp mode-name) (car mode-name) mode-name)
            "]"))

  (telephone-line-defsegment* telephone-line-simple-pos-segment ()
    (concat "%c : %l/"
            (number-to-string (count-lines (point-min) (point-max)))))

  (setq telephone-line-lhs
        '((nil . (telephone-line-projectile-buffer-segment))
          (accent . (telephone-line-simpler-major-mode-segment))
          (nil . (telephone-line-meow-tag-segment
                  telephone-line-misc-info-segment)))
        telephone-line-rhs
        '((nil . (telephone-line-simple-pos-segment))
          (accent . (telephone-line-buffer-modified-segment))))
  (telephone-line-mode 1))

(use-package dashboard
  :demand t
  :custom
  (recentf-exclude '("~/org/"))
  (dashboard-agenda-release-buffers t)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-footer nil)
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-agenda-sort-strategy '(time-up))
  (dashboard-item-names '(("Recent Files:" . "recent:")
                          ("Projects:" . "projects:")
                          ("Agenda for the coming week:" . "agenda:")))
  (dashboard-banner-logo-title "GNU emacsへようこそ。")
  :config
  (setq dashboard-startup-banner
        (expand-file-name
         (if (or (display-graphic-p) (daemonp))
             "hiten_render_rsz.png"
           "gnu.txt")
         user-emacs-directory))
  (dashboard-setup-startup-hook))

(defun esrh-configure-pixel-scroll ()
  "Use conservative pixel-scroll interpolation."
  (setq pixel-scroll-precision-large-scroll-height 1
        pixel-scroll-precision-interpolation-factor 1))

(when (fboundp 'pixel-scroll-precision-mode)
  (esrh-configure-pixel-scroll)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)
  (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))

(provide 'esrh-ui)
;;; esrh-ui.el ends here
