;;; early-init.el --- Startup settings -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(defun esrh-restore-gc-settings ()
  "Restore garbage collection settings after startup."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'esrh-restore-gc-settings)

;;; early-init.el ends here
