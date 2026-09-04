;;; esrh-core.el --- Core behavior and key bindings -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'use-package)

(setq use-package-always-ensure nil
      read-process-output-max (* 1024 1024)
      native-comp-jit-compilation t
      warning-suppress-log-types '((comp))
      ring-bell-function #'ignore
      use-dialog-box nil
      vc-handled-backends '(Git)
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-cache nil
      user-full-name "Eshan Ramesh"
      user-mail-address "esrh@esrh.me"
      vc-follow-symlinks nil
      mode-require-final-newline t
      initial-major-mode 'lisp-interaction-mode
      initial-scratch-message ""
      confirm-kill-processes nil)

(setq-default frame-title-format '("emacs: %b")
              c-basic-offset 4
              kill-whole-line t
              indent-tabs-mode nil)

(defmacro fn (&rest forms)
  "Return a nullary lambda that evaluates FORMS."
  (declare (indent 0))
  `(lambda () ,@forms))

(defmacro add-fs-to-hook (hook &rest forms)
  "Add one function containing FORMS to HOOK.
Symbols are called as nullary functions with argument 1."
  `(add-hook ,hook
             (fn ,@(mapcar (lambda (form)
                             (if (listp form)
                                 form
                               (list form 1)))
                           forms))))

(defmacro add-to-hooks (function &rest hooks)
  "Add FUNCTION to each of HOOKS."
  `(progn ,@(mapcar (lambda (hook)
                      `(add-hook ,hook ,function))
                    hooks)))

(defmacro -< (expression &rest forms)
  "Branch EXPRESSION into the final argument position of FORMS."
  (declare (indent defun))
  (let ((value (make-symbol "value")))
    `(let ((,value ,expression))
       (list ,@(mapcar (lambda (form)
                        (if (symbolp form)
                            `(,form ,value)
                          (append form (list value))))
                      forms)))))

(defmacro -<< (expression &rest forms)
  "Branch EXPRESSION into the first argument position of FORMS."
  (declare (indent defun))
  (let ((value (make-symbol "value")))
    `(let ((,value ,expression))
       (list ,@(mapcar (lambda (form)
                        (if (symbolp form)
                            `(,form ,value)
                          `(,(car form) ,value ,@(cdr form))))
                      forms)))))

(defalias 'yes-or-no-p #'y-or-n-p)

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defconst esrh-temporary-directory
  (expand-file-name (format "emacs%d/" (user-uid)) temporary-file-directory)
  "Directory used for backup and auto-save files.")

(make-directory esrh-temporary-directory t)
(setq backup-directory-alist `((".*" . ,esrh-temporary-directory))
      auto-save-file-name-transforms `((".*" ,esrh-temporary-directory t))
      auto-save-list-file-prefix esrh-temporary-directory
      recentf-save-file (expand-file-name "recentf" esrh-temporary-directory))

(defun esrh-create-parent-directory (filename &rest _)
  "Create the parent directory of FILENAME when necessary."
  (unless (file-exists-p filename)
    (when-let ((directory (file-name-directory filename)))
      (make-directory directory t))))

(advice-add 'find-file :before #'esrh-create-parent-directory)

(defun split-and-follow-horizontally ()
  "Split below, balance windows, and select the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  "Split right, balance windows, and select the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun maybe-delete-frame-buffer (frame)
  "Kill FRAME's buffer when it is unique to a single-window FRAME."
  (let ((windows (window-list frame)))
    (when (= 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (= 1 (length (get-buffer-window-list buffer nil t)))
          (kill-buffer buffer))))))

(add-hook 'delete-frame-functions #'maybe-delete-frame-buffer)

(defun load-init ()
  "Reload every module in this configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun load-this-file ()
  "Load the file visited by the current buffer."
  (interactive)
  (load-file (buffer-file-name)))

(defun kill-other-buffers ()
  "Kill every buffer except the current buffer."
  (interactive)
  (mapc #'kill-buffer (delq (current-buffer) (buffer-list))))

(global-set-key (kbd "C-h") #'delete-backward-char)
(global-set-key (kbd "C-x h") #'help-command)
(global-set-key (kbd "C-z") #'kill-whole-line)
(global-set-key (kbd "C-x 2") #'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") #'split-and-follow-vertically)
(global-set-key (kbd "C-x k") #'kill-buffer)
(global-set-key (kbd "C-x C-k") #'kill-buffer-and-window)
(global-set-key (kbd "C-c /") #'comment-or-uncomment-region)

(define-key key-translation-map (kbd "C-x") (kbd "C-u"))
(define-key key-translation-map (kbd "C-u") (kbd "C-x"))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'load-this-file))

(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
  (define-key comint-mode-map (kbd "C-n") #'comint-next-input)
  (define-key comint-mode-map (kbd "C-w") #'backward-kill-word))

(cond
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell"
        ispell-local-dictionary "en_US"
        ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
           ("-d" "en_US") nil utf-8)))
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(provide 'esrh-core)
;;; esrh-core.el ends here
