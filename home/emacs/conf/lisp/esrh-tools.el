;;; esrh-tools.el --- Projects, files, terminals, and version control -*- lexical-binding: t; -*-

(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (projectile-mode 1))

(defun find-file-or-projectile ()
  "Find a project file when inside a project, or visit any file."
  (interactive)
  (if (projectile-project-p)
      (call-interactively #'projectile-find-file)
    (call-interactively #'find-file)))

(global-set-key (kbd "C-x C-f") #'find-file-or-projectile)
(meow-leader-define-key '("U" . find-file))

(use-package envrc
  :demand t
  :config
  (envrc-global-mode 1))

(use-package dired
  :ensure nil
  :demand t
  :bind (("C-x C-j" . dired)
         (:map dired-mode-map
               ("-" . swiper)
               ("<" . beginning-of-buffer)
               (">" . end-of-buffer)))
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-x
  :ensure nil
  :demand t
  :bind ("C-x d" . dired-jump))

(defun esrh-vterm-process-sentinel (process _event)
  "Kill PROCESS's buffer and window after the process exits."
  (unless (process-live-p process)
    (when-let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (kill-buffer)
          (ignore-errors (delete-window))
          (message "VTerm closed."))))))

(defun esrh-vterm-setup ()
  "Configure the current vterm buffer."
  (hl-line-mode -1)
  (when-let ((process (get-buffer-process (current-buffer))))
    (set-process-sentinel process #'esrh-vterm-process-sentinel)))

(use-package vterm
  :demand t
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-buffer-name-string "vt")
  :hook (vterm-mode . esrh-vterm-setup))

(with-eval-after-load 'meow
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert)))

(use-package vterm-toggle
  :demand t
  :custom
  (vterm-toggle-hide-method 'delete-window)
  (vterm-toggle-fullscreen-p nil)
  :config
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _action)
       (with-current-buffer (get-buffer buffer-or-name)
         (derived-mode-p 'vterm-mode)))
     (display-buffer-reuse-window display-buffer-at-bottom)
     (dedicated . t)
     (reusable-frames . visible)
     (window-height . 0.3))))

(meow-leader-define-key '("d" . vterm-toggle-cd))

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package magit
  :demand t)

(setq ediff-diff-options ""
      ediff-custom-diff-options "-u"
      ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-vertically)

(use-package deadgrep
  :if (executable-find "rg")
  :demand t)

(setq erc-default-server "irc.libera.chat")

(defun esrh-load-erc-secrets (&rest _)
  "Load the byte-compiled ERC secrets file when it exists."
  (let ((secrets (expand-file-name "ircconfig.elc" user-emacs-directory)))
    (when (file-exists-p secrets)
      (load-file secrets))))

(add-hook 'erc-before-connect #'esrh-load-erc-secrets)

(provide 'esrh-tools)
;;; esrh-tools.el ends here
