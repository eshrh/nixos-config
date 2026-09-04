;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst esrh-config-modules
  '(esrh-core
    esrh-completion
    esrh-editing
    esrh-ui
    esrh-tools
    esrh-org
    esrh-languages)
  "Features that make up this configuration.")

(dolist (module esrh-config-modules)
  (load (symbol-name module) nil 'nomessage))

;;; init.el ends here
