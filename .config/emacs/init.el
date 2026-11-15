;;; init.el -*- lexical-binding: t; -*-
;; Personal Emacs configuration. See lisp/ for the modular config.

;; Set early so Custom writes to (and loads from) the right place.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Activate the package manager. Archives are configured in early-init.el.
(package-initialize)

;; Install missing packages referenced by use-package automatically.
(setq use-package-always-ensure t)

;; Load the modular configuration.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory) t)
(require 'core)
(require 'appearance)
(require 'completion-config)
(require 'keybindings)
(require 'editing)
(require 'prog)
(require 'languages)
(require 'git)

;; Restore performance settings after startup finishes.
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))
            (setq file-name-handler-alist my/file-name-handler-alist)))

;; Load Custom's own settings last so user customizations win.
(load custom-file t)

(provide 'init)
;;; init.el ends here
