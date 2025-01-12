(setq custom-file "~/.config/emacs/custom.el")          ; file managed by custom
(load custom-file t)

(setq initial-major-mode 'fundamental-mode)             ; default mode for the *scratch* buffer

(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq auto-revert-avoid-polling t)                      ; automatically reread from disk if the underlying file changes
(setq auto-revert-interval 5)                           ; some systems don't do file notifications well
(setq auto-revert-check-vc-info t)

(setq ring-bell-function #'ignore)                      ; no bell sound, no flashing screen

(setq use-short-answers t)
(setq custom-safe-themes t)
(setq require-final-newline t)

;; backups and auto-saves
(setq backup-directory-alist
      `(("." . "~/.config/emacs/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-save-list/" t)))

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(use-package package
  :ensure t
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory) t)

(require 'appearance)
(require 'completion-config)
(require 'key-bindings)
(require 'programming)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; load the system's PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package vterm
  :ensure t
  :config
  (setq vterm-term-environment-variable "xterm-256color"))

(use-package vterm-toggle
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package deadgrep
  :ensure t)

(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(display-time-mode 1)

(provide 'init)
;;; init.el ends here
