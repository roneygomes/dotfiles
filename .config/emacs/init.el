(load-theme 'modus-operandi-tinted t)

(setq initial-major-mode 'fundamental-mode)             ; default mode for the *scratch* buffer
(setq help-window-select t)

(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq auto-revert-avoid-polling t)                      ; automatically reread from disk if the underlying file changes
(setq auto-revert-interval 5)                           ; some systems don't do file notifications well
(setq auto-revert-check-vc-info t)

(setq ring-bell-function #'ignore)                      ; no bell sound, no flashing screen

(setq use-short-answers t)
(setq custom-safe-themes t)
(setq require-final-newline t)

(setq-default with-editor-emacsclient-executable "emacsclient")

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

(use-package vterm
  :ensure t
  :config
  (setq vterm-term-environment-variable "xterm-256color"))

(use-package vterm-toggle
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package neotree
  :ensure t
  :config
  (setq neo-show-slash-for-folder nil)
  (setq neo-show-hidden-files t)
  (setq neo-show-updir-line nil)
  (setq neo-hide-cursor t)
  (setq neo-smart-open t)
  (setq neo-autorefresh t)
  (setq neo-window-width 40)

  (add-hook 'neo-after-create-hook
          (lambda (&optional _)
            (setq-local evil-normal-state-cursor '(nil)
                        evil-emacs-state-cursor  '(nil))))

  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; global modes
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(display-time-mode 1)

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file t)

(provide 'init)
;;; init.el ends here
