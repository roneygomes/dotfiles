;; file managed by custom
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file t)

(setopt inhibit-splash-screen t)
(setopt make-backup-files nil)
(setopt create-lockfiles nil)

(setopt auto-revert-avoid-polling t)                    ; automatically reread from disk if the underlying file changes
(setopt auto-revert-interval 5)                         ; some systems don't do file notifications well
(setopt auto-revert-check-vc-info t)

(setopt initial-major-mode 'fundamental-mode)           ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil)          ; this information is useless for most

(setq ring-bell-function #'ignore)                      ; no bell sound, no flashing screen

(setq switch-to-prev-buffer-skip-regexp "\*[^*]+\*")    ; ignore special buffers when cycling back
(setq switch-to-next-buffer-skip-regexp "\*[^*]+\*")    ; ignore special buffers when cycling forward

(setq display-line-numbers-type 'relative)              ; relative line numbers
(setopt display-line-numbers-width 3)                   ; set a minimum width

(setq whitespace-style '(face tabs trailing tab-mark))  ; use a face for displaying trailing whitespace and hard tabs

(setq-default line-spacing 0.1)

(set-face-attribute 'default nil
  :font "Iosevka Comfy"
  :height 160
  :weight 'regular)

;; icons on the completion menu
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; for anything text editing we should have these essential modes
(defun my-essential-modes ()
  (whitespace-mode 1)
  (hl-line-mode 1)
  (display-line-numbers-mode 1))

(add-hook 'text-mode-hook 'my-essential-modes)
(add-hook 'prog-mode-hook 'my-essential-modes)

;; it's ok to enable these for all buffers
(global-auto-revert-mode 1)
(savehist-mode 1)
(recentf-mode 1)

;; fix archaic defaults
(setopt sentence-end-double-space nil)

;; make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; backups and auto-saves
(setq backup-directory-alist
      `(("." . "~/.config/emacs/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-save-list/" t)))

(use-package emacs
  :config
  (load-theme 'modus-operandi-tinted))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; load the system's PATH on gui emacs in macOS
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package package
  :ensure t
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(use-package vterm
  :ensure t
  :hook
  ((vterm-mode . (lambda ()
                   (turn-off-evil-mode))))

  :config
  (setq vterm-term-environment-variable "xterm-256color"))

(use-package sudo-edit
  :ensure t)

;; my custom configs
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory) t)

(require 'completion-config)
(require 'key-bindings)
(require 'programming)
