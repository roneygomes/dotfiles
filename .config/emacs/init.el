;; file managed by custom
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file t)

(setq inhibit-splash-screen t)
(setq initial-major-mode 'fundamental-mode)           ; default mode for the *scratch* buffer

(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq auto-revert-avoid-polling t)                    ; automatically reread from disk if the underlying file changes
(setq auto-revert-interval 5)                         ; some systems don't do file notifications well
(setq auto-revert-check-vc-info t)

(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(setq display-time-format "%F %R")

(setq ring-bell-function #'ignore)                      ; no bell sound, no flashing screen

(setq switch-to-prev-buffer-skip-regexp "\*[^*]+\*")    ; ignore special buffers when cycling back
(setq switch-to-next-buffer-skip-regexp "\*[^*]+\*")    ; ignore special buffers when cycling forward

(setq display-line-numbers-type 'relative)              ; relative line numbers
(setq display-line-numbers-width 3)                     ; set a minimum width

(setq whitespace-style '(face tabs trailing tab-mark))  ; use a face for displaying trailing whitespace and hard tabs

(setq use-short-answers t)
(setq custom-safe-themes t)
(setq require-final-newline t)
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(setq-default line-spacing 0.2)

(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-mixed-fonts t)

(load-theme 'modus-operandi-tinted)

(set-face-attribute 'fixed-pitch nil
  :font "Monaco"
  :height 140
  :weight 'regular)

(set-face-attribute 'default nil
  :font "Monaco"
  :height 140
  :weight 'regular)

(set-face-attribute 'variable-pitch nil
  :font "Geneva"
  :height 150
  :weight 'regular)

(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode)
  (markdown-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

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
  (column-number-mode 1)
  (electric-pair-mode 1))

(add-hook 'text-mode-hook 'my-essential-modes)
(add-hook 'prog-mode-hook 'my-essential-modes)

;; it's ok to enable these for all buffers
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(display-time-mode 1)

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
  :config
  (setq vterm-term-environment-variable "xterm-256color"))

(use-package vterm-toggle
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode 1))

(use-package deadgrep
  :ensure t)

;; my custom configs
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory) t)

(require 'completion-config)
(require 'key-bindings)
(require 'programming)
