;;; appearance.el -*- lexical-binding: t; -*-
;; Theme, fonts, modeline, padding, line numbers, whitespace.

(setq custom-safe-themes t)
(load-theme 'modus-operandi-tinted t)

(setq inhibit-splash-screen t)

(setq-default line-spacing 0.1)
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)
(setq-default tab-width 4)

;; line numbers (relative, enabled in text/prog modes below)
(setq display-line-numbers-type 'relative)

;; trailing whitespace + hard tabs
(setq whitespace-style '(face tabs trailing tab-mark))

(defun my-essential-modes ()
  "Modes to enable in every text and programming buffer."
  (column-number-mode 1)
  (electric-pair-mode 1)
  (display-line-numbers-mode 1))

(add-hook 'text-mode-hook #'my-essential-modes)
(add-hook 'prog-mode-hook #'my-essential-modes)

(defun my-toggle-whitespace-mode ()
  "Toggle display of whitespace characters (tabs and trailing spaces)."
  (interactive)
  (whitespace-mode 'toggle)
  (if whitespace-mode
      (message "Whitespace mode enabled")
    (message "Whitespace mode disabled")))

(blink-cursor-mode -1)

;; --- modeline ---
(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name-with-project
        doom-modeline-icon nil
        doom-modeline-time-analogue-clock nil
        doom-modeline-modal nil
        doom-modeline-display-misc-in-all-mode-lines nil
        doom-modeline-env-version nil
        doom-modeline-percent-position nil
        doom-modeline-persp-name nil
        doom-modeline-buffer-encoding nil
        doom-modeline-minor-modes nil
        doom-modeline-check-simple-format t
        doom-modeline-lsp nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil)
  (doom-modeline-mode 1))

;; --- padding ---
(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 6
          :header-line-width 2
          :mode-line-width 3
          :tab-width 2
          :right-divider-width 6
          :fringe-width 8))
  (spacious-padding-mode 1))

;; --- fonts ---
(set-face-attribute 'default nil :font "Iosevka" :height 160 :weight 'regular)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 150)
(set-face-attribute 'variable-pitch nil :font "Geneva" :height 150 :weight 'regular)

;; --- clock in the modeline ---
(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(setq display-time-format "%F %R")

;; better right-click context menu
(when (display-graphic-p)
  (context-menu-mode))

(provide 'appearance)
;;; appearance.el ends here
