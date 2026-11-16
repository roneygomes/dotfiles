;;; appearance.el -*- lexical-binding: t; -*-
;; Modeline, padding, line numbers, whitespace.  Theme, fonts and spacing live
;; in early-init.el so the first frame already looks right.

(setq inhibit-splash-screen t)

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

;; Org buffers inherit text-mode settings; keep them free of line numbers.
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; --- soft wrapping in org ---
;; Org truncates long lines by default.  visual-line-mode wraps them (clearing
;; `truncate-lines') and visual-fill-column-mode pins the wrap to a fixed
;; column instead of the window edge.
(defun my/org-soft-wrap ()
  "Wrap org lines at `visual-fill-column-width', breaking at word boundaries."
  (require 'visual-fill-column)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-soft-wrap)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text nil))

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

;; --- clock in the modeline ---
(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(setq display-time-format "%F %R")

;; better right-click context menu
(when (display-graphic-p)
  (context-menu-mode))

(provide 'appearance)
;;; appearance.el ends here
