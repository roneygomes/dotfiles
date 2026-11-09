(setq-default line-spacing 0.1)

(setq inhibit-splash-screen t)

(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(setq display-time-format "%F %R")

(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)
(setq-default tab-width 4)

(setq display-line-numbers-type 'relative)

;; use a face for displaying trailing whitespace and hard tabs
(setq whitespace-style '(face tabs trailing tab-mark))

;; for anything text editing we should have these essential modes
(defun my-essential-modes ()
  (column-number-mode 1)
  (electric-pair-mode 1))

(add-hook 'text-mode-hook 'my-essential-modes)
(add-hook 'prog-mode-hook 'my-essential-modes)

;; toggle function for showing hard tab markers
(defun my-toggle-whitespace-mode ()
  "Toggle display of whitespace characters (tabs and trailing spaces)."
  (interactive)
  (whitespace-mode 'toggle)
  (if whitespace-mode
      (message "Whitespace mode enabled")
    (message "Whitespace mode disabled")))

(blink-cursor-mode -1)

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name-with-project)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-time-analogue-clock nil)
  (setq doom-modeline-modal nil)
  (setq doom-modeline-display-misc-in-all-mode-lines nil)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-check-simple-format t)
  (setq doom-modeline-lsp nil)
  ;; Disable file state checking to improve performance (saves ~5% CPU on redisplay)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)

  (doom-modeline-mode 1))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 6
           :header-line-width 2
           :mode-line-width 3
           :tab-width 2
           :right-divider-width 6
           :fringe-width 8))

  (spacious-padding-mode 1))

(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 160
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height 150)

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

;; better right-click
(when (display-graphic-p)
  (context-menu-mode))

(provide 'appearance)
;;; appearance.el ends here
