(setq-default line-spacing 0.2)

(setq inhibit-splash-screen t)

(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(setq display-time-format "%F %R")

(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

(setq display-line-numbers-type 'relative)

;; use a face for displaying trailing whitespace and hard tabs
(setq whitespace-style '(face tabs trailing tab-mark))

;; for anything text editing we should have these essential modes
(defun my-essential-modes ()
  (whitespace-mode 1)
  (column-number-mode 1)
  (electric-pair-mode 1))

(add-hook 'text-mode-hook 'my-essential-modes)
(add-hook 'prog-mode-hook 'my-essential-modes)

(blink-cursor-mode -1)

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode 1))

(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode 1))

(set-face-attribute 'default nil
  :font "Iosevka Comfy"
  :height 160
  :weight 'regular)

(set-face-attribute 'fixed-pitch nil
  :font "Iosevka Comfy"
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

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package treemacs-all-the-icons
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-completion
  :ensure t
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; better right-click
(when (display-graphic-p)
  (context-menu-mode))

(provide 'appearance)
;;; appearance.el ends here
