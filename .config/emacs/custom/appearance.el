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
  (display-line-numbers-mode 1)
  (whitespace-mode 1)
  (column-number-mode 1)
  (electric-pair-mode 1))

(add-hook 'text-mode-hook 'my-essential-modes)
(add-hook 'prog-mode-hook 'my-essential-modes)

(blink-cursor-mode -1)

(use-package almost-mono-themes
  :ensure t
  :config
  (load-theme 'almost-mono-white))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode 1))

(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode 1))

(custom-set-faces
 '(font-lock-comment-face ((t (:italic nil))))
 '(whitespace-tab ((t (:foreground "#BDBDBD" :background nil)))))

(set-face-attribute 'default nil
  :font "Monaco"
  :height 140
  :weight 'regular)

(set-face-attribute 'fixed-pitch nil
  :font "Monaco"
  :height 130)

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
