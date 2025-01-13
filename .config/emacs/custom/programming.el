(setq read-process-output-max (* 1024 1024))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "s-l")
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-headerline-breadcrumb-enable nil)

  :hook
  ((go-ts-mode . lsp)
   (rust-ts-mode . lsp))

  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-mode-hook #'flycheck-mode)

  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-border "black")
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 0)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)

  (custom-set-faces
   '(lsp-ui-doc-background ((t (:background nil :inherit default)))))

  :commands lsp-ui-mode)

(use-package consult-lsp
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-format-margin-function 'company-text-icons-margin)
  (setq company-icon-margin 3)
  (setq company-search-regexp-function 'company-search-flex-regexp)

  (add-hook 'after-init-hook 'global-company-mode))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc"))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-width 45)
  (setq treemacs-select-when-already-in-treemacs 'close)
  (setq treemacs-no-delete-other-windows nil)

  (treemacs-hide-gitignored-files-mode t)
  (treemacs-project-follow-mode))

(use-package treemacs-evil
  :ensure t)

(use-package lsp-treemacs
  :ensure t)

(provide 'programming)
;;; programming.el ends here
