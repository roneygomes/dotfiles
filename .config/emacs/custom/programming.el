(setq read-process-output-max (* 1024 1024))
(setq eldoc-echo-area-use-multiline-p nil)

(setq ediff-diff-options "")
(setq ediff-custom-diff-options "-u")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)

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
  (setq lsp-eldoc-render-all t)

  :hook
  ((go-ts-mode . lsp-deferred)
   (rust-ts-mode . lsp-deferred))

  :config
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports)

  :commands lsp)

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :config
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)

  :commands lsp-ui-mode)

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode))

(use-package flycheck
  :ensure t
  :hook (lsp-mode . flycheck-mode))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup)
  :after (flycheck))

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

  (add-to-list 'company-backends 'company-restclient)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package restclient
  :ensure t)

(use-package company-restclient
  :ensure t
  :after (restclient company))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc"))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-width 40)
  (setq treemacs-select-when-already-in-treemacs 'move-back)
  (setq treemacs-no-delete-other-windows nil)

  (setq treemacs-sorting 'alphabetic-case-insensitive-asc)

  (set-face-attribute 'treemacs-root-face nil :height 1.0 :font "Avenir Next-18" :weight 'bold)
  (set-face-attribute 'treemacs-file-face nil :height 1.0 :font "Avenir Next-15")

  (set-face-attribute 'treemacs-directory-face nil :height 1.0 :font "Avenir Next-15" :weight 'medium)
  (set-face-attribute 'treemacs-directory-collapsed-face nil :height 1.0 :font "Avenir Next-15" :weight 'medium)

  (treemacs-git-mode -1)
  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-project-follow-mode))

(use-package treemacs-evil
  :ensure t
  :after (treemacs))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs))

(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :after magit
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (diff-hl-margin-mode 1)
  (global-diff-hl-mode 1))

(use-package eldoc-box
  :ensure t
  :config
  (setq eldoc-box-max-pixel-width 1280)
  (setq eldoc-box-max-pixel-height 720))

(use-package beframe
  :ensure t
  :config
  (beframe-mode 1))

(use-package terraform-mode
  :ensure t
  :custom (terraform-indent-level 4)
  :config
  (defun my/terraform-mode-init ()
    (outline-minor-mode 1))

  (add-hook 'terraform-mode-hook 'my/terraform-mode-init))

(provide 'programming)
;;; programming.el ends here
