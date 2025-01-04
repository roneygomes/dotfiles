(setq read-process-output-max (* 1024 1024))
(setq help-window-select t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :ensure t
  :demand t
  :hook
  (rust-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (go-ts-mode . go-format-on-save-mode)

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate eglot in referenced non-project files

  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package eldoc
  :ensure t
  :custom (eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box
  :ensure t)

(use-package reformatter
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package go-ts-mode
  :config
  (reformatter-define go-format
    :program "goimports"
    :args '("/dev/stdin")))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 45)
  (setq treemacs-select-when-already-in-treemacs 'close)
  (setq treemacs-no-delete-other-windows nil)
  (setq treemacs-no-png-images t)

  (treemacs-hide-gitignored-files-mode t)
  (treemacs-project-follow-mode))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(provide 'programming)
