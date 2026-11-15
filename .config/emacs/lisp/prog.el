;;; prog.el -*- lexical-binding: t; -*-
;; Language servers, completion, and programming tooling.

;; --- ediff ---
(setq ediff-diff-options ""
      ediff-custom-diff-options "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-vertically)

;; --- eldoc ---
(setq eldoc-echo-area-use-multiline-p nil)

;; --- LSP ---
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l"
        lsp-modeline-diagnostics-scope :workspace
        lsp-headerline-breadcrumb-enable nil
        lsp-eldoc-render-all t)
  :hook ((go-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil)
  :commands lsp-ui-mode)

;; consult integration for LSP symbols and diagnostics
(use-package consult-lsp
  :after (consult lsp-mode))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; --- company (in-buffer completion) ---
(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-format-margin-function 'company-text-icons-margin
        company-icon-margin 3
        company-search-regexp-function 'company-search-flex-regexp)
  (add-to-list 'company-backends 'company-restclient)
  (add-hook 'after-init-hook #'global-company-mode))

;; --- treemacs (file tree) ---
(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 40
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-no-delete-other-windows nil
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-no-png-images t
        treemacs--project-follow-delay 0.5)

  (set-face-attribute 'treemacs-root-face nil :height 1.0 :font "Geneva-18" :weight 'bold)
  (set-face-attribute 'treemacs-file-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-directory-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-directory-collapsed-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-git-modified-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-git-ignored-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-git-added-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-git-renamed-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-git-conflict-face nil :height 1.0 :font "Geneva-14")
  (set-face-attribute 'treemacs-git-untracked-face nil :height 1.0 :font "Geneva-14")

  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-project-follow-mode))

(use-package treemacs-evil :after treemacs)
(use-package lsp-treemacs :after (lsp-mode treemacs))

;; --- LSP documentation popups ---
(use-package eldoc-box
  :defer t
  :config
  (setq eldoc-box-max-pixel-width 1280
        eldoc-box-max-pixel-height 720))

;; --- formatting ---
(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :config
  (setq apheleia-formatters-respect-indent-level nil))

;; elisp: spaces, not tabs
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(provide 'prog)
;;; prog.el ends here
