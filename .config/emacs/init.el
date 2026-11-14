;;; init.el -*- lexical-binding: t; -*-
;; Minimal Emacs configuration.

;; ---------------------------------------------------------------------------
;; Package management
;; ---------------------------------------------------------------------------

(package-initialize)
(setq use-package-always-ensure t)  ; auto-install packages in use-package blocks

;; Restore performance settings after startup finishes.
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; ---------------------------------------------------------------------------
;; Environment
;; ---------------------------------------------------------------------------

;; GUI apps on macOS don't inherit the login shell's PATH.
(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; ---------------------------------------------------------------------------
;; Appearance
;; ---------------------------------------------------------------------------

(load-theme 'modus-operandi-tinted t)  ; light, high-contrast
(setq inhibit-startup-screen t)

;; Minimal UI: no scroll bar, tool bar, or menu bar.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Iosevka for code, Geneva for prose.
(set-face-attribute 'default nil :family "Iosevka" :height 160)
(set-face-attribute 'variable-pitch nil :family "Geneva" :height 150)

;; Thin "pipe" cursor instead of a block.
(setq-default cursor-type 'bar)

;; Absolute line numbers in code, none in org (prose).
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Roomy, centered frame.
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

;; ---------------------------------------------------------------------------
;; Editing basics
;; ---------------------------------------------------------------------------

(setq-default tab-width 4
              indent-tabs-mode nil)
(setq require-final-newline t)
(setq ring-bell-function #'ignore)

(electric-pair-mode 1)        ; auto-close (), [], {}, ""
(delete-selection-mode 1)     ; typing over a selection replaces it
(show-paren-mode 1)           ; highlight matching paren
(global-auto-revert-mode 1)   ; reload files changed on disk
(savehist-mode 1)             ; persist minibuffer history
(save-place-mode 1)           ; remember point per file
(recentf-mode 1)              ; recent files
(column-number-mode 1)
(display-time-mode 1)         ; clock in modeline

;; Keep backups/auto-saves out of the working tree.
(setq backup-directory-alist '(("." . "~/.config/emacs/backups"))
      auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t))
      create-lockfiles nil)

;; Smooth scrolling (macOS).
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; ---------------------------------------------------------------------------
;; Completion
;; ---------------------------------------------------------------------------

;; Minibuffer: vertico + orderless + marginalia + consult.
(use-package vertico
  :config
  (vertico-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; In-buffer: corfu + cape.
(use-package corfu
  :config
  (global-corfu-mode 1))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; TAB completes in the minibuffer.
(keymap-set minibuffer-mode-map "TAB" #'minibuffer-complete)

;; ---------------------------------------------------------------------------
;; LSP (eglot, built-in)
;; ---------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :hook ((go-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  (setq eglot-report-progress nil)
  ;; Format the buffer on save, when the server supports it.
  (add-hook 'before-save-hook
            (lambda ()
              (when (bound-and-true-p eglot-managed-mode)
                (ignore-errors (eglot-format-buffer))))))

;; ---------------------------------------------------------------------------
;; Tree-sitter
;; ---------------------------------------------------------------------------

;; Remap built-in modes to their tree-sitter equivalents and install the
;; grammars on demand.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        '(bash go rust typescript tsx javascript yaml json python toml dockerfile))
  (dolist (lang treesit-auto-langs)
    (treesit-auto-add-to-auto-mode-alist lang)))

;; ---------------------------------------------------------------------------
;; Languages
;; ---------------------------------------------------------------------------

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . visual-line-mode))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode))

;; Org: plain prose.
(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  (org-hide-emphasis-markers t))

;; ---------------------------------------------------------------------------
;; Git
;; ---------------------------------------------------------------------------

(use-package magit
  :bind ("C-x g" . magit-status))

;; ---------------------------------------------------------------------------
;; Terminal
;; ---------------------------------------------------------------------------

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 100000))

;; ---------------------------------------------------------------------------
;; Custom settings last (so user customizations win)
;; ---------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;; init.el ends here
