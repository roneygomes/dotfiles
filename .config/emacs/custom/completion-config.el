;; completions ui layout
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)

  (vertico-multiform-mode 1)
  (vertico-mode 1))

;; displays additional info for completions on the minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package consult
  :ensure t)

;; fuzzy matching
(use-package hotfuzz
  :ensure t
  :config
  (setq completion-styles '(hotfuzz)))

;; loads completions
(use-package corfu
  :ensure t
  :if (display-graphic-p)
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-popupinfo-mode)

  :config
  (setq corfu-preview-current nil)
  (setq corfu-preselect 'first)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(0.25 . 0.25))
  (setq corfu-popupinfo-hide nil)

  ;; sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(defun my-eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file))))

(use-package cape
  :ensure t
  :init
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)                              ; use the minibuffer whilst in the minibuffer
  (completion-cycle-threshold 1)                                ; tab cycles candidates
  (tab-always-indent 'complete)                                 ; when I hit tab, try to complete, otherwise, indent

  (completion-auto-help 'always)                                ; open completion always; `lazy' another option
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)   ; TAB acts more like how it does in the shell

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'completion-config)
