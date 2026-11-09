;; completions ui layout
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)

  (vertico-multiform-mode 1)
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package consult
  :ensure t
  :after perspective
  :config
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

;; fuzzy matching
(use-package hotfuzz
  :ensure t
  :config
  (setq completion-styles '(hotfuzz)))

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)                              ; use the minibuffer whilst in the minibuffer
  (completion-cycle-threshold 1)                                ; tab cycles candidates
  (tab-always-indent 'complete)                                 ; when I hit tab, try to complete, otherwise, indent
  (tab-first-completion 'eol)

  (completion-auto-help 'always)                                ; open completion always; `lazy' another option
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete))  ; TAB acts more like how it does in the shell

(provide 'completion-config)
;;; completion-config.el ends here
