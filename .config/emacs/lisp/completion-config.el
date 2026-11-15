;;; completion-config.el -*- lexical-binding: t; -*-
;; Minibuffer completion: vertico + marginalia + consult + hotfuzz.

(use-package vertico
  :config
  (setq vertico-cycle t
        vertico-resize nil)
  (vertico-multiform-mode 1)
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :after perspective
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize consult-source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

;; fuzzy matching
(use-package hotfuzz
  :config
  (setq completion-styles '(hotfuzz)))

;; Built-in completion tweaks.
(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (tab-first-completion 'eol)
  (completion-auto-help 'always)
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)
  :config
  ;; TAB completes like it does in the shell.
  (keymap-set minibuffer-mode-map "TAB" #'minibuffer-complete))

(provide 'completion-config)
;;; completion-config.el ends here
