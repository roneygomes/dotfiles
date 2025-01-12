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
  :ensure t
  :config
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

;; fuzzy matching
(use-package hotfuzz
  :ensure t
  :config
  (setq completion-styles '(hotfuzz)))

;; loads completions
;; (use-package corfu
;;   :ensure t
;;   :if (display-graphic-p)
;;   :hook
;;   (after-init . global-corfu-mode)
;;   (after-init . corfu-popupinfo-mode)

;;   :config
;;   (setq corfu-preview-current nil)
;;   (setq corfu-preselect 'first)
;;   (setq corfu-min-width 20)

;;   (setq corfu-popupinfo-delay '(0.25 . 0.25))
;;   (setq corfu-popupinfo-hide nil)

;;   ;; sort by input history
;;   (with-eval-after-load 'savehist
;;     (corfu-history-mode 1)
;;     (add-to-list 'savehist-additional-variables 'corfu-history)))

;; ;; (use-package cape
;;   :ensure t
;;   :init
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file))

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
