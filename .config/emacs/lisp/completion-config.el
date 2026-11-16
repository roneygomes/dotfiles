;;; completion-config.el -*- lexical-binding: t; -*-
;; Minibuffer completion: vertico + marginalia + consult + hotfuzz.

(use-package vertico
  :config
  (setq vertico-cycle t
        vertico-resize nil)
  (vertico-multiform-mode 1)
  (vertico-mode 1))

;; --- evil bindings in vertico minibuffers ---
;; Completion minibuffers (vertico/consult, the project switcher, M-x, ...)
;; open in evil normal state: j/k move through candidates, gg/G jump to the
;; first/last, C-d/C-u scroll, RET accepts; press i (or a) to type, ESC goes
;; back to normal.  Non-completion prompts (read-string, y-or-n-p) are left
;; alone.
(defun my/minibuffer-enter-normal ()
  "Switch the minibuffer to evil normal state, keeping the input at point-max."
  (interactive)
  (evil-normal-state)
  (goto-char (point-max)))

(defun my/minibuffer-evil-normal ()
  "Enable evil in completion minibuffers and start in normal state."
  (when (and (minibufferp) minibuffer-completion-table)
    (unless evil-local-mode
      (evil-local-mode 1))
    ;; Vertico only matches up to point, so point must stay at the end of the
    ;; input instead of being pulled back the way evil normal state does.
    (setq-local evil-move-beyond-eol t)
    (my/minibuffer-enter-normal)
    (add-hook 'pre-command-hook
              (lambda ()
                (when (and (minibufferp) (evil-normal-state-p) (not (eobp)))
                  (goto-char (point-max))))
              nil t)))

(with-eval-after-load 'evil
  (add-hook 'minibuffer-setup-hook #'my/minibuffer-evil-normal)
  (evil-define-key 'normal vertico-map
    (kbd "j") #'vertico-next
    (kbd "k") #'vertico-previous
    (kbd "g g") #'vertico-first
    (kbd "G") #'vertico-last
    (kbd "C-d") #'vertico-scroll-down
    (kbd "C-u") #'vertico-scroll-up
    (kbd "RET") #'vertico-exit
    (kbd "o") #'vertico-exit)
  (evil-define-key 'insert vertico-map
    (kbd "<escape>") #'my/minibuffer-enter-normal))

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
