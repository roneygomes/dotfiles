;;; keybindings.el -*- lexical-binding: t; -*-
;; Global key bindings. Package configuration lives in the other modules.

(global-set-key (kbd "s-P") #'execute-extended-command)
(global-set-key (kbd "s-p") #'project-find-file)

(global-set-key (kbd "s-o") #'find-file)
(global-set-key (kbd "s-O") #'consult-outline)

(global-set-key (kbd "s-=") #'text-scale-increase)
(global-set-key (kbd "s--") #'text-scale-decrease)

(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-e") #'persp-switch-to-buffer*)
(global-set-key (kbd "s-E") #'persp-switch-to-buffer)

(global-set-key (kbd "s-/") #'comment-line)

(global-set-key (kbd "s-f") #'consult-line)
(global-set-key (kbd "s-F") #'consult-git-grep)

;; --- project switching (perspective) ---
(defun my/persp-switch-to-project (project-root)
  (interactive (list (project-prompt-project-dir)))
  (let ((project-name (file-name-nondirectory (directory-file-name project-root))))
    (if (gethash project-name (perspectives-hash))
        (persp-switch project-name)
      (progn
        (persp-switch project-name)
        (project-switch-project project-root)))))

(global-set-key (kbd "C-x C-p") #'persp-switch)
(global-set-key (kbd "C-M-p") #'my/persp-switch-to-project)

;; --- file tree ---
(global-set-key (kbd "s-1") #'treemacs-select-window)

;; --- terminal ---
(defun my/new-vterm ()
  (interactive)
  (let ((vterm-buffer-name (generate-new-buffer-name "*vterm*")))
    (vterm vterm-buffer-name)))

(global-set-key (kbd "s-j") #'vterm-toggle)
(global-set-key (kbd "s-J") #'my/new-vterm)

;; --- lisp eval ---
(define-key emacs-lisp-mode-map (kbd "s-<return>") #'eval-last-sexp)
(define-key lisp-mode-map (kbd "s-<return>") #'eval-last-sexp)

;; --- git ---
(global-set-key (kbd "s-9") #'magit-status)

(global-set-key (kbd "S-<f12>") #'xref-find-references)

;; --- per-mode keybindings (applied when the package loads) ---
(with-eval-after-load 'restclient-mode
  (define-key restclient-mode-map (kbd "s-<return>") #'restclient-http-send-current-stay-in-window))

(with-eval-after-load 'company-mode
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "s-0") #'lsp-treemacs-errors-list)
  (define-key lsp-mode-map (kbd "s-2") #'lsp-treemacs-symbols)
  (define-key lsp-mode-map (kbd "C-M-h") #'lsp-treemacs-call-hierarchy)

  (define-key lsp-mode-map (kbd "C-M-o") #'consult-lsp-symbols)
  (define-key lsp-mode-map (kbd "s-O") #'consult-lsp-file-symbols)

  (define-key lsp-mode-map (kbd "C-j") #'eldoc-box-help-at-point)

  (define-key lsp-mode-map (kbd "s-b") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "s-B") #'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "M-<f7>") #'lsp-find-references)

  (define-key lsp-mode-map (kbd "s-M-l") #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "S-<f6>") #'lsp-rename)

  (define-key lsp-mode-map (kbd "M-<return>") #'lsp-execute-code-action))

(provide 'keybindings)
;;; keybindings.el ends here
