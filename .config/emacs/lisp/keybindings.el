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
(defun my/remember-project (project-root)
  "Add PROJECT-ROOT to the project list so project prompts offer it later.
Directories that are not a VCS project are remembered as transient
projects, which is what `project-switch-project' does for them."
  (project-remember-project
   (or (project-current nil project-root)
       (cons 'transient (file-name-as-directory (expand-file-name project-root))))))

(defun my/persp-switch-to-project (project-root)
  "Switch to PROJECT-ROOT's perspective, restoring its previous state.
Reuses the perspective's saved buffers and window layout, so you land
back on the file you had open last time.  A project visited for the very
first time opens its root directory instead of prompting for a file."
  (interactive (list (project-prompt-project-dir)))
  (my/remember-project project-root)
  ;; Make sure the saved perspectives are available before deciding whether
  ;; this project has been visited before.
  (my/persp-ensure-state)
  (let* ((persp-name (file-name-nondirectory (directory-file-name project-root)))
         (new-project (not (gethash persp-name (perspectives-hash)))))
    (persp-switch persp-name)
    (when new-project
      (dired project-root))))

(global-set-key (kbd "C-x C-p") #'persp-switch)
(global-set-key (kbd "s-M-p") #'my/persp-switch-to-project)

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
