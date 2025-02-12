(setq mac-command-modifier 'super)

;; command palette
(global-set-key (kbd "s-p") 'execute-extended-command)

;; user interface
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; buffer management
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-e") 'consult-project-buffer)
(global-set-key (kbd "s-E") 'consult-buffer)

;; editing
(global-set-key (kbd "s-/") 'comment-line)

;; search
(global-set-key (kbd "s-f") 'consult-line)
(global-set-key (kbd "s-F") 'consult-git-grep)

;; projects
(global-set-key (kbd "s-O") 'find-file)
(global-set-key (kbd "s-o") 'project-find-file)
(global-set-key (kbd "s-P") 'project-switch-project)

;; file tree
(global-set-key (kbd "s-1") 'treemacs-select-window)

;; terminal
(defun my/new-vterm ()
  (interactive)
  (let ((vterm-buffer-name (generate-new-buffer-name "*vterm*")))
    (vterm vterm-buffer-name)))

(global-set-key (kbd "M-<f12>") 'vterm-toggle)
(global-set-key (kbd "M-S-<f12>") 'my/new-vterm)

(define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)

(global-set-key (kbd "s-<f12>") 'consult-outline)
(global-set-key (kbd "M-<f7>") 'xref-find-references)

(global-set-key (kbd "s-9") 'magit-status)

(with-eval-after-load 'restclient-mode
  (define-key restclient-mode-map (kbd "s-<return>") 'restclient-http-send-current-stay-in-window))

(with-eval-after-load 'company-mode
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "s-0") 'lsp-treemacs-errors-list)
  (define-key lsp-mode-map (kbd "s-2") 'lsp-treemacs-symbols)
  (define-key lsp-mode-map (kbd "C-M-h") 'lsp-treemacs-call-hierarchy)

  (define-key lsp-mode-map (kbd "s-S-<f12>") 'consult-lsp-symbols)
  (define-key lsp-mode-map (kbd "s-<f12>") 'consult-lsp-file-symbols)

  (define-key lsp-mode-map (kbd "s-j") 'eldoc-box-help-at-point)

  (define-key lsp-mode-map (kbd "s-b") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "s-B") 'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "M-<f7>") 'lsp-find-references)

  (define-key lsp-mode-map (kbd "s-M-l") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "S-<f6>") 'lsp-rename)

  (define-key lsp-mode-map (kbd "M-<return>") 'lsp-execute-code-action))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-fine-undo t)

  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)

  ;; remap evil's window management commands
  (define-key evil-normal-state-map (kbd "s-b") 'evil-goto-definition)
  (define-key evil-motion-state-map (kbd "C-w") nil)

  (define-prefix-command 'my-evil-window-map)
  (global-set-key (kbd "s-w") 'my-evil-window-map)

  (define-key my-evil-window-map (kbd "s") 'split-window-below)               ; ⌘-w s
  (define-key my-evil-window-map (kbd "v") 'split-window-right)               ; ⌘-w v
  (define-key my-evil-window-map (kbd "w") 'other-window)                     ; ⌘-w w
  (define-key my-evil-window-map (kbd "q") 'delete-window)                    ; ⌘-w c
  (define-key my-evil-window-map (kbd "h") 'evil-window-left)                 ; ⌘-w h
  (define-key my-evil-window-map (kbd "l") 'evil-window-right)                ; ⌘-w l
  (define-key my-evil-window-map (kbd "j") 'evil-window-down)                 ; ⌘-w j
  (define-key my-evil-window-map (kbd "k") 'evil-window-up)                   ; ⌘-w k
  (define-key my-evil-window-map (kbd "d") 'evil-window-delete)               ; ⌘-w d

  (define-key my-evil-window-map (kbd "r") 'evil-window-rotate-downwards)     ; ⌘-w r
  (define-key my-evil-window-map (kbd "R") 'evil-window-rotate-upwards)       ; ⌘-w R
  (define-key my-evil-window-map (kbd "H") 'evil-window-move-far-left)        ; ⌘-w H
  (define-key my-evil-window-map (kbd "L") 'evil-window-move-far-right)       ; ⌘-w L
  (define-key my-evil-window-map (kbd "J") 'evil-window-move-very-bottom)     ; ⌘-w J
  (define-key my-evil-window-map (kbd "K") 'evil-window-move-very-top)        ; ⌘-w K
  (define-key my-evil-window-map (kbd "o") 'delete-other-windows)             ; ⌘-w o

  (define-key evil-motion-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-insert-state-map (kbd "M-.") nil)
  (define-key evil-visual-state-map (kbd "M-.") nil)

  (evil-set-undo-system 'undo-redo)

  (evil-mode))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  ;; Highlights all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

  ;; Match with ⌘-d.
  (define-key evil-normal-state-map (kbd "s-d") 'evil-multiedit-match-and-next)
  (define-key evil-visual-state-map (kbd "s-d") 'evil-multiedit-match-and-next)
  (define-key evil-insert-state-map (kbd "s-d") 'evil-multiedit-toggle-marker-here)

  ;; Same as ⌘-d but in reverse.
  (define-key evil-normal-state-map (kbd "s-D") 'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "s-D") 'evil-multiedit-match-and-prev)

  ;; Restore last selection in visual mode.
  (define-key evil-visual-state-map (kbd "M-s-d") 'evil-multiedit-restore)

  ;; RET will toggle the region under the cursor.
  (define-key evil-multiedit-mode-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; ...and in visual mode, RET will disable all fields outside the selected region
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; Move between matches.
  (define-key evil-multiedit-mode-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-mode-map (kbd "C-p") 'evil-multiedit-prev))

(provide 'key-bindings)
