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
(global-set-key (kbd "s-e") 'consult-buffer)         ; show all buffers
(global-set-key (kbd "s-E") 'consult-project-buffer) ; show only project buffers

;; editing
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-l") 'consult-goto-line)

;; search
(global-set-key (kbd "s-f") 'consult-line)
(global-set-key (kbd "s-F") #'deadgrep)

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

;; programming
(define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)

(global-set-key (kbd "s-<f12>") 'consult-imenu)

(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "<tab>") 'corfu-complete))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "s-0") 'consult-flymake)

  (define-key eglot-mode-map (kbd "s-j") 'eldoc-box-help-at-point)
  (define-key eglot-mode-map (kbd "s-J") 'eldoc)

  (define-key eglot-mode-map (kbd "s-b") 'evil-goto-definition)
  (define-key eglot-mode-map (kbd "s-B") 'eglot-find-implementation)

  (define-key eglot-mode-map (kbd "s-M-l") 'eglot-format-buffer)

  (define-key eglot-mode-map (kbd "S-<f6>") 'eglot-rename)

  (define-key eglot-mode-map (kbd "M-<return>") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "M-<f7>") 'xref-find-references))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-fine-undo t)

  :config
  (evil-set-undo-system 'undo-redo)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'flymake-diagnostics-buffer-mode 'emacs)
  (evil-set-initial-state 'flymake-project-diagnostics-mode 'emacs)
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  (evil-mode)

  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-n") 'next-line)
    (define-key evil-insert-state-map (kbd "C-p") 'previous-line)

    ;; unbind the default C-w prefix key
    (define-key evil-motion-state-map (kbd "C-w") nil)

    ;; define the ⌘-w prefix key
    (define-prefix-command 'my-evil-window-map)
    (global-set-key (kbd "s-w") 'my-evil-window-map)

    ;; remap evil's window management commands
    (define-key my-evil-window-map (kbd "s") 'split-window-below)               ; ⌘-w s
    (define-key my-evil-window-map (kbd "v") 'split-window-right)               ; ⌘-w v
    (define-key my-evil-window-map (kbd "w") 'other-window)                     ; ⌘-w w
    (define-key my-evil-window-map (kbd "c") 'delete-window)                    ; ⌘-w c
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
    (define-key evil-visual-state-map (kbd "M-.") nil)))

(provide 'key-bindings)
