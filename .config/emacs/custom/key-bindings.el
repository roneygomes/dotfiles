(setq mac-command-modifier 'super)

;; command palette
(global-set-key (kbd "s-p") 'execute-extended-command)

;; user interface
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; buffer management
(defvar my/consult-source-beframe-buffers
  `(:name     "Frame Buffers"
    :narrow   ?b
    :category buffer
    :items    ,(lambda () (mapcar #'buffer-name (beframe-buffer-list)))
    :action   consult--buffer-action
    :state    consult--buffer-state))

(defun my/consult-buffer-beframe ()
  "Consult-buffer showing only frame-local buffer names."
  (interactive)
  (let ((consult-buffer-sources '(my/consult-source-beframe-buffers)))
    (consult-buffer)))

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-e") 'my/consult-buffer-beframe)

;; editing
(global-set-key (kbd "s-/") 'comment-line)

;; search
(global-set-key (kbd "s-f") 'consult-line)
(global-set-key (kbd "s-F") #'deadgrep)

;; projects
(defun my/project-switch ()
  "Switch to project in a new dedicated frame."
  (interactive)
  (let* ((dir (project-prompt-project-dir))
         ;; get project name from the last directory component
         (proj-name (file-name-nondirectory (directory-file-name dir)))
         (new-frame (make-frame `((title . ,proj-name)))))
    (with-selected-frame new-frame
      (project-switch-project dir)
      ;; update the title when switching projects within this frame
      (add-hook 'project-switch-hook
                (lambda ()
                  ;; recompute the project name here to get the current project
                  (let ((new-name (file-name-nondirectory
                                   (directory-file-name (car (project-roots (project-current)))))))
                    (modify-frame-parameters (selected-frame)
                                             `((title . ,new-name)))))
                ;; t makes the hook buffer-local
                nil t))))

(global-set-key (kbd "s-O") 'find-file)
(global-set-key (kbd "s-o") 'project-find-file)
(global-set-key (kbd "s-P") 'my/project-switch)

;; file tree
(defun my/neotree ()
  "Toggle neotree at the project's root directory"
  (interactive)
  (let ((dir (project-root (project-current))))
    (if (neo-global--window-exists-p)
      (neotree-hide)
    (neotree-dir dir))))

(global-set-key (kbd "s-1") 'my/neotree)

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
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)

  (evil-set-undo-system 'undo-redo)

  (evil-mode)

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "s-b") 'evil-goto-definition)

    ;; unbind the default C-w prefix key
    (define-key evil-motion-state-map (kbd "C-w") nil)

    ;; define the ⌘-w prefix key
    (define-prefix-command 'my-evil-window-map)
    (global-set-key (kbd "s-w") 'my-evil-window-map)

    ;; remap evil's window management commands
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
    (define-key evil-visual-state-map (kbd "M-.") nil)))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(provide 'key-bindings)
