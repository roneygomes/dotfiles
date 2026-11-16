;;; editing.el -*- lexical-binding: t; -*-
;; Modal editing (evil), multi-cursor, undo, and privileged files.

;; --- undo: undo-fu with cross-session persistence ---
(use-package undo-fu
  :demand t)

(use-package undo-fu-session
  :demand t
  :custom
  (undo-fu-session-directory (locate-user-emacs-file "undo-fu-session/"))
  :config
  (undo-fu-session-global-mode 1))

;; --- evil (vim bindings) ---
(use-package evil
  :demand t
  :init
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-fine-undo t
        evil-undo-system 'undo-fu
        evil-auto-indent t
        evil-respect-visual-line-mode t
        evil-disable-insert-state-bindings t
        evil-want-C-u-scroll nil
        evil-want-C-i-jump nil)
  :config
  (global-set-key (kbd "<escape>") #'keyboard-escape-quit)

  ;; Modes that keep Emacs bindings instead of evil's.
  (dolist (mode '(vterm-mode treemacs-mode dired-mode special-mode magit-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Jump to definition.
  (define-key evil-normal-state-map (kbd "s-b") #'evil-goto-definition)

  ;; Window management: vim's C-w prefix (C-w h/j/k/l ...) in normal,
  ;; visual, and operator states, plus a ⌘-w alternative on macOS.
  (define-prefix-command 'my-evil-window-map)
  (global-set-key (kbd "s-w") 'my-evil-window-map)

  (define-key my-evil-window-map (kbd "s") #'split-window-below)
  (define-key my-evil-window-map (kbd "v") #'split-window-right)
  (define-key my-evil-window-map (kbd "w") #'other-window)
  (define-key my-evil-window-map (kbd "q") #'delete-window)
  (define-key my-evil-window-map (kbd "h") #'evil-window-left)
  (define-key my-evil-window-map (kbd "l") #'evil-window-right)
  (define-key my-evil-window-map (kbd "j") #'evil-window-down)
  (define-key my-evil-window-map (kbd "k") #'evil-window-up)
  (define-key my-evil-window-map (kbd "d") #'evil-window-delete)
  (define-key my-evil-window-map (kbd "r") #'evil-window-rotate-downwards)
  (define-key my-evil-window-map (kbd "R") #'evil-window-rotate-upwards)
  (define-key my-evil-window-map (kbd "H") #'evil-window-move-far-left)
  (define-key my-evil-window-map (kbd "L") #'evil-window-move-far-right)
  (define-key my-evil-window-map (kbd "J") #'evil-window-move-very-bottom)
  (define-key my-evil-window-map (kbd "K") #'evil-window-move-very-top)
  (define-key my-evil-window-map (kbd "o") #'delete-other-windows)

  ;; Free M-. for other uses.
  (define-key evil-motion-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-insert-state-map (kbd "M-.") nil)
  (define-key evil-visual-state-map (kbd "M-.") nil)

  (evil-mode))

;; --- multi-cursor editing ---
(use-package evil-multiedit
  :after evil
  :config
  ;; Highlight all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" #'evil-multiedit-match-all)

  ;; Match with ⌘-d.
  (define-key evil-normal-state-map (kbd "s-d") #'evil-multiedit-match-and-next)
  (define-key evil-visual-state-map (kbd "s-d") #'evil-multiedit-match-and-next)
  (define-key evil-insert-state-map (kbd "s-d") #'evil-multiedit-toggle-marker-here)

  ;; Same as ⌘-d but in reverse.
  (define-key evil-normal-state-map (kbd "s-D") #'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "s-D") #'evil-multiedit-match-and-prev)

  ;; Restore last selection in visual mode.
  (define-key evil-visual-state-map (kbd "M-s-d") #'evil-multiedit-restore)

  ;; RET toggles the region under the cursor.
  (define-key evil-multiedit-mode-map (kbd "RET") #'evil-multiedit-toggle-or-restrict-region)
  (define-key evil-motion-state-map (kbd "RET") #'evil-multiedit-toggle-or-restrict-region)

  ;; Move between matches.
  (define-key evil-multiedit-mode-map (kbd "C-n") #'evil-multiedit-next)
  (define-key evil-multiedit-mode-map (kbd "C-p") #'evil-multiedit-prev))

;; --- edit privileged files ---
(use-package sudo-edit
  :defer t)

(provide 'editing)
;;; editing.el ends here
