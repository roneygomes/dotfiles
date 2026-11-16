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
        ;; C-i / C-o walk Evil's jumplist (like vim), so you can go back to
        ;; the dired buffer after visiting a file or directory from it.
        evil-want-C-i-jump t)
  :config
  (global-set-key (kbd "<escape>") #'keyboard-escape-quit)

  ;; Modes that keep Emacs bindings instead of evil's.
  (dolist (mode '(vterm-mode special-mode magit-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Dired uses evil's normal state (hjkl movement, dired keys on the
  ;; overriding map) so vim motions work in directory listings.
  (evil-set-initial-state 'dired-mode 'normal)

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

;; --- org: navigate screen lines (soft-wrapped text) ---
;; Org buffers enable `visual-line-mode' (see appearance.el), so a wrapped
;; logical line is displayed as several screen lines.  Evil's j/k always move
;; by logical line, so point them at screen-line motions instead, making each
;; wrapped segment behave like its own line.  The `screen-line' type keeps
;; operators consistent (dj/yj/cj act on whole screen lines, like Vim with
;; wrap on); `evil-respect-visual-line-mode' (set above) does the same for
;; dd/V.
(with-eval-after-load 'org
  (evil-define-motion my/evil-next-screen-line (count)
    "Move COUNT screen lines down."
    :type screen-line
    (let ((line-move-visual t))
      (evil-line-move (or count 1))))

  (evil-define-motion my/evil-previous-screen-line (count)
    "Move COUNT screen lines up."
    :type screen-line
    (let ((line-move-visual t))
      (evil-line-move (- (or count 1)))))

  (evil-define-key '(normal motion visual) org-mode-map
    (kbd "j") #'my/evil-next-screen-line
    (kbd "k") #'my/evil-previous-screen-line
    (kbd "<down>") #'my/evil-next-screen-line
    (kbd "<up>") #'my/evil-previous-screen-line
    (kbd "0") #'evil-beginning-of-visual-line
    (kbd "$") #'evil-end-of-visual-line
    (kbd "^") #'evil-first-non-blank-of-visual-line))

;; --- dired: always-editable listings with vim bindings (wdired) ---
;; Dired buffers open in wdired (editable) and stay in evil normal state, so
;; file and directory names can be edited with vim commands ("ce", "cw", ...).
;; ESC leaves insert state; ⌘-s applies the renames and stays editable;
;; RET/"o" open the entry in this window, "O" opens it in another window;
;; "q" closes the window.  `C-c C-c'/`C-x C-s' also commit but leave for the
;; read-only dired mode (re-enter editing with `C-x C-q').  Opening an entry
;; records a jump, so C-o returns to the listing and C-i goes forward again.
;; `u'/`C-r' undo/redo uncommitted text edits as usual.
(use-package wdired
  :ensure nil
  :config
  ;; Directory names are editable (default since Emacs 31); make sure a
  ;; rename into a new parent directory works without extra setup.
  (setq wdired-create-parent-directories t)
  ;; wdired enters during `dired-mode-hook', before dired-aux is normally
  ;; loaded, but it needs `dired-isearch-search-filenames' from there.
  (require 'dired-aux)

  (defvar my/wdired-suppress-auto nil
    "Non-nil while wdired itself is driving dired reverts.")

  (defun my/wdired-enter ()
    "Make a dired buffer editable, keeping evil's normal state."
    (when (and (eq major-mode 'dired-mode)
               (not my/wdired-suppress-auto))
      (let ((inhibit-message t))
        (wdired-change-to-wdired-mode))
      (evil-normal-state)))

  (defun my/wdired-save ()
    "Apply pending renames and remain in editable dired."
    (interactive)
    (if (not (buffer-modified-p))
        (message "No changes to apply")
      (wdired-finish-edit)
      (my/wdired-enter)))

  ;; Evil records jumps by file name (or real dired-mode) and reopens the
  ;; target with `find-file'.  A wdired buffer is neither, so identify it by
  ;; buffer name and let the jumplist treat that as a buffer target: jumping
  ;; back then switches to the very same editable dired buffer.
  (defun my/wdired-jump-file-name (orig)
    (or (funcall orig)
        (when (eq major-mode 'wdired-mode)
          (buffer-name))))
  (advice-add 'evil--jumps-current-file-name :around #'my/wdired-jump-file-name)
  ;; Buffer names never start with "/", while file names always do.
  (setq evil--jumps-buffer-targets
        (format "\\`\\(?:[^/]\\|%s\\)" evil--jumps-buffer-targets))

  (defun my/wdired-find-file (&optional other-window)
    "Apply pending renames, then visit the entry at point.
With OTHER-WINDOW, visit it in another window.  The dired position is
recorded in Evil's jumplist, so C-o returns to it."
    (interactive "P")
    (let ((file (dired-get-file-for-visit)))
      (when (buffer-modified-p)
        (my/wdired-save))
      (evil-set-jump)
      (if other-window
          (find-file-other-window file)
        (find-file file))))

  ;; Enter editable mode as soon as a directory listing has been read, and
  ;; not in the middle of wdired's own revert/commit passes.
  (add-hook 'dired-after-readin-hook #'my/wdired-enter)
  (dolist (fn '(wdired-finish-edit wdired-revert))
    (advice-add fn :around
                (lambda (orig &rest args)
                  (let ((my/wdired-suppress-auto t))
                    (apply orig args)))))

  ;; Evil state maps take precedence over the local map, so the dired keys
  ;; have to be installed as evil (normal state) bindings to win over
  ;; evil's own "o"/"q"/"RET" commands.
  (evil-define-key 'normal wdired-mode-map
    (kbd "s-s") #'my/wdired-save
    (kbd "RET") #'my/wdired-find-file
    (kbd "o") #'my/wdired-find-file
    (kbd "O") (lambda () (interactive) (my/wdired-find-file t))
    (kbd "q") #'quit-window)

  ;; Never let a plain save write the listing text over the directory.
  (advice-add 'save-buffer :before
              (lambda (&rest _)
                (when (eq major-mode 'wdired-mode)
                  (user-error "Use Cmd-s to apply renames")))))

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
