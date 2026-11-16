;;; core.el -*- lexical-binding: t; -*-
;; General settings, PATH, workspaces, terminal, and global minor modes.

;; --- basic behavior ---
(setq initial-major-mode 'fundamental-mode)
(setq help-window-select t)
(setq use-short-answers t)
(setq require-final-newline t)
(setq ring-bell-function #'ignore)

;; --- files: backups, auto-save, revert ---
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-check-vc-info t)
(setq auto-revert-interval 5)
(setq auto-save-visited-interval 5)

;; Follow symlinks into version-controlled files without prompting.
(setq vc-follow-symlinks t)

(setq backup-directory-alist
      `(("." . "~/.config/emacs/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-save-list/" t)))

;; --- encoding ---
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; --- PATH from the login shell (GUI apps on macOS don't inherit it) ---
(use-package exec-path-from-shell
  :demand t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; --- workspace management ---
(use-package perspective
  :custom
  (persp-suppress-no-prefix-key-warning t)
  :config
  (setq persp-state-default-file "~/.config/emacs/perspective-state")
  (setq persp-show-modestring nil)
  (persp-mode)
  ;; Loading the previous session reopens every buffer, so don't do it at
  ;; startup: that delays the first frame and replaces the scratch buffer.
  ;; Load it the first time a perspective command is used instead, and never
  ;; overwrite the saved state unless it was actually loaded.
  (defvar my/persp-state-loaded nil)

  (add-hook 'kill-emacs-hook
            (lambda ()
              (when my/persp-state-loaded
                (persp-state-save))))

  (defun my/persp-ensure-state (&rest _)
    "Load the saved perspective state once, on first perspective use."
    (unless my/persp-state-loaded
      (setq my/persp-state-loaded t)
      (when (file-exists-p persp-state-default-file)
        (let ((inhibit-message t))
          (persp-state-load persp-state-default-file)))))

  (advice-add 'persp-switch :before #'my/persp-ensure-state)
  (advice-add 'persp-switch-to-buffer :before #'my/persp-ensure-state)
  (advice-add 'persp-switch-to-buffer* :before #'my/persp-ensure-state))

;; --- project root helper (used by vterm) ---
(defun my/vterm-project-root ()
  "Return the current project root, or default-directory."
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

;; --- terminal ---
(use-package vterm
  :commands (vterm)
  :config
  (setq vterm-starting-directory #'my/vterm-project-root)
  (setq vterm-term-environment-variable "xterm-256color"))

(use-package vterm-toggle
  :commands (vterm-toggle)
  :config
  (setq vterm-toggle-scope 'project)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-max-scrollback 100000)

  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.4))))

;; --- which-key (built-in) ---
(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1))

;; --- global minor modes ---
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(display-time-mode 1)
(delete-selection-mode 1)
(auto-save-visited-mode 1)

;; --- smooth scrolling (macOS) ---
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(provide 'core)
;;; core.el ends here
