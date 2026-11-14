;;; config.el -*- lexical-binding: t; -*-
;; Personal Doom configuration. Evaluated after all modules load.

;; --- fonts ---
;; Iosevka Nerd Font (patched for Doom's icons) + Geneva for prose.
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16))
(setq doom-variable-pitch-font (font-spec :family "Geneva" :size 15))

;; --- window ---
;; Start maximized.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; --- cursor ---
;; Thin "pipe" cursor instead of a block.
(setq-default cursor-type 'bar)

;; --- theme ---
;; Keep the light modus-operandi theme from the old config.
;; (Swap for 'doom-one to go back to Doom's dark default.)
(setq doom-theme 'modus-operandi-tinted)

;; --- modeline ---
;; Disable icons in the modeline (text-only).
(setq doom-modeline-icon nil
      doom-modeline-time-icon nil)

;; --- treemacs (file tree) ---
;; No icons: disable the nerd-glyph theme and PNG images, leaving a text-only
;; tree (just +/- collapse indicators).
(setq treemacs-no-png-images t)

;; --- company completion popup ---
;; `+childframe' (company-box) was removed from the company module in init.el:
;; its childframe popup rendered as a stray "$" on macOS. With plain `company',
;; the in-buffer overlay tooltip is used instead (no childframe, no icons).

;; --- line numbers ---
(setq display-line-numbers-type t)  ; absolute (relative recomputes on every change)

;; No line numbers in org-mode (prose, not code).
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; --- clock in the modeline ---
(setq display-time-day-and-date t
      display-time-default-load-average nil
      display-time-format "%F %R")
(display-time-mode 1)

;; --- vterm scrollback ---
(setq vterm-max-scrollback 100000)

;; --- spacious padding (centered frame) ---
;; Re-added without mixed-pitch, and with absolute line numbers, to stay
;; under the redisplay budget. Remove if typing gets slow again.
(use-package! spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 6
          :header-line-width 2
          :mode-line-width 3
          :tab-width 2
          :right-divider-width 6
          :fringe-width 8))
  (spacious-padding-mode 1))

;; --- new project helper ---
(defun my/project-new (dir)
  "Create DIR as a new project: mkdir -p, git init, register it with
projectile, and switch to it (opening a new workspace)."
  (interactive (list (expand-file-name (read-file-name "Create project: " "~/projects/"))))
  (make-directory dir t)
  (let ((default-directory dir))
    (unless (file-directory-p ".git")
      (call-process "git" nil nil nil "init")))
  (projectile-add-known-project dir)
  (projectile-switch-project-by-name dir)
  (message "Created project %s" dir))

(map! :leader
      :desc "New project" "p n" #'my/project-new)

;; --- sync & restart (stays inside Emacs) ---
;; `doom/reload' is unreliable for module changes: it re-boots in-process and can
;; hit "Memory exhausted". This runs a real `doom sync --aot', then hard-restarts
;; Emacs in place via `doom/restart' — the same reliable path as a terminal,
;; without ever leaving Emacs.
(defun my/doom-sync-and-restart ()
  "Run `doom sync --aot -j 4', then restart Emacs when it finishes."
  (interactive)
  (let* ((default-directory doom-emacs-dir)
         (doom-bin (expand-file-name "bin/doom" doom-emacs-dir))
         (emacs-bin (expand-file-name invocation-name invocation-directory))
         buf)
    (let ((process-environment
           (cons (format "EMACS=%s" emacs-bin)
                 (cons (format "EMACSDIR=%s" doom-emacs-dir)
                       (cons (format "DOOMDIR=%s" doom-user-dir)
                             (cons (format "DOOMLOCALDIR=%s" doom-local-dir)
                                   process-environment))))))
      (setq buf (compile (format "%s sync --aot -j 4" doom-bin) t)))
    (with-current-buffer buf
      (add-hook 'compilation-finish-functions
                (lambda (_buf status)
                  (if (string-prefix-p "finished" status)
                      (progn (message "doom sync done — restarting Emacs…")
                             (sit-for 0.3)
                             (doom/restart))
                    (message "doom sync FAILED (%s) — not restarting"
                             (string-trim status))))
                nil 'local))))

(map! :leader
      :desc "Sync & restart" "h r s" #'my/doom-sync-and-restart)
