;;; config.el -*- lexical-binding: t; -*-
;; Personal Doom configuration. Evaluated after all modules load.

;; --- fonts ---
;; Iosevka Nerd Font (patched for Doom's icons) + Geneva for prose.
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16))
(setq doom-variable-pitch-font (font-spec :family "Geneva" :size 15))

;; --- window ---
;; Start maximized.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; --- theme ---
;; Keep the light modus-operandi theme from the old config.
;; (Swap for 'doom-one to go back to Doom's dark default.)
(setq doom-theme 'modus-operandi-tinted)

;; --- modeline ---
;; Disable icons in the modeline (text-only).
(setq doom-modeline-icon nil)

;; --- line numbers ---
(setq display-line-numbers-type 'relative)

;; --- clock in the modeline ---
(setq display-time-day-and-date t
      display-time-default-load-average nil
      display-time-format "%F %R")
(display-time-mode 1)

;; --- vterm scrollback ---
(setq vterm-max-scrollback 100000)

;; --- spacious padding (centered frame) ---
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

;; --- mixed-pitch in prose buffers ---
(use-package! mixed-pitch
  :hook ((org-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode))
  :config
  (setq mixed-pitch-set-height t))
