;; General editing customizations.

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Vim bindings.
(require 'evil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; evil-leader mode should be enabled before evil-mode otherwise
;; it won't load on initial buffers.
(global-evil-leader-mode)
(evil-mode 1)

(evil-leader/set-leader ",")

;; Default encoding.
(set-language-environment "UTF-8")

;; Company completion settings.
(require 'company)

(setq company-minimum-prefix-length 1)

(global-company-mode)

(define-key company-active-map (kbd "<tab>") 'company-select-next)
(define-key company-active-map (kbd "S-<tab>") 'company-select-previous)

;; Defines a key binding to use hippie expand for text completion.
(global-set-key (kbd "<tab>") 'hippie-expand)
(global-set-key (kbd "TAB") 'hippie-expand)

;; Lisp-friendly hippie expand.
(setq hippie-expand-try-functions-list
     '(try-expand-dabbrev
       try-expand-dabbrev-all-buffers
       try-expand-dabbrev-from-kill
       try-complete-lisp-symbol-partially
       try-complete-lisp-symbol))


;; Highlights matching parentheses.
(show-paren-mode 1)

;; Highlights current line.
(global-hl-line-mode 1)

;; Default tab width.
(setq-default tab-width 4)

;; Disable hard tabs.
(setq-default indent-tabs-mode nil)

(require 'saveplace)
(setq-default save-place t)

;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . , (concat
                                         user-emacs-directory "backups"))))
