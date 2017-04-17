;; Define package repositories.
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
	     '("elpa" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

; Update local index.
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Define packages to install.
(defvar my-packages
  '(
        ;; Vim goodness on Emacs.
        evil
        evil-leader
        powerline
        powerline-evil

        ;; Improved handling of list expressions.
        paredit

        ;; Clojure
        cider
        clojure-mode
        rainbow-delimiters
        clojure-mode-extra-font-locking

        ;; Smart completion.
        company
        ;; Helm is awesome.
        helm))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (unless (package-installed-p p)
	(package-install p)))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Customizations for Emacs look-and-feel. 
(load "ui.el")

;; General editing customizations.
(load "editing.el")

;; Navigation configs, mostly helm and evil-leader.
(load "navigation.el")

;; Clojure configs.
(load "clojure.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company relative-line-numbers evil-visual-mark-mode powerline-evil evil-leader use-package helm evil))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
