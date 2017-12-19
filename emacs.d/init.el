;; auto generated stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(package-selected-packages
   (quote
    (counsel swiper evil ivy which-key use-package rainbow-delimiters powerline-evil paredit helm evil-leader company color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ui customizations.
(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-frame-font "D2 Coding Bold 12" nil t)

;; Packages.
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (load-theme 'sanityinc-tomorrow-bright))

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume))
  :config (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
	  (setq enable-recursive-minibuffer t))

(use-package swiper
  :ensure t
  :bind ("\C-s" . swiper))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate)))

(use-package company
  :ensure t
  :config (global-company-mode)
          (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))
