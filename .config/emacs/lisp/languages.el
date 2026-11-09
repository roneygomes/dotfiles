;;; languages.el -*- lexical-binding: t; -*-
;; Language-specific modes and tree-sitter.

;; --- tree-sitter (statically mapped, avoids slow dynamic remapping) ---
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(bash go rust typescript tsx javascript yaml json python sql toml dockerfile))
  (dolist (lang treesit-auto-langs)
    (treesit-auto-add-to-auto-mode-alist lang)))

;; --- HTTP (restclient) ---
(use-package restclient)
(use-package company-restclient :after (restclient company))

;; --- markdown ---
(use-package markdown-mode
  :defer t
  :hook (markdown-mode . visual-line-mode)
  :config
  (setq markdown-command "pandoc"))

;; --- terraform ---
(defun my/terraform-mode-init ()
  (outline-minor-mode 1))

(use-package terraform-mode
  :defer t
  :custom
  (terraform-indent-level 4)
  :hook (terraform-mode . my/terraform-mode-init))

(provide 'languages)
;;; languages.el ends here
