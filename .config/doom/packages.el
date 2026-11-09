;;; packages.el -*- lexical-binding: t; -*-
;; Additional packages not already pulled in by enabled modules.

;; Centered, roomy frame padding (face-remapping; see config.el).
(package! spacious-padding)

;; Disable treemacs' nerd-glyph icons (see config.el for treemacs-no-png-images).
(package! treemacs-nerd-icons :disable t)

;; Edit privileged files (ported from the old config).
(package! sudo-edit)
