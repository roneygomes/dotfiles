;;; init.el -*- lexical-binding: t; -*-
;; Doom Emacs: module declarations (run `doom sync` after editing).

;; Prevent "Memory exhausted" during startup. Doom disables GC (sets
;; `gc-cons-threshold' to `most-positive-fixnum') to speed up boot, but loading
;; many natively-compiled packages can exhaust the Lisp heap before Doom
;; re-enables GC. Cap it at a large-but-finite value so GC can still run.
(setq gc-cons-threshold (* 256 1024 1024))  ; 256 MiB

(doom! :input
       ;;layout

       :completion
       (company +childframe)     ; in-buffer completion
       vertico                   ; vertico + marginalia + consult

       :ui
       doom                      ; Doom UI polish + theme
       dashboard                 ; landing screen
       modeline                  ; doom-modeline
       (popup +defaults)         ; tame popup windows
       treemacs                  ; file tree
       vc-gutter                 ; git diff in the fringe
       vi-tilde-fringe           ; vim ~ on empty lines
       workspaces                ; perspective-like workspace groups

       :editor
       (evil +everywhere)        ; vim bindings everywhere
       file-templates
       (format +onsave)          ; apheleia formatting
       multiple-cursors          ; evil-multiedit style
       snippets
       word-wrap

       :emacs
       (dired +icons)
       electric
       undo                      ; undo-fu + cross-session persistence
       vc

       :checkers
       syntax                    ; flycheck
       ;;spell
       ;;grammar

       :tools
       lookup
       lsp
       magit
       terraform
       (tree-sitter +auto)       ; native tree-sitter modes

       :os
       (:if IS-MAC macos)
       tty

       :lang
       emacs-lisp
       go
       javascript                ; js + typescript
       json
       markdown
       org                       ; org-mode notes
       python
       rest                      ; restclient
       rust
       sh                        ; bash/shell
       web                       ; html/css/tsx
       yaml

       :config
       ;; Sets reasonable defaults (recentf, savehist, etc.).
       (default +bindings +smartparens))
