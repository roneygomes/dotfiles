;;; early-init.el -*- lexical-binding: t; -*-
;; Runs before package.el and GUI frame initialization.

;; Start maximized with a minimal UI — set before the GUI frame is created so
;; the window never flashes a toolbar, scroll bar, or menu bar during startup.
(setq default-frame-alist
      (append '((fullscreen . maximized)
                (tool-bar-lines . 0)
                (menu-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil))
              default-frame-alist))

;; Package archives — must be set before `package-initialize' (in init.el).
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; MELPA signs packages but its key isn't in the keyring by default, which
;; makes installation fail with "Failed to verify signature". Disable signature
;; checking (fine for a personal config).
(setq package-check-signature nil)

;; Native-compile installed packages.
(setq package-native-compile t)

;; Defer GC during startup; restored at the end of init.el.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Bigger read buffer for process output (LSP, subprocesses).
(setq read-process-output-max (* 1024 1024))

;;; early-init.el ends here
