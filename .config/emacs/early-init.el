;;; early-init.el -*- lexical-binding: t; -*-
;; Runs before package.el and GUI frame initialization.

;; Package archives — must be set before `package-initialize' (in init.el).
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Native-compile installed packages.
(setq package-native-compile t)

;; Defer GC during startup; restored at the end of init.el.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Bigger read buffer for process output (LSP, subprocesses).
(setq read-process-output-max (* 1024 1024))

;;; early-init.el ends here
