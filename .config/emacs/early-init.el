;;; early-init.el -*- lexical-binding: t; -*-
;; Runs before package.el and GUI frame initialization.

(setenv "LSP_USE_PLISTS" "true")

;; ---------------------------------------------------------------------------
;; Startup performance
;; ---------------------------------------------------------------------------

;; Disable GC during startup; restored at the end of init.el.
(setq gc-cons-threshold most-positive-fixnum)

;; Skip file-name handlers during startup; restored in init.el.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq read-process-output-max (* 4 1024 1024)) ; 4 MiB

;; ---------------------------------------------------------------------------
;; Package manager — must be configured before package-initialize
;; ---------------------------------------------------------------------------

(setq package-enable-at-startup nil) ; we call package-initialize ourselves in init.el
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-native-compile t)

;; ---------------------------------------------------------------------------
;; Silence noise
;; ---------------------------------------------------------------------------

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; ---------------------------------------------------------------------------
;; UI / input
;; ---------------------------------------------------------------------------

(setq inhibit-startup-echo-area-message (user-login-name))
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            (ns-transparent-titlebar . t)))

(setq mac-command-modifier 'super)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; early-init.el ends here
