;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded before package.el and the GUI. Keep this minimal and fast.

;;; Code:

;; ─── Disable package.el auto-init at startup ─────────────────────────────────
;; straight.el manages everything. package.el is still available in init.el
;; but must NOT auto-initialize here, otherwise both try to load the same
;; packages and straight throws the warning.
(setq package-enable-at-startup nil)

;; ─── Suppress all warning popups ─────────────────────────────────────────────
;; Warnings still go to the *Warnings* buffer (M-x view-warnings to review)
;; but NEVER pop up as an annoying window while you work.
(setq warning-minimum-level                    :emergency
      warning-minimum-log-level                :warning
      native-comp-async-report-warnings-errors nil)  ; silence native-comp spam

;; ─── Speed up startup ────────────────────────────────────────────────────────
(setq gc-cons-threshold (* 128 1024 1024))   ; raise GC threshold early
(setq load-prefer-newer t)                   ; prefer .el over stale .elc

;; ─── Clean frame before it renders ──────────────────────────────────────────
;; Prevents momentary flash of default toolbar/scrollbar before init runs
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here