;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;; Loaded before package.el and the GUI is initialized.
;; Keep this lean — only things that must happen before the frame exists.

;;; Performance

;; Maximize GC threshold during startup; reset in init.el after startup hook.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Native-comp: silence warnings and missing-source noise.
(setq native-comp-async-report-warnings-errors nil
      native-comp-warning-on-missing-source nil)

;;; Frame
(setq default-frame-alist
      '((min-height              . 1)
        (min-width               . 1)
        (internal-border-width   . 12)   ; breathing room on linux desktop
        (vertical-scroll-bars    . nil)
        (horizontal-scroll-bars  . nil)
        (tool-bar-lines          . 0)
        (menu-bar-lines          . 0)))

;; Italic buffer names on Linux frame title-bar (Gtk window title).
;; Pango markup is NOT supported in frame titles; we use the italic
;; Unicode math script range as a portable visual hint instead —
;; the actual italic face is applied in init.el via frame-title-format.
(setq frame-inhibit-implied-resize t)

;;; Startup
(setq inhibit-startup-screen        t
      inhibit-startup-message       t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init          t)

(setq use-dialog-box nil
      use-file-dialog nil)

;;; Rendering tweaks

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq inhibit-compacting-font-caches t)

;; Disable package.el - bootstrap use-package manually in init.el.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
