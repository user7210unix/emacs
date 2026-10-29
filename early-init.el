;;; early-init.el --- Runs before package.el and the first frame -*- lexical-binding: t; -*-

(defvar my/gc-threshold-runtime (* 64 1024 1024))
(defvar my/startup-file-handlers file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil
      load-prefer-newer t
      package-enable-at-startup nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-compacting-font-caches t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      use-dialog-box nil
      use-file-dialog nil
      native-comp-async-report-warnings-errors nil
      native-comp-warning-on-missing-source nil)

(setq default-frame-alist
      '((fullscreen . maximized)
        (internal-border-width . 0)
        (vertical-scroll-bars)
        (horizontal-scroll-bars)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              cursor-in-non-selected-windows nil)
(setq bidi-inhibit-bpa t
      highlight-nonselected-windows nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/gc-threshold-runtime
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/startup-file-handlers)))

;;; early-init.el ends here
