;; Performance: Increase garbage collection threshold during startup
;; Reset to reasonable value after initialization in init.el
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Suppress native compilation warnings
(setq native-comp-async-report-warnings-errors nil
      native-comp-warning-on-missing-source nil)

;; Disable package.el in favor of manual use-package initialization
(setq package-enable-at-startup nil)

;; UI: Remove visual clutter before the GUI initializes
;; Prevents flickering during startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Frame settings for clean appearance
(setq frame-inhibit-implied-resize t)

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

;; Disable unnecessary UI warnings
(setq use-dialog-box nil
      use-file-dialog nil)

;; Bidirectional text performance boost
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Faster scrolling settings
(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; Reduce rendering/line move work
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Prevent font compaction caching
(setq inhibit-compacting-font-caches t)

;;; early-init.el ends here
