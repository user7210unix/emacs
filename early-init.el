;;; early-init.el --- Early initialization to prevent package.el conflicts
;;; Commentary:
;;; This file is loaded before init.el and prevents package.el from
;;; conflicting with straight.el

;;; Code:

;; Disable package.el completely to prevent conflicts with straight.el
(setq package-enable-at-startup nil)

;; Prevent package.el from being loaded
(advice-add #'package--ensure-init-file :override #'ignore)

;; Speed up startup by preventing some UI elements from loading
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable native compilation warnings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-warning-on-missing-source nil))

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore garbage collection settings after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;;; early-init.el ends here
