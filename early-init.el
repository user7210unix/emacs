;; Disable unnecessary UI elements
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))

;; Add internal border for aesthetics
(add-to-list 'default-frame-alist '(internal-border-width . 20))

;; Disable mode-line (will be customized in init.el)
(setq mode-line-format nil)
