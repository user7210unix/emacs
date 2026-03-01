;;; config-ui.el --- UI polish  -*- lexical-binding: t; -*-

;;; Code:

;; ─── doom-modeline ───────────────────────────────────────────────────────────
(use-package doom-modeline
  :straight nil :ensure t   ; MELPA
  :init
  (setq doom-modeline-height 24
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 24
        doom-modeline-lsp t
        doom-modeline-env-version t)
  :config
  (doom-modeline-mode 1))


;; ─── eldoc-box — hover docs ──────────────────────────────────────────────────
(use-package eldoc-box
  :straight nil :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-max-pixel-width  650)
  (eldoc-box-max-pixel-height 350)
  (eldoc-box-clear-with-C-g   t))

;; ─── posframe (required by vertico-posframe) ─────────────────────────────────
(use-package posframe :straight nil :ensure t)

;; ─── Ultra-smooth scrolling ──────────────────────────────────────────────────
(use-package ultra-scroll
  :straight (:host github :repo "jdtsmith/ultra-scroll")
  :init (setq scroll-conservatively 101 scroll-margin 0)
  :config (ultra-scroll-mode 1))

(provide 'config-ui)
;;; config-ui.el ends here
