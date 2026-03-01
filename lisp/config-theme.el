;;; config-theme.el --- moe-dark theme + icons  -*- lexical-binding: t; -*-
;;; Commentary:
;; moe-theme is on MELPA/NonGNU ELPA — use :straight nil :ensure t
;; moe-theme.el is NOT a theme file itself; it wraps moe-dark-theme / moe-light-theme.
;; The correct call sequence:
;;   1. require 'moe-theme
;;   2. set customization variables  (BEFORE calling moe-dark)
;;   3. call (moe-dark)

;;; Code:

;; ─── Nerd icons (before theme consumers) ─────────────────────────────────────
(use-package nerd-icons
  :straight nil :ensure t   ; available on MELPA
  :custom
  (nerd-icons-font-family "CaskaydiaCove Nerd Font Mono"))

;; ─── moe-theme ────────────────────────────────────────────────────────────────
;; On NonGNU ELPA and MELPA — install via package.el
(use-package moe-theme
  :straight nil :ensure t
  :init
  ;; Resize headings in org / markdown (must be set BEFORE moe-dark)
  (setq moe-theme-resize-org-title      '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0)
        moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0)
        ;; Highlight buffer name in modeline
        moe-theme-highlight-buffer-id t)
  :config
  ;; Apply moe-dark and set cyan as modeline accent colour
  (moe-dark))

;; ─── Rainbow delimiters ──────────────────────────────────────────────────────
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ─── Rainbow mode — hex/CSS colour highlights ────────────────────────────────
(use-package rainbow-mode
  :straight nil :ensure t   ; MELPA
  :diminish
  :hook (prog-mode css-mode html-mode web-mode))

;; ─── Nerd icons in completion / M-x ─────────────────────────────────────────
(use-package nerd-icons-completion
  :straight nil :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'config-theme)
;;; config-theme.el ends here
