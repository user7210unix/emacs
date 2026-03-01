;;; config-extra.el --- Extra packages  -*- lexical-binding: t; -*-

;;; Code:

;; ─── Markdown ────────────────────────────────────────────────────────────────
(use-package markdown-mode
  :straight nil :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-fontify-code-blocks-natively t
        markdown-list-indent-width 2))

(use-package grip-mode
  :straight nil :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map ("g" . grip-mode)))

;; ─── Smartparens ─────────────────────────────────────────────────────────────
(use-package smartparens
  :straight nil :ensure t
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config (require 'smartparens-config))

;; ─── Magit ───────────────────────────────────────────────────────────────────
(use-package magit
  :straight nil :ensure t
  :commands (magit-status magit-blame magit-log-current)
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :straight nil :ensure t
  :hook
  (prog-mode  . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode 1))

;; ─── Helpful ─────────────────────────────────────────────────────────────────
(use-package helpful
  :straight nil :ensure t
  :bind
  ("C-h f"   . helpful-callable)
  ("C-h v"   . helpful-variable)
  ("C-h k"   . helpful-key)
  ("C-h C-d" . helpful-at-point)
  ("C-h F"   . helpful-function)
  ("C-h C"   . helpful-command))

;; ─── Multiple cursors ────────────────────────────────────────────────────────
(use-package multiple-cursors
  :straight nil :ensure t
  :bind
  ("C->"       . mc/mark-next-like-this)
  ("C-<"       . mc/mark-previous-like-this)
  ("C-c C-<"   . mc/mark-all-like-this)
  ("C-S-c C-S-c" . mc/edit-lines))

;; ─── Expand region ───────────────────────────────────────────────────────────
(use-package expand-region
  :straight nil :ensure t
  :bind ("C-=" . er/expand-region))

;; ─── Avy — jump to visible text ──────────────────────────────────────────────
(use-package avy
  :straight nil :ensure t
  :bind
  ("M-j"   . avy-goto-char-timer)
  ("C-c j" . avy-goto-line))

;; ─── YAML / TOML ─────────────────────────────────────────────────────────────
(use-package yaml-mode :straight nil :ensure t :mode "\\.ya?ml\\'")
(use-package toml-mode :straight nil :ensure t :mode "\\.toml\\'")

;; ─── Docker ──────────────────────────────────────────────────────────────────
(use-package dockerfile-mode :straight nil :ensure t :mode "Dockerfile\\'")

;; ─── Rust ────────────────────────────────────────────────────────────────────
(use-package rust-mode
  :straight nil :ensure t
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil))))

;; ─── Web mode ────────────────────────────────────────────────────────────────
(use-package web-mode
  :straight nil :ensure t
  :mode ("\\.html?\\'" "\\.erb\\'" "\\.hbs\\'" "\\.ejs\\'" "\\.njk\\'" "\\.jsx\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset    2)
  (web-mode-code-indent-offset   2))

(use-package emmet-mode
  :straight nil :ensure t
  :hook (html-mode web-mode css-mode))

;; ─── Org modern ──────────────────────────────────────────────────────────────
(use-package org-modern
  :straight nil :ensure t
  :hook (org-mode . org-modern-mode))

(provide 'config-extra)
;;; config-extra.el ends here
