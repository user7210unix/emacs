;;; config-lsp.el --- LSP via Eglot + diagnostics + formatters  -*- lexical-binding: t; -*-

;;; Code:

;; ─── Eglot (built-in Emacs 30) ───────────────────────────────────────────────
(use-package eglot
  :ensure nil :straight nil
  :hook
  (js-ts-mode         . eglot-ensure)
  (tsx-ts-mode        . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (python-ts-mode     . eglot-ensure)
  (rust-ts-mode       . eglot-ensure)
  (go-ts-mode         . eglot-ensure)
  (lua-mode           . eglot-ensure)
  (css-ts-mode        . eglot-ensure)
  (html-mode          . eglot-ensure)
  (bash-ts-mode       . eglot-ensure)
  (json-ts-mode       . eglot-ensure)
  (yaml-ts-mode       . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  ;; Merge eglot + cape backends
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'eglot-completion-at-point
                                       #'cape-dabbrev
                                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l s" . consult-eglot-symbols)
              ("M-."     . xref-find-definitions)
              ("M-,"     . xref-go-back)
              ("M-?"     . xref-find-references)))

(use-package consult-eglot
  :straight nil :ensure t
  :after (consult eglot))

;; ─── Flymake ─────────────────────────────────────────────────────────────────
(use-package flymake
  :ensure nil :straight nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5))

(use-package flymake-popon
  :straight (:host codeberg :repo "akib/emacs-flymake-popon")
  :hook (flymake-mode . flymake-popon-mode)
  :custom (flymake-popon-delay 0.3))

(use-package sideline
  :straight (:host github :repo "emacs-sideline/sideline")
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake
  :straight (:host github :repo "emacs-sideline/sideline-flymake")
  :after sideline
  :init (setq sideline-flymake-display-mode 'line))

;; ─── Apheleia — async formatters ─────────────────────────────────────────────
(use-package apheleia
  :straight nil :ensure t
  :config (apheleia-global-mode +1))

(provide 'config-lsp)
;;; config-lsp.el ends here
