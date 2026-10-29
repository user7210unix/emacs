;;; my-prog.el --- Programming: tree-sitter, Eglot, JavaScript, C/C++ -*- lexical-binding: t; -*-

(require 'my-settings)
(require 'my-completion)

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4))

(use-package eglot
  :ensure nil
  :hook (eglot-managed-mode . my/eglot-capf-setup)
  :bind (:map eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l h" . eglot-inlay-hints-mode)
         ("C-c l d" . eldoc-doc-buffer))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.3)
  :config
  (add-to-list 'eglot-server-programs
               `(,my/js-modes . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"))))

(dolist (mode my/eglot-modes)
  (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-max-pixel-width 520)
  (eldoc-box-max-pixel-height 320))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package flymake-eslint)

(defun my/js-eslint-setup ()
  (when (and (apply #'derived-mode-p my/js-modes)
             (executable-find "npx"))
    (flymake-eslint-enable)))

(add-hook 'eglot-managed-mode-hook #'my/js-eslint-setup)

(use-package apheleia
  :hook (after-init . apheleia-global-mode))

(defun my/js-mode-setup ()
  (setq-local js-indent-level my/js-indent
              typescript-ts-mode-indent-offset my/js-indent
              compile-command (my/js-compile-command)))

(defun my/js-compile-command ()
  (cond ((file-exists-p "package.json") "npm run build")
        (buffer-file-name (format "node %s" (shell-quote-argument buffer-file-name)))
        (t "node")))

(dolist (mode my/js-modes)
  (add-hook (intern (format "%s-hook" mode)) #'my/js-mode-setup))

(use-package emmet-mode
  :hook ((html-mode mhtml-mode css-mode tsx-ts-mode) . emmet-mode))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package project
  :ensure nil
  :custom (project-vc-extra-root-markers '("package.json" "pom.xml" "build.gradle")))

(defun my/c-mode-setup ()
  (setq-local c-basic-offset my/tab-width))
(add-hook 'c-mode-common-hook #'my/c-mode-setup)

(provide 'my-prog)
;;; my-prog.el ends here
