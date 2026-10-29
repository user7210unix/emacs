;;; my-completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-

(require 'my-settings)

(defvar consult-buffer-filter)

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-resize nil))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(defun my/consult-buffer-all ()
  "Switch buffer, including the ones normally hidden."
  (interactive)
  (let ((consult-buffer-filter '("\\` ")))
    (consult-buffer)))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x B" . my/consult-buffer-all)
         ("C-x C-r" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep))
  :config
  (setq consult-buffer-filter (append consult-buffer-filter my/buffer-hidden-patterns)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

(use-package corfu
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)
         (global-corfu-mode . corfu-history-mode))
  :bind (:map corfu-map
         ("TAB" . corfu-insert)
         ("<tab>" . corfu-insert)
         ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil)
  (corfu-min-width 32)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file 90)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 90))

(defun my/eglot-capf-setup ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'eglot-completion-at-point #'cape-dabbrev)
                    #'cape-file)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'my-completion)
;;; my-completion.el ends here
