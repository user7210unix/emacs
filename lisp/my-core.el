;;; my-core.el --- Sane defaults and built-in modes -*- lexical-binding: t; -*-

(require 'my-settings)

(setq-default
 fill-column my/fill-column
 tab-width my/tab-width
 indent-tabs-mode nil
 line-spacing my/line-spacing
 truncate-lines nil
 cursor-type 'bar
 require-final-newline t
 help-window-select t
 vc-follow-symlinks t
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil)

(setq ring-bell-function 'ignore
      warning-minimum-level :error
      scroll-conservatively 101
      scroll-margin 8
      read-process-output-max (* 1024 1024)
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      apropos-do-all t
      sentence-end-double-space nil
      tab-always-indent 'complete
      switch-to-buffer-obey-display-actions t
      use-short-answers t
      custom-file (locate-user-emacs-file "custom.el"))

(load custom-file t)
(when (fboundp (quote fringe-mode))
  (fringe-mode my/fringe-width))

(use-package emacs
  :ensure nil
  :hook ((after-init . global-auto-revert-mode)
         (after-init . delete-selection-mode)
         (after-init . electric-pair-mode)
         (after-init . repeat-mode)
         (after-init . pixel-scroll-precision-mode)))

(defun my/trim-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (history-length 200)
  (history-delete-duplicates t))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.6))

(use-package pulsar
  :hook (after-init . pulsar-global-mode)
  :custom
  (pulsar-delay 0.04)
  (pulsar-iterations 8)
  (pulsar-face 'pulsar-magenta))

(use-package hl-line
  :ensure nil
  :hook (prog-mode . hl-line-mode))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t))

(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :custom (whitespace-style '(face trailing)))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(bind-keys ("M-o" . other-window)
           ("C-c k" . kill-current-buffer)
           ("C-c c" . compile)
           ("C-c r" . recompile)
           ("C-c e" . eshell))

(defun my/disable-corfu-locally ()
  (corfu-mode -1))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . my/disable-corfu-locally)
  :custom (eshell-destroy-buffer-when-process-dies t))

(use-package xterm-color)

(defun my/compilation-colorize (proc)
  (when (process-live-p proc)
    (set-process-filter
     proc
     (lambda (proc string)
       (comint-output-filter proc (xterm-color-filter string))))))

(add-hook 'before-save-hook #'my/trim-trailing-whitespace)
(add-hook 'compilation-start-hook #'my/compilation-colorize)

(provide 'my-core)
;;; my-core.el ends here
