;;; -*- lexical-binding: t; -*-
;;; init.el --- Emacs styled like a GNOME app (light Inter setup)

;; -------------------------------
;; FRAME & STARTUP
;; -------------------------------
(push '(width  . 80) default-frame-alist)
(push '(height . 46) default-frame-alist)
(setq frame-title-format nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil
      ring-bell-function 'ignore)

;; -------------------------------
;; PACKAGE MANAGEMENT
;; -------------------------------
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; -------------------------------
;; UI APPEARANCE
;; -------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)

;; Smooth scrolling
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq scroll-margin 8
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Theme & padding
(use-package ef-themes
  :init (load-theme 'ef-light t))

(use-package spacious-padding
  :init
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :mode-line-width 6
           :right-divider-width 10
           :scroll-bar-width 8))
  (spacious-padding-mode 1))

;; Font — keep your Inter SemiBold
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inter" :weight semi-bold :height 128 :width normal)))))

;; -------------------------------
;; ICONS
;; -------------------------------
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :after vertico
  :config (nerd-icons-completion-mode))

;; -------------------------------
;; COMPLETION
;; -------------------------------
;; Disable fido, use Vertico instead for GNOME minimal look
(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize t))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :init (setq completion-styles '(orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company
  :init (global-company-mode))

;; -------------------------------
;; LINE NUMBERS
;; -------------------------------
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t)
  (dolist (hook '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
    (add-hook hook (lambda () (display-line-numbers-mode 0)))))

;; -------------------------------
;; SYNTAX CHECKING
;; -------------------------------
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; -------------------------------
;; LSP CONFIGURATION
;; -------------------------------
(use-package lsp-mode
  :commands lsp
  :hook ((java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil))

(use-package lsp-java)

;; -------------------------------
;; GNOME-LIKE MODELINE
;; -------------------------------
(use-package mood-line
  :init
  (add-hook 'after-init-hook #'mood-line-mode))

;; -------------------------------
;; GNOME-LIKE TABS
;; -------------------------------
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-format '(tab-bar-format-tabs))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
