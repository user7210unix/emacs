;; -*- lexical-binding: t; -*-
;; Simple Emacs config — minimal, usable, extendable

;; Basic UI cleanup
(setq inhibit-startup-screen t)        ;; no splash screen
(menu-bar-mode -1)                     ;; disable menu bar
(tool-bar-mode -1)                     ;; disable tool bar
(scroll-bar-mode -1)                   ;; disable scroll bar
(blink-cursor-mode 0)                 ;; no blinking cursor

;; Basic editing tweaks
(setq-default indent-tabs-mode nil)   ;; use spaces, not tabs
(setq-default tab-width 4)
(global-auto-revert-mode t)           ;; auto-reload changed files

;; Setup package management
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Optionally install use-package for easier package declarations
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Example: install and enable a useful package (e.g. Ivy for better completion)
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

;; Example: set a simple default theme
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

;; Org-mode setup (notes, TODOs, etc.)
(use-package org
  :ensure t
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t))

;; --- custom-file: keep settings made via M-x customize separate ---
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

;; Optional: Show line numbers globally
(global-display-line-numbers-mode 1)

;; --- Smooth Scrolling ---
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 40.0
      pixel-scroll-precision-interpolate-page t
      scroll-margin 3
      scroll-step 1
      scroll-conservatively 101
      auto-window-vscroll nil)

;; --- Vertico Completion Setup ---
(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult
  :ensure t)
(global-set-key (kbd "C-s") 'consult-line)

;; --- Better Dired ---
(use-package dired
  :ensure nil
  :custom (dired-listing-switches "-alh --group-directories-first"))

(put 'dired-find-alternate-file 'disabled nil)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-preview
  :ensure t
  :hook (dired-mode . dired-preview-mode))

(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Always use bash
  (setq vterm-shell "/bin/bash")

  ;; Optional: don't ask about killing active processes
  (setq vterm-kill-buffer-on-exit t))


(provide 'init)
;;; init.el ends here
