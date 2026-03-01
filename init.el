;;; init.el --- Hyprstellar Emacs config v3  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs 30.2 — moe-dark theme, CaskaydiaCove NF, native keybinds.
;; Dual package managers: straight.el (default) + package.el/MELPA (fallback).
;; No evil-mode. OS-adaptive (Linux / macOS / Windows).

;;; Code:

;; ─── package.el — MELPA + GNU + NonGNU ELPA ──────────────────────────────────
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")
        ("melpa"  . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa"  . 10)
        ("nongnu" .  5)
        ("gnu"    .  1)))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; ─── Bootstrap straight.el ───────────────────────────────────────────────────
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; ─── Pin transient FIRST — before magit/rg/anything that uses it ─────────────
;; rg.el calls transient--set-layout which was removed in transient 0.8+.
;; Force the MELPA version (newest) before any other package loads transient.
(straight-use-package 'transient)
(require 'transient)

;; ─── use-package defaults ────────────────────────────────────────────────────
(setq straight-use-package-by-default t
      use-package-always-ensure t
      use-package-expand-minimally t)

;; ─── Package manager usage guide ─────────────────────────────────────────────
;; straight (default, git clone):         (use-package foo ...)
;; straight from GitHub:                  (use-package foo :straight (:host github :repo "u/r") ...)
;; package.el / MELPA tarball:            (use-package foo :straight nil :ensure t ...)
;; built-in — no install:                 (use-package foo :straight nil :ensure nil ...)

(setq package-enable-at-startup nil)

;; load Font
(set-face-attribute 'default nil 
                    :font "CaskaydiaCove Nerd Font" 
                    :height 120)

(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-11"))


;; ─── Load modules ─────────────────────────────────────────────────────────────
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'config-os)          ; OS detection — loaded first
(require 'config-options)
(require 'config-theme)
(require 'config-ui)
(require 'config-completion)
(require 'config-lsp)
;;(require 'config-treesitter)
(require 'config-dired)
(require 'config-search)
(require 'config-keymaps)
(require 'config-extra)

;;; init.el ends here
