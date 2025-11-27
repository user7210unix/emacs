;;; init.el --- Fast & clean Emacs config for C development -*- lexical-binding: t; -*-

;; ────────────────────────────── Package setup ──────────────────────────────
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ────────────────────────────── Better defaults ──────────────────────────────
(use-package better-defaults)   ; sensible defaults (already includes no startup screen)

;; Disable menu bar and tool bar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; ────────────────────────────── Font: M PLUS Code Latin 170 ──────────────────────────────
(defun my/set-mplus-font ()
  "Set M PLUS Code Latin at size 170 (17.0 pt)."
  (when (member "M PLUS Code Latin" (font-family-list))
    (set-face-attribute 'default nil
                        :font "M PLUS Code Latin-17"
                        :weight 'normal
                        :height 200)
    (set-face-attribute 'fixed-pitch nil :font "M PLUS Code Latin-20")
    (set-face-attribute 'variable-pitch nil :font "M PLUS Code Latin-20")))

(add-hook 'after-init-hook #'my/set-mplus-font)
(add-hook 'server-after-make-frame-hook #'my/set-mplus-font) ; for emacsclient

;; ────────────────────────────── Theme & visuals ──────────────────────────────
(use-package kanagawa-themes
  :config
  (load-theme 'kanagawa-wave t)   ; beautiful Japanese-inspired theme
  ;; Italic comments (IntelliJ style)
  (custom-set-faces
   '(font-lock-comment-face ((t (:slant italic))))
   '(font-lock-doc-face ((t (:slant italic))))))

;; auto close brackets insertion 
(electric-pair-mode 1)

;; ────────────────────────────── Company (fast C completion) ──────────────────────────────
(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.05)
  :config
  (add-to-list 'company-backends 'company-clang))

;; ────────────────────────────── C mode ──────────────────────────────
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "linux")
            (setq c-basic-offset 4
                  indent-tabs-mode t)))

;; ────────────────────────────── Ivy/Counsel (M-x with suggestions) ──────────────────────────────
(use-package ivy
  :demand t
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-height 12))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x b"   . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)))

;; ────────────────────────────── Improved ibuffer ──────────────────────────────
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ────────────────────────────── Nerd icons + better dired ──────────────────────────────
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; ────────────────────────────── Ultra-scroll (smooth scrolling) ──────────────────────────────
(use-package ultra-scroll
  :config
  (setq ultra-scroll-smooth-scroll t)
  (ultra-scroll-mode 1))

;; ────────────────────────────── Optional enhancements ──────────────────────────────
;; Uncomment if you want these packages:
;; (use-package org-modern)         ; prettier org-mode
;; (use-package nano-modeline)      ; minimal modeline

;; ────────────────────────────── Speed tweaks ──────────────────────────────
(setq gc-cons-threshold (* 128 1024 1024)  ; 128 MiB
      gc-cons-percentage 0.6)

;; ────────────────────────────── Custom variables (auto-generated) ──────────────────────────────
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077" 
     "c20728f5c0cb50972b50c929b004a7496d3f2e2ded387bf870f89da25793bb44" 
     "d2ab3d4f005a9ad4fb789a8f65606c72f30ce9d281a9e42da55f7f4b9ef5bfc6" 
     default))
 '(package-selected-packages
   '(kanagawa-themes org-modern ultra-scroll nerd-icons-dired nerd-icons 
     counsel ivy company doom-themes better-defaults)))

;; Done!
