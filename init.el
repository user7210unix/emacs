;==========================================================
;   PERFORMANCE OPTIMIZATION
;==========================================================

;; Suppress compilation warnings
(setq native-comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq warning-minimum-level :error)

;; Increase garbage collection threshold for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Reduce startup time
(setq package-enable-at-startup nil)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(tool-bar-mode -1)             ; Hide the outdated icons

;==========================================================
;   PACKAGE MANAGEMENT
;==========================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(load-theme 'tango t)

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)  ; Changed from 'relative to t
(setq display-line-numbers-width-start t)

;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Better scrolling
(setq scroll-margin 8
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; Remove initial scratch message and "For information about GNU Emacs and the 
;; GNU system, type C-h C-a"
(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message "")))

;; Allow y/n instead of having to type yes/no
(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

;; Cape: extra completion sources
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Orderless: smarter fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Vertico: minibuffer completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Marginalia: annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Consult: better search & navigation
(use-package consult
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(package-selected-packages '(consult marginalia vertico orderless cape corfu))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Arial" :foundry "TMC " :slant normal :weight regular :height 151 :width normal)))))

;==========================================================
;   TREEMACS - FILE EXPLORER
;==========================================================

(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 35
        treemacs-follow-mode t
        treemacs-filewatch-mode t
        treemacs-fringe-indicator-mode 'always
        treemacs-git-mode 'deferred
        treemacs-show-hidden-files t
        treemacs-indentation 2
        treemacs-indent-guide-style 'line)

  ;; Restore Opened Files
(progn
  (desktop-save-mode 1)
  ;; save when quit
  (setq desktop-save t)

  ;; no ask if crashed
  (setq desktop-load-locked-desktop t)

  (setq desktop-restore-frames t)

  (setq desktop-auto-save-timeout 300)

  ;; save some global vars
  (setq desktop-globals-to-save nil)
  ;; 2023-09-16 default
  ;; '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)
  )

(when (< emacs-major-version 25)
  (require 'saveplace)
  (setq-default save-place t))

(when (>= emacs-major-version 25)
  (save-place-mode 1))

;; save minibuffer history
(savehist-mode 1)
