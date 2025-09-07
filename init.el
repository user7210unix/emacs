;;; init.el --- Complete Emacs Living Environment Configuration
;;; Commentary:
;;; A comprehensive Emacs configuration for living inside Emacs
;;; Features: IDE, browser, media player, vim keybindings, modern UI

;;; Code:

;; ============================================================================
;; PACKAGE MANAGEMENT - Using straight.el for modern package management
;; ============================================================================

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; ============================================================================

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; ============================================================================
;; BASIC EMACS CONFIGURATION
;; ============================================================================

;; Personal information
(setq user-full-name "Your Name"
      user-mail-address "your.email@example.com")

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Better defaults
(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Prefer spaces over tabs
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                           ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                      ; Don't use sRGB colors
 recenter-positions '(5 top bottom)             ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 10                                ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward             ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t)                             ; Stretch cursor to the glyph width

;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)

;; ============================================================================
;; UI/UX CONFIGURATION - Modern and Beautiful Interface
;; ============================================================================

;; Remove unnecessary UI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Show line numbers globally
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Show column number
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Show matching parentheses
(show-paren-mode 1)

;; ============================================================================
;; ELEGANT THEME AND APPEARANCE
;; ============================================================================

;; Modern theme - Doom themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Modern modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))

;; All the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Better fonts (install these fonts on your system)
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font "JetBrains Mono Nerd Font"
                      :height 110
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :font "Inter"
                      :height 110
                      :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
                      :font "JetBrains Mono Nerd Font"
                      :height 110
                      :weight 'normal))

;; ============================================================================
;; VIM KEYBINDINGS - Evil mode configuration
;; ============================================================================

;; Evil mode - Vim emulation
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil collection for better integration
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil commentary for commenting
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Evil surround for surrounding text objects
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ============================================================================
;; WHICH-KEY - Keybinding discovery
;; ============================================================================

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-max-height 0.25
        which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-description-length 25))

;; ============================================================================
;; GENERAL - Better key definitions
;; ============================================================================

(use-package general
  :after evil
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  ;; Define leader key mappings
  (my/leader-keys
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf-open-files :which-key "recent files")
    "fs" '(save-buffer :which-key "save file")
    
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bk" '(kill-buffer :which-key "kill buffer")
    "bl" '(list-buffers :which-key "list buffers")
    
    "w"  '(:ignore t :which-key "windows")
    "wh" '(evil-window-left :which-key "window left")
    "wj" '(evil-window-down :which-key "window down")
    "wk" '(evil-window-up :which-key "window up")
    "wl" '(evil-window-right :which-key "window right")
    "ws" '(evil-window-split :which-key "split horizontal")
    "wv" '(evil-window-vsplit :which-key "split vertical")
    "wd" '(evil-window-delete :which-key "delete window")
    
    "p"  '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(projectile-find-file :which-key "find file in project")
    "pg" '(projectile-ripgrep :which-key "grep in project")
    
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")
    "gc" '(magit-commit :which-key "git commit")
    "gp" '(magit-push :which-key "git push")
    
    "t"  '(:ignore t :which-key "toggle")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "tn" '(display-line-numbers-mode :which-key "line numbers")
    
    "h"  '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "hk" '(describe-key :which-key "describe key")))

;; ============================================================================
;; PROJECT MANAGEMENT
;; ============================================================================

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy
        projectile-project-search-path '("~/projects/")))

;; ============================================================================
;; COMPLETION FRAMEWORK - Ivy/Counsel/Swiper
;; ============================================================================

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

;; ============================================================================
;; PROGRAMMING - IDE FEATURES
;; ============================================================================

;; Company mode for autocompletion
(use-package company
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match 'never))

;; Company box for better UI
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; Flycheck for syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode))

;; LSP Mode for Language Server Protocol
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-signature-auto-activate nil))

;; LSP UI enhancements
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil))

;; LSP Ivy integration
(use-package lsp-ivy
  :after (lsp-mode ivy))

;; Treemacs for project tree view
(use-package treemacs
  :config
  (setq treemacs-width 35
        treemacs-follow-mode t
        treemacs-filewatch-mode t))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; ============================================================================
;; LANGUAGE SPECIFIC CONFIGURATIONS
;; ============================================================================

;; C/C++ Configuration
(use-package cc-mode
  :straight nil
  :config
  (setq c-default-style "linux"
        c-basic-offset 4))

;; Java Configuration
(use-package lsp-java
  :after lsp-mode)

;; Python Configuration
(use-package python
  :straight nil
  :config
  (setq python-indent-offset 4))

;; Web Development
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; ============================================================================
;; GIT INTEGRATION
;; ============================================================================

(use-package magit
  :commands magit-status)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; ============================================================================
;; FILE MANAGEMENT - Dired enhancements
;; ============================================================================

(use-package dired
  :straight nil
  :config
  (setq dired-listing-switches "-alh --group-directories-first"))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; ============================================================================
;; WEB BROWSER - EWW and external browser integration
;; ============================================================================

(use-package eww
  :straight nil
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="
        browse-url-browser-function 'eww-browse-url))

;; Better web browsing with xwidget-webkit (if available)
(when (featurep 'xwidget-internal)
  (use-package xwidget
    :straight nil
    :bind ("C-c w" . xwidget-webkit-browse-url)))

;; ============================================================================
;; MEDIA PLAYERS
;; ============================================================================

;; EMMS - Emacs MultiMedia System
(use-package emms
  :config
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/"
        emms-playlist-buffer-name "*EMMS Playlist*"
        emms-info-asynchronously t
        emms-show-format "Playing: %s")
  
  ;; Add keybindings for media control
  (global-set-key (kbd "C-c m p") 'emms-pause)
  (global-set-key (kbd "C-c m s") 'emms-stop)
  (global-set-key (kbd "C-c m n") 'emms-next)
  (global-set-key (kbd "C-c m b") 'emms-previous)
  (global-set-key (kbd "C-c m l") 'emms-playlist-mode-go)
  (global-set-key (kbd "C-c m d") 'emms-add-directory)
  (global-set-key (kbd "C-c m f") 'emms-add-file))

;; VLC support for video files
(use-package vlc
  :straight (vlc :type git :host github :repo "xuchunyang/vlc.el")
  :config
  (setq vlc-executable "vlc"))

;; ============================================================================
;; TERMINAL EMULATION
;; ============================================================================

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000))

;; Multi-term for multiple terminals
(use-package multi-term
  :config
  (setq multi-term-program "/bin/bash"))

;; ============================================================================
;; PDF VIEWER
;; ============================================================================

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.1
        pdf-view-use-unicode-ligatures t))

;; ============================================================================
;; ORG MODE CONFIGURATION
;; ============================================================================

(use-package org
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ============================================================================
;; DASHBOARD - Beautiful startup screen
;; ============================================================================

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))))

;; ============================================================================
;; PERFORMANCE AND UTILITY PACKAGES
;; ============================================================================

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; Save place in files
(save-place-mode 1)

;; Auto-save and backup configuration
(setq auto-save-default t
      make-backup-files t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Winner mode - undo/redo window configurations
(winner-mode 1)

;; Electric pair mode - auto-close brackets
(electric-pair-mode 1)

;; Highlight TODO keywords
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================================
;; CUSTOM FUNCTIONS
;; ============================================================================

(defun my/open-config ()
  "Open the init.el file."
  (interactive)
  (find-file user-init-file))

(defun my/reload-config ()
  "Reload the init.el file."
  (interactive)
  (load-file user-init-file))

;; Add to leader keys
(my/leader-keys
  "fed" '(my/open-config :which-key "edit config")
  "fer" '(my/reload-config :which-key "reload config"))

;; ============================================================================
;; FINAL CONFIGURATIONS
;; ============================================================================

;; Start server for emacsclient
(unless (server-running-p)
  (server-start))

;; Custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Message on startup completion
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; init.el ends here
