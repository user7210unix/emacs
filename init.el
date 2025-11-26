;; Basic UI cleanup
(setq inhibit-startup-screen t)        ;; no splash screen
(menu-bar-mode -1)                     ;; disable menu bar
(tool-bar-mode -1)                     ;; disable tool bar
(scroll-bar-mode -1)                   ;; disable scroll bar
(blink-cursor-mode 0)                 ;; no blinking cursor


;; Font
(custom-set-faces
 '(default ((t (:family "M PLUS CODE Latin" :weight Regular :height 168 :width normal))))) 

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

;; Ivy for better completion
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

;; Moody (modeline)
(use-package moody
  :ensure t
  :config
  ;; Replace buffer-identification and mode-line major-mode constructs
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification))
;;  (moody-replace-mode-line-major-mode))

;; Minions (optional, makes minor modes collapse into a menu)
(use-package minions
  :ensure t
  :hook (after-init . minions-mode))


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

;; ============================================================================
;; LSP Mode - Java Development
;; ============================================================================

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((java-mode . lsp-deferred)
         (lsp-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable t)
  (lsp-lens-enable t)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t)
  (lsp-completion-provider :none)
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-eldoc-enable-hover t)
  
  ;; Java specific
  (lsp-java-format-enabled t)
  (lsp-java-format-settings-url (expand-file-name "~/Documents/JetBrains-plugins/codestyle.xml"))
  (lsp-java-format-settings-profile "Default")
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-autobuild-enabled t)
  (lsp-java-import-gradle-enabled t)
  (lsp-java-import-maven-enabled t)
  (lsp-java-configuration-runtimes '[(:name "JavaSE-11" :path "/usr/lib/jvm/java-11-openjdk-amd64")
                                      (:name "JavaSE-17" :path "/usr/lib/jvm/java-17-openjdk-amd64" :default t)
                                      (:name "JavaSE-21" :path "/usr/lib/jvm/java-21-openjdk-amd64")])
  
  ;; Performance tuning
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 2000)
  
  :config
  ;; Ignored directories
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.idea\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\bin\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.settings\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.classpath\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.project\\'"))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t))

(use-package lsp-java
  :after lsp-mode)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-java))

;; ============================================================================
;; Suppress Messages
;; ============================================================================

(setq inhibit-message t)
(defun my/suppress-messages (orig-fun &rest args)
  "Suppress messages from function calls."
  (let ((inhibit-message t))
    (apply orig-fun args)))

(advice-add 'auto-save-mode :around #'my/suppress-messages)
(advice-add 'do-auto-save :around #'my/suppress-messages)

(run-with-idle-timer 2 t
                     (lambda ()
                       (when (string-match-p "^\\(Saving\\|Wrote\\|Auto-saving\\)" (or (current-message) ""))
                         (message nil))))


;; ============================================================================
;; Custom Functions
;; ============================================================================

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

;; ============================================================================
;; Programming Modes Configuration
;; ============================================================================

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'auto-revert-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

;; Java specific hooks
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

(provide 'init)
;; init.el end
