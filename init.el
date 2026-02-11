;; =================================================================
;; Package System Setup
;; =================================================================

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'bind-key)
  (package-install 'bind-key))

(eval-when-compile
  (require 'use-package))

;; Ensure bind-keys is available for :bind keyword
(require 'bind-key)

;; Always ensure packages are installed
(setq use-package-always-ensure t
      use-package-always-defer t  ; Lazy load by default
      use-package-expand-minimally t)

;; =================================================================
;; Performance & Startup
;; =================================================================

;; Reset GC threshold after startup (set high in early-init.el)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
                  gc-cons-percentage 0.1)))

;; Profile startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; =================================================================
;; General Settings
;; =================================================================

;; Better defaults
(setq-default
 ad-redefinition-action 'accept                 ; Silence warnings
 auto-save-default nil                           ; No auto-save files
 create-lockfiles nil                            ; No lock files
 fill-column 80                                  ; Line width
 help-window-select t                            ; Focus help windows
 indent-tabs-mode nil                            ; Use spaces
 make-backup-files nil                           ; No backup files
 require-final-newline t                         ; End files with newline
 tab-width 4                                     ; Tab display width
 truncate-lines nil                              ; Wrap long lines
 vc-follow-symlinks t)                           ; Follow symlinks

;; User interface
(defalias 'yes-or-no-p 'y-or-n-p)               ; Short answers
(setq ring-bell-function 'ignore)               ; No bell
(setq confirm-kill-emacs 'y-or-n-p)             ; Confirm before exit

;; Silence annoying prompts and warnings
(setq read-process-output-max (* 1024 1024)     ; 1MB read buffer for LSP
      warning-minimum-level :error)              ; Only show errors, not warnings

;; Keep custom settings in separate file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Remember recent files
(use-package recentf
  :ensure nil
  :demand t
  :config
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :demand t
  :config
  (setq history-length 100
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (savehist-mode 1))

;; Remember point position in files
(use-package saveplace
  :ensure nil
  :demand t
  :config
  (save-place-mode 1))

;; Auto-revert buffers when files change on disk
(use-package autorevert
  :ensure nil
  :demand t
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (global-auto-revert-mode 1))

;; =================================================================
;; Visual Settings
;; =================================================================

;; Line numbers in programming modes
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'absolute
        display-line-numbers-width-start t))

;; Show matching parentheses
(use-package paren
  :ensure nil
  :demand t
  :config
  (setq show-paren-delay 0
        show-paren-style 'mixed
        show-paren-when-point-inside-paren t)
  (show-paren-mode 1))

;; Electric pair mode for auto-closing brackets
(use-package elec-pair
  :ensure nil
  :demand t
  :config
  (electric-pair-mode 1))

;; Smooth pixel scrolling (Emacs 29+)
(use-package pixel-scroll
  :ensure nil
  :demand t
  :if (fboundp 'pixel-scroll-precision-mode)
  :config
  (pixel-scroll-precision-mode 1))

;; Scrolling behavior
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; Font configuration
(when (member "JetBrainsMono Nerd Font" (font-family-list))
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 123
                      :weight 'regular)
  
  ;; Make comments italic and smaller for focus
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic
                      :height 0.9)
  (set-face-attribute 'font-lock-doc-face nil
                      :slant 'italic
                      :height 0.9))

;; Color theme
(use-package emacs
  :ensure nil
  :demand t
  :config
  (load-theme 'tango-dark t))

;; =================================================================
;; Modeline - doom-modeline
;; =================================================================

(use-package nerd-icons
  :demand t
  :config
  ;; Run M-x nerd-icons-install-fonts on first use
  (when (and (display-graphic-p)
             (not (find-font (font-spec :name "Symbols Nerd Font Mono"))))
    (nerd-icons-install-fonts t)))

(use-package doom-modeline
  :demand t
  :after nerd-icons
  :config
  (setq doom-modeline-height 26
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-state-icon t
        doom-modeline-lsp t
        doom-modeline-check-simple-format t
        doom-modeline-vcs-max-length 20
        doom-modeline-env-version nil)
  (doom-modeline-mode 1))

;; =================================================================
;; Completion Framework - Vertico & Friends
;; =================================================================

(use-package vertico
  :demand t
  :config
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 12)
  (vertico-mode 1))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :after vertico
  :config
  (marginalia-mode 1))

(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s d"   . consult-find)
         ("M-s r"   . consult-ripgrep)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

;; =================================================================
;; Programming - General
;; =================================================================

;; Indentation guides
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\│
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t
        highlight-indent-guides-auto-odd-face-perc 5
        highlight-indent-guides-auto-even-face-perc 10
        highlight-indent-guides-auto-character-face-perc 15))

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Company - Autocompletion
(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-selection))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-show-quick-access t
        company-tooltip-align-annotations t
        company-tooltip-limit 10
        company-backends '(company-capf company-files company-keywords)
        ;; Disable in eshell
        company-global-modes '(not eshell-mode shell-mode)))

;; Flycheck - Syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.25))

;; Yasnippet - Snippet expansion
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; =================================================================
;; Java Development with Eglot
;; =================================================================

;; Eglot - Built-in LSP client (Emacs 29+)
;; More lightweight than lsp-mode, works great with jdtls
(use-package eglot
  :ensure nil
  :hook (java-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc-doc-buffer)
              ("C-c l o" . eglot-code-action-organize-imports))
  :config
  ;; Eglot settings
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-sync-connect nil  ; Don't block Emacs waiting for server
        eglot-connect-timeout 10)
  
  ;; Performance: Reduce events-buffer size
  (setq eglot-events-buffer-size 0)
  
  ;; Silence confirmation prompts
  (setq eglot-confirm-server-initiated-edits nil)
  
  ;; JDTLS Configuration
  (defvar my/jdtls-path (expand-file-name "~/.local/share/jdtls"))
  (defvar my/jdtls-workspace-path (expand-file-name "~/.cache/jdtls-workspace"))
  
  ;; Create workspace directory if it doesn't exist
  (unless (file-directory-p my/jdtls-workspace-path)
    (make-directory my/jdtls-workspace-path t))
  
  ;; Function to find the launcher jar automatically
  (defun my/jdtls-find-launcher-jar ()
    "Find the Eclipse Equinox launcher jar in jdtls plugins directory."
    (let ((plugins-dir (expand-file-name "plugins" my/jdtls-path)))
      (when (file-directory-p plugins-dir)
        (let ((jars (directory-files plugins-dir t "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$")))
          (when jars
            (car jars))))))
  
  ;; Function to get project-specific workspace
  (defun my/jdtls-workspace-for-project ()
    "Return workspace directory for current project."
    (let* ((project-root (or (and (fboundp 'project-root)
                                   (when-let ((proj (project-current)))
                                     (if (fboundp 'project-root)
                                         (project-root proj)
                                       (car (project-roots proj)))))
                             default-directory))
           (project-name (file-name-nondirectory 
                         (directory-file-name project-root))))
      (expand-file-name project-name my/jdtls-workspace-path)))
  
  ;; Configure jdtls server only if launcher jar exists
  (let ((launcher-jar (my/jdtls-find-launcher-jar))
        (config-dir (expand-file-name "config_linux" my/jdtls-path)))
    (if (and launcher-jar (file-exists-p launcher-jar) (file-directory-p config-dir))
        (progn
          (message "JDTLS configured with jar: %s" launcher-jar)
          (add-to-list 'eglot-server-programs
                       `(java-mode . ("java"
                                     "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                     "-Dosgi.bundles.defaultStartLevel=4"
                                     "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                     "-Dlog.protocol=true"
                                     "-Dlog.level=ALL"
                                     "-Xms256m"
                                     "-Xmx2048m"
                                     "--add-modules=ALL-SYSTEM"
                                     "--add-opens" "java.base/java.util=ALL-UNNAMED"
                                     "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                                     "-jar" ,launcher-jar
                                     "-configuration" ,config-dir
                                     "-data" ,(my/jdtls-workspace-for-project)))))
      (message "WARNING: JDTLS not found at %s - Java LSP features will not be available" my/jdtls-path)))
  
  ;; Don't prompt for server edits - just apply them
  (advice-add 'eglot--apply-workspace-edit :around
              (lambda (fn &rest args)
                (let ((yes-or-no-p (lambda (&rest _) t)))
                  (apply fn args)))))

;; Additional eglot keybindings (alternative method if :bind doesn't work)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l d") 'eldoc-doc-buffer)
  (define-key eglot-mode-map (kbd "C-c l o") 'eglot-code-action-organize-imports))

;; Java mode configuration
(use-package cc-mode
  :ensure nil
  :mode ("\\.java\\'" . java-mode)
  :bind (:map java-mode-map
              ("C-c i" . my/java-insert-common-imports))
  :config
  ;; Java style preferences
  (setq c-basic-offset 4
        java-ts-mode-indent-offset 4)
  
  ;; Add common Java imports quickly
  (defun my/java-insert-common-imports ()
    "Insert commonly used Java imports."
    (interactive)
    (let ((imports '("java.util.*"
                    "java.io.*"
                    "java.nio.file.*"
                    "java.util.stream.*")))
      (save-excursion
        (goto-char (point-min))
        (search-forward "package " nil t)
        (forward-line 1)
        (dolist (import imports)
          (insert (format "import %s;\n" import)))))))

;; Maven/Gradle integration helper
(use-package compile
  :ensure nil
  :hook (java-mode . my/java-compile-command)
  :config
  ;; Custom compile commands for Java
  (defun my/java-compile-command ()
    "Set compile command based on project structure."
    (cond
     ((file-exists-p "pom.xml")
      (setq-local compile-command "mvn compile"))
     ((file-exists-p "build.gradle")
      (setq-local compile-command "gradle build"))
     (t
      (setq-local compile-command "javac *.java")))))

;; =================================================================
;; Project Management
;; =================================================================

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project" "pom.xml" "build.gradle")))

;; =================================================================
;; Eshell Configuration
;; =================================================================

(use-package eshell
  :ensure nil
  :config
  ;; Disable company in eshell (can be slow)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (company-mode -1)))
  
  ;; Eshell visual commands
  (setq eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more"))
  (setq eshell-destroy-buffer-when-process-dies t))

;; =================================================================
;; Org Mode
;; =================================================================

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-ellipsis " ▾"))

;; =================================================================
;; File Management & Dired
;; =================================================================

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh --group-directories-first"
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t))

;; =================================================================
;; Terminal Integration
;; =================================================================

;; Better terminal integration for i3wm
(use-package xterm-color
  :config
  ;; Use xterm-color for better color support
  (add-hook 'compilation-start-hook
            (lambda (proc)
              (when (process-live-p proc)
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (comint-output-filter
                    proc
                    (xterm-color-filter string))))))))

;; =================================================================
;; Keybindings
;; =================================================================

;; Global useful bindings
(global-set-key (kbd "C-c c") #'compile)
(global-set-key (kbd "C-c r") #'recompile)
(global-set-key (kbd "C-c e") #'eshell)
(global-set-key (kbd "C-c k") #'kill-this-buffer)

;; Window management
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; =================================================================
;; Final Message
;; =================================================================

(message "Emacs configuration loaded successfully!")

;;; init.el ends here
