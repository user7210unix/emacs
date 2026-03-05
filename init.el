;; Package System Setup

(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
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

(require 'bind-key)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)

;; Performance & Startup

;; Reset GC threshold after startup (set high in early-init.el)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Profile startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; General Settings

(setq-default
 ad-redefinition-action 'accept
 auto-save-default nil
 create-lockfiles nil
 fill-column 80
 help-window-select t
 indent-tabs-mode nil
 make-backup-files nil
 require-final-newline t
 tab-width 4
 truncate-lines nil
 vc-follow-symlinks t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(setq confirm-kill-emacs 'y-or-n-p)

(setq read-process-output-max (* 1024 1024)
      warning-minimum-level :error)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t))

(use-package recentf
  :ensure nil
  :demand t
  :config
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :demand t
  :config
  (setq history-length 100
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :demand t
  :config
  (save-place-mode 1))


;; Visual Settings

;; (internal-border-width . 0)

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'absolute
        display-line-numbers-width-start t))

(use-package paren
  :ensure nil
  :demand t
  :config
  (setq show-paren-delay 0
        show-paren-style 'mixed
        show-paren-when-point-inside-paren t)
  (show-paren-mode 1))

(use-package elec-pair
  :ensure nil
  :demand t
  :config
  (electric-pair-mode 1))

(use-package pixel-scroll
  :ensure nil
  :demand t
  :if (fboundp 'pixel-scroll-precision-mode)
  :config
  (pixel-scroll-precision-mode 1))

(setq scroll-margin 5
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; Font configuration

(when (member "CaskaydiaCove Nerd Font" (font-family-list))
  (set-face-attribute 'default nil
                      :family "CaskaydiaCove Nerd Font"
                      :height 125
                      :weight 'regular))

;; Italic comments — applied after theme loads
(defun my/set-italic-comments ()
  "Make comments italic."
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic))

;; (use-package moe-theme
;;   :ensure nil
;;   :demand t
;;   :config
;;   (load-theme 'moe-dark t)
;;   (my/set-italic-comments))

(use-package ef-themes
  :ensure nil
  :demand t
  :config
  (load-theme 'ef-elea-dark t)
  (my/set-italic-comments))


;; Frame defaults

(setq frame-title-format '("%b — Emacs"))
(setq default-frame-alist
      '((min-height . 1)
        (min-width  . 1)
        (internal-border-width . 0)
        (vertical-scroll-bars   . nil)
        (horizontal-scroll-bars . nil)))


;; Completion Framework — Vertico & Friends
(use-package vertico
  :demand t
  :config
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 12)
  (vertico-mode 1))

;; Floating posframe for M-x, find-file and all minibuffer commands.
;; Requires a graphical Emacs session (no-op in terminal).
(use-package vertico-posframe
  :demand t
  :after vertico
  :if (display-graphic-p)
  :config
  (setq vertico-posframe-width 100
        vertico-posframe-height vertico-count
        vertico-posframe-border-width 2
        vertico-posframe-poshandler #'posframe-poshandler-frame-center ;; centeres the floating minibuffers
        vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  (vertico-posframe-mode 1))

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

;; Nerd icons in the minibuffer completion list
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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


;; Programming — General

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Autocompletion — Corfu (replaces Company)
;;
;; Corfu is the modern choice: it uses Emacs' native completion-at-point API
;; directly, pairs perfectly with Eglot, and renders a clean childframe popup.

(use-package corfu
  :demand t
  :bind (:map corfu-map
         ("C-n"   . corfu-next)
         ("C-p"   . corfu-previous)
         ("<tab>" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-quit-no-match t)
  (corfu-on-exact-match 'insert)
  :config
  ;; Show doc popup next to completions (replaces company-quickhelp)
  (setq corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-popupinfo-mode 1)
  ;; Persist history across sessions
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  (global-corfu-mode 1))

;; Cape provides extra completion backends (dabbrev, file paths, etc.)
(use-package cape
  :demand t
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev 20)
  (add-hook 'completion-at-point-functions #'cape-file 20)
  (add-hook 'completion-at-point-functions #'cape-keyword 20))

;; Kind-icon adds VS Code-style icons to the Corfu popup
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Flycheck — Syntax checking

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.25))


;; Snippets

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)


;; EWW Browser

(use-package eww
  :ensure nil
  :bind (("C-c w" . eww)
         :map eww-mode-map
         ("h" . eww-list-histories)
         ("b" . eww-add-bookmark)
         ("B" . eww-list-bookmarks)
         ("o" . eww)
         ("&" . eww-browse-with-external-browser))
  :config
  ;; Readability
  (setq shr-use-fonts t               ; variable-pitch fonts for prose
        shr-use-colors nil             ; ignore garish site colors
        shr-indentation 4              ; left margin
        shr-width 80                   ; max line width, like your fill-column
        shr-max-image-proportion 0.5   ; images don't take over the buffer
        shr-inhibit-images nil)        ; images on by default, toggle with M-I

  ;; EWW itself
  (setq eww-search-prefix "https://duckduckgo.com/html/?q="
        eww-history-limit 100
        eww-download-directory (expand-file-name "~/Downloads/")
        eww-browse-url-new-window-is-tab nil)

  ;; Use variable-pitch (serif/sans) face for readability in eww buffers
  (add-hook 'eww-mode-hook #'variable-pitch-mode)

  ;; Auto-use eww's readable mode (strips nav, ads, sidebars) on page load.
  ;; Comment this out if you want the raw page instead.
  (add-hook 'eww-after-render-hook #'eww-readable))

;; Documentation popups — Eldoc Box

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  ;; Show the popup in the upper corner; use eldoc-box-hover-at-point-mode
  (eldoc-box-max-pixel-width  500)
  (eldoc-box-max-pixel-height 300)
  :config
  ;; Let Eglot render Markdown in the doc box (needs markdown-mode installed)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

;; Markdown mode for prettier Javadoc rendering inside eldoc-box
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))


;; Java Development with Eglot

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
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-sync-connect nil
        eglot-connect-timeout 10
        eglot-events-buffer-size 0
        eglot-confirm-server-initiated-edits nil)

  ;; JDTLS Configuration
  (defvar my/jdtls-path (expand-file-name "~/.local/share/jdtls"))
  (defvar my/jdtls-workspace-path (expand-file-name "~/.cache/jdtls-workspace"))

  (unless (file-directory-p my/jdtls-workspace-path)
    (make-directory my/jdtls-workspace-path t))

  (defun my/jdtls-find-launcher-jar ()
    "Find the Eclipse Equinox launcher jar in jdtls plugins directory."
    (let ((plugins-dir (expand-file-name "plugins" my/jdtls-path)))
      (when (file-directory-p plugins-dir)
        (let ((jars (directory-files plugins-dir t "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$")))
          (when jars (car jars))))))

  (defun my/jdtls-workspace-for-project ()
    "Return workspace directory for current project."
    (let* ((project-root (or (and (fboundp 'project-root)
                                   (when-let ((proj (project-current)))
                                     (project-root proj)))
                             default-directory))
           (project-name (file-name-nondirectory (directory-file-name project-root))))
      (expand-file-name project-name my/jdtls-workspace-path)))

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

  (advice-add 'eglot--apply-workspace-edit :around
              (lambda (fn &rest args)
                (let ((yes-or-no-p (lambda (&rest _) t)))
                  (apply fn args)))))

;; C++ via clangd — hook eglot into c++-mode as well
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd")))
  (add-hook 'c++-mode-hook #'eglot-ensure)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l d") 'eldoc-doc-buffer)
  (define-key eglot-mode-map (kbd "C-c l o") 'eglot-code-action-organize-imports))


;; Java mode configuration

(use-package cc-mode
  :ensure nil
  :mode (("\\.java\\'" . java-mode)
         ("\\.cpp\\'"  . c++-mode)
         ("\\.cc\\'"   . c++-mode)
         ("\\.h\\'"    . c++-mode))
  :bind (:map java-mode-map ("C-c i" . my/java-insert-common-imports))
  :config
  (setq c-basic-offset 4
        java-ts-mode-indent-offset 4)

  (defun my/java-insert-common-imports ()
    "Insert commonly used Java imports."
    (interactive)
    (let ((imports '("java.util.*" "java.io.*" "java.nio.file.*" "java.util.stream.*")))
      (save-excursion
        (goto-char (point-min))
        (search-forward "package " nil t)
        (forward-line 1)
        (dolist (import imports)
          (insert (format "import %s;\n" import)))))))

(use-package compile
  :ensure nil
  :hook (java-mode . my/java-compile-command)
  :config
  (defun my/java-compile-command ()
    "Set compile command based on project structure."
    (cond ((file-exists-p "pom.xml")      (setq-local compile-command "mvn compile"))
          ((file-exists-p "build.gradle") (setq-local compile-command "gradle build"))
          (t                              (setq-local compile-command "javac *.java")))))


;; Project Management

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project" "pom.xml" "build.gradle")))


;; Eshell Configuration

(use-package eshell
  :ensure nil
  :config
  (add-hook 'eshell-mode-hook (lambda () (corfu-mode -1)))
  (setq eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more")
        eshell-destroy-buffer-when-process-dies t))


;; Org Mode

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-ellipsis " ▾"))


;; Terminal — Eat

(use-package eat
  :config
  (setq eat-kill-buffer-on-exit t
        eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t
        eat-enable-shell-prompt-annotation t
        eat-term-scrollback-size nil)
  (with-eval-after-load 'eshell
    (eat-eshell-mode)))


;; File Management — Dired

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches    "-alh --group-directories-first"
        dired-dwim-target          t
        dired-kill-when-opening-new-dired-buffer t
        dired-hide-details-hide-symlink-targets nil))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))


;; Terminal Integration

(use-package xterm-color
  :config
  (add-hook 'compilation-start-hook
            (lambda (proc)
              (when (process-live-p proc)
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (comint-output-filter proc (xterm-color-filter string))))))))


;; Keybindings

(global-set-key (kbd "C-c c") #'compile)
(global-set-key (kbd "C-c r") #'recompile)
(global-set-key (kbd "C-c e") #'eshell)
(global-set-key (kbd "C-c k") #'kill-this-buffer)
(global-set-key (kbd "M-o")   #'other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)


(message "Emacs configuration loaded successfully!")

;;; init.el ends here
