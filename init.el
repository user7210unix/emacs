;; Basic UI settings
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(width . 80) default-frame-alist)
(push '(height . 46) default-frame-alist)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Package setup
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

;; Basic editor settings
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-hl-line-mode 1)
(savehist-mode 1)

;; Line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; Better scrolling
(setq scroll-margin 8
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Indentation
(setq-default tab-width 6
              indent-tabs-mode nil
              tab-always-indent 'complete)

;; Save place in files
(when (>= emacs-major-version 25)
  (save-place-mode 1))

;; Desktop save mode
(desktop-save-mode 1)
(setq desktop-save t
      desktop-load-locked-desktop t
      desktop-restore-frames t
      desktop-auto-save-timeout 300)

;; Completion with fido-mode (built-in, fast)
(when (>= emacs-major-version 28)
  (fido-vertical-mode)
  (setq completion-styles '(flex)))

;; Company for auto-completion
(use-package company)

;; LSP Mode for Java
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

;; LSP UI for better visuals (optional but recommended)
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil))

;; LSP Java
(use-package lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-vmargs
        '("-XX:+UseParallelGC"
          "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Xmx2G"
          "-Xms100m")))

;; Flycheck for syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Magit for git
(use-package magit
  :bind ("C-x g" . magit-status))


;; Modus Vivendi theme (built-in from Emacs 28+)
(load-theme 'modus-vivendi t)

(custom-set-faces
 '(default ((t (:family "Consolas" :foundry "MS  " :slant normal :weight regular :height 155 :width normal)))))

(defun display-startup-echo-area-message ()
  (message ""))
