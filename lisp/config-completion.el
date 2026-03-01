;;; config-completion.el --- Completion stack  -*- lexical-binding: t; -*-
;;; Commentary:
;; TAB = insert top candidate (VSCode/zsh feel)
;; completion-preview-mode = Emacs 30 built-in ghost text (all buffers)
;; Eshell: sane defaults only — history, pcomplete via corfu, NO autosuggest packages

;;; Code:

;; ─── Orderless ───────────────────────────────────────────────────────────────
(use-package orderless
  :straight nil :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file  (styles basic partial-completion))
                                   (eglot (styles orderless basic))))
  (completion-category-defaults nil))

;; ─── Vertico ─────────────────────────────────────────────────────────────────
(use-package vertico
  :straight nil :ensure t
  :init
  (setq vertico-cycle  t
        vertico-resize nil
        vertico-count  14)
  :config
  (vertico-mode 1))

;; Floating posframe picker for vertico (command-palette feel)
(use-package vertico-posframe
  :straight nil :ensure t
  :after (vertico posframe)
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-width   90)
  (vertico-posframe-height  20)
  (vertico-posframe-border-width 2)
  (vertico-posframe-parameters '((left-fringe  . 8)
                                 (right-fringe . 8)))
  :config
  (vertico-posframe-mode 1))

;; ─── Marginalia ──────────────────────────────────────────────────────────────
(use-package marginalia
  :straight nil :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

;; ─── Consult ─────────────────────────────────────────────────────────────────
(use-package consult
  :straight nil :ensure t
  :bind
  ("C-x b"   . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x r b" . consult-bookmark)
  ("M-s r"   . consult-ripgrep)
  ("M-s l"   . consult-line)
  ("M-s g"   . consult-grep)
  ("M-s f"   . consult-find)
  ("M-y"     . consult-yank-pop)
  ("M-g g"   . consult-goto-line)
  ("M-g i"   . consult-imenu)
  ("M-g o"   . consult-outline)
  ("C-s"     . consult-line)
  :custom
  (consult-preview-key '(:debounce 0.2 any))
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (when (executable-find "fd")
    (setq consult-find-command "fd --color=never --full-path ARG OPTS")))

(use-package embark
  :straight nil :ensure t
  :bind ("C-." . embark-act)
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight nil :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ─── Corfu — in-buffer popup ─────────────────────────────────────────────────
(use-package corfu
  :straight nil :ensure t
  :init
  ;; These MUST be in :init (before global-corfu-mode activates)
  (setq corfu-cycle           t
        corfu-auto            t          ; show popup automatically
        corfu-auto-delay      0.0        ; no delay — instant popup on type
        corfu-auto-prefix     1          ; trigger after just 1 character
        corfu-separator       ?\s        ; space separates orderless components
        corfu-quit-at-boundary nil       ; don't quit at word boundary
        corfu-quit-no-match   nil        ; keep popup even if no match yet
        corfu-preview-current t          ; preview top candidate inline
        corfu-preselect       'first     ; pre-select first candidate
        corfu-on-exact-match  nil        ; don't auto-insert on single match
        corfu-min-width       30
        corfu-max-width       90
        corfu-count           12
        corfu-scroll-margin   3
        corfu-popupinfo-delay '(0.2 . 0.1))
  (global-corfu-mode 1)   ; enable in :init so it's active immediately
  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode   1)
  :bind
  (:map corfu-map
        ("TAB"      . corfu-insert)
        ("<tab>"    . corfu-insert)
        ("RET"      . corfu-insert)       ; also accept with Enter
        ("C-n"      . corfu-next)
        ("C-p"      . corfu-previous)
        ("<up>"     . corfu-previous)
        ("<down>"   . corfu-next)
        ("C-g"      . corfu-quit)
        ("<escape>" . corfu-quit)
        ("M-SPC"    . corfu-insert-separator))) ; insert orderless separator

;; Nerd icons in the corfu popup
(use-package nerd-icons-corfu
  :straight nil :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ─── Cape — extra completion backends ───────────────────────────────────────
(use-package cape
  :straight nil :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; ─── Emacs 30 built-in ghost-text (all prog buffers) ─────────────────────────
(use-package emacs
  :ensure nil :straight nil
  :config
  (setq completion-preview-minimum-symbol-length 2)
  (global-completion-preview-mode 1)
  :bind
  (:map completion-preview-active-mode-map
        ("M-f"   . completion-preview-insert-word)
        ("C-M-f" . completion-preview-insert-sexp)
        ("C-e"   . completion-preview-insert)))

;; ─── Eshell — sane defaults, NO external autosuggest packages ────────────────
(use-package eshell
  :ensure nil :straight nil
  :hook (eshell-mode . my/eshell-setup)
  :custom
  (eshell-history-size         10000)
  (eshell-save-history-on-exit t)
  (eshell-hist-ignoredups      t)        ; no duplicate history entries
  (eshell-scroll-to-bottom-on-input t)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-prefer-lisp-functions nil)     ; prefer external commands when available
  :config
  ;; Nice prompt: user@host dir $ (or # for root)
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] "
        eshell-prompt-function
        (lambda ()
          (concat
           (propertize (user-login-name) 'face '(:foreground "#00d7ff" :weight bold))
           (propertize "@" 'face '(:foreground "#6c6c6c"))
           (propertize (system-name) 'face '(:foreground "#87d787"))
           " "
           (propertize (abbreviate-file-name (eshell/pwd))
                       'face '(:foreground "#d7af5f" :weight bold))
           " "
           (propertize (if (= (user-uid) 0) "#" "$")
                       'face '(:foreground "#ff5f5f" :weight bold))
           " "))))

(defun my/eshell-setup ()
  "Sane eshell buffer-local settings."
  ;; Use corfu for tab completion (pcomplete-based)
  (setq-local corfu-auto t
              corfu-auto-delay 0.05
              corfu-auto-prefix 1)
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'pcomplete-completions-at-point
                                     #'cape-history
                                     #'cape-file)))
  (corfu-mode 1)
  ;; M-r to search history with consult
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history)
  ;; Useful eshell aliases
  (eshell/alias "ff"   "find-file $1")
  (eshell/alias "d"    "dired $1")
  (eshell/alias "ll"   "ls -lAh $*")
  (eshell/alias "la"   "ls -A $*")
  (eshell/alias "gs"   "magit-status")
  (eshell/alias "clear" "eshell/clear-scrollback"))

;; ─── Yasnippet ───────────────────────────────────────────────────────────────
(use-package yasnippet
  :straight nil :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight nil :ensure t
  :after yasnippet)

(provide 'config-completion)
;;; config-completion.el ends here
