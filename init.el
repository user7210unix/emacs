;;; init.el --- Main configuration entry point -*- lexical-binding: t; -*-
;;
;;  Loads:
;;    init.el          — this file (core, UI, packages)
;;    org-config.el    — Org-mode: appearance & workflow
;;    org-capture.el   — Capture templates & agenda
;;    org-export.el    — Export settings (HTML, PDF, reveal.js)

;;; .. Package bootstrap ....................................................

(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'bind-key)
  (package-install 'bind-key))

(eval-when-compile (require 'use-package))
(require 'bind-key)

(setq use-package-always-ensure t
      use-package-always-defer  t
      use-package-expand-minimally t)

;;; .. Performance & startup ................................................

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            (message "Emacs ready in %s with %d GCs."
                     (format "%.2fs"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; .. Defaults .............................................................

(setq-default
 ad-redefinition-action 'accept
 auto-save-default      nil
 create-lockfiles       nil
 fill-column            80
 help-window-select     t
 indent-tabs-mode       nil
 make-backup-files      nil
 require-final-newline  t
 tab-width              4
 truncate-lines         nil
 vc-follow-symlinks     t)

(setq scroll-conservatively 10
      scroll-margin         8
      read-process-output-max (* 1024 1024)
      warning-minimum-level :error
      ring-bell-function    'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file t))

;;; .. Built-in minor modes .................................................

(use-package recentf
  :ensure nil :demand t
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items  20
        recentf-auto-cleanup    'never)
  (recentf-mode 1))

(use-package savehist
  :ensure nil :demand t
  :config
  (setq history-length 200
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package saveplace :ensure nil :demand t
  :config (save-place-mode 1))

(use-package paren
  :ensure nil :demand t
  :config
  (setq show-paren-delay 0
        show-paren-style 'mixed
        show-paren-when-point-inside-paren t)
  (show-paren-mode 1))

(use-package elec-pair
  :ensure nil :demand t
  :config (electric-pair-mode 1))

;; Line numbers for code/text buffers
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type t)

;;; .. Font .................................................................

(when (member "JetBrainsMono Nerd Font" (font-family-list))
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 138
                      :weight 'regular)
  ;; Italic faces use a slanted variant so they look distinct
  (set-face-attribute 'italic nil :slant 'italic))

;;; .. Frame title (italic buffer name on Linux desktop border) .............
(setq frame-title-format
      '(:eval
        (let ((buf (or (buffer-file-name)
                       (buffer-name))))
          (format "𝘌𝘮𝘢𝘤𝘴 — %s" (file-name-nondirectory buf)))))

;;; .. Theme: doom-oceanic-next .............................................

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold   t
        doom-themes-enable-italic t)
  (load-theme 'doom-oceanic-next t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)          ; neotree integration
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;;; .. Modeline: nano-modeline ..............................................
(use-package nano-modeline
  :demand t
  :config
  ;; Position: bottom (default mode-line position)
  (setq nano-modeline-position 'nano-modeline-footer)
  ;; Register per-mode handlers
  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'dired-mode-hook           #'nano-modeline-prog-mode)
  (add-hook 'eat-mode-hook             #'nano-modeline-term-mode)
  (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  ;; Make prog the default for everything else
  (nano-modeline-prog-mode t))

;;; .. NeoTree (file sidebar) ...............................................
(use-package neotree
  :demand t
  :bind ("<f8>" . neotree-toggle)
  :config
  ;; Use nerd-icons (already a dep of doom-themes / nerd-icons-dired)
  (setq neo-theme                   (if (display-graphic-p) 'nerd-icons 'arrow)
        neo-window-width            28
        neo-window-fixed-size       nil   ; let it resize
        neo-show-hidden-files       t
        neo-auto-indent-point       t
        neo-smart-open              t     ; jump to current file on open
        neo-vc-integration          '(face char)  ; VCS state in sidebar
        neo-mode-line-type          'none ; nano-modeline handles it
        ;; Open neotree at project root, not CWD
        projectile-switch-project-action 'neotree-projectile-action))

;; Helper: open neotree at the project root (git root preferred)
(defun my/neotree-project-root ()
  "Open NeoTree at the current project or file's directory."
  (interactive)
  (let* ((project (when (fboundp 'project-current)
                    (project-current)))
         (root    (if project
                      (project-root project)
                    (file-name-directory (or (buffer-file-name)
                                             default-directory)))))
    (neotree-dir root)
    (neotree-find (buffer-file-name))))

(global-set-key (kbd "C-c n") #'my/neotree-project-root)

;;; .. Line height & whitespace .............................................

(setq-default line-spacing 0.2)

(use-package whitespace
  :ensure nil
  :custom
  (whitespace-display-mappings
   '((space-mark 32 [183] [46])
     (tab-mark    9 [187 9] [92 9])))
  (whitespace-style '(face spaces tabs trailing space-mark tab-mark))
  :init
  (setq whitespace-global-modes
        '(not text-mode org-mode markdown-mode bibtex-mode erc-mode
              magit-mode help-mode apropos-mode compilation-mode
              shell-mode eshell-mode vterm-mode eat-mode
              dired-mode wdired-mode message-mode))
  :config
  (global-whitespace-mode 1))

;;; .. Completion framework: Vertico + Orderless + Marginalia + Consult .....

(use-package vertico
  :demand t
  :config
  (setq vertico-cycle  t
        vertico-resize nil
        vertico-count  12)
  (vertico-mode 1))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t :after vertico
  :config (marginalia-mode 1))

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
  :bind (("C-."   . embark-act)
         ("M-."   . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult :after (embark consult))

;;; .. Programming — general ................................................

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method           'character
        highlight-indent-guides-character        ?│
        highlight-indent-guides-responsive       'top
        highlight-indent-guides-auto-enabled     t
        highlight-indent-guides-auto-odd-face-perc       5
        highlight-indent-guides-auto-even-face-perc      10
        highlight-indent-guides-auto-character-face-perc 15))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; .. Autocompletion — Corfu + Cape + kind-icons ...........................

(use-package corfu
  :demand t
  :bind (:map corfu-map
         ("C-n"   . corfu-next)
         ("C-p"   . corfu-previous)
         ("<tab>" . corfu-insert))
  :custom
  (corfu-cycle           t)
  (corfu-auto            t)
  (corfu-auto-delay      0.2)
  (corfu-auto-prefix     2)           ; 2 chars before popup (was 1, too eager)
  (corfu-preview-current nil)
  (corfu-min-width       30)
  (corfu-quit-no-match   t)
  (corfu-on-exact-match  nil)         ; ← fixes Java partial-match swallow
  :config
  (setq corfu-popupinfo-delay '(0.4 . 0.2))
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  (global-corfu-mode 1))

;; LSP kind icons in the Corfu popup (method/field/class/var indicators)
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape: extra completion backends
(use-package cape
  :demand t
  :config
  ;; For Java buffers: merge eglot capf with dabbrev so local identifiers
  ;; also appear.  cape-capf-super deduplicates and merges.
  (defun my/java-capf-setup ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-dabbrev)
                      #'cape-file)))
  (add-hook 'java-mode-hook #'my/java-capf-setup)

  ;; Global fallbacks
  (add-hook 'completion-at-point-functions #'cape-dabbrev 90)
  (add-hook 'completion-at-point-functions #'cape-file    90)
  (add-hook 'completion-at-point-functions #'cape-keyword 90))

;;; .. Syntax checking — Flycheck ...........................................

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay       0.25))

;;; .. Snippets .............................................................

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets :after yasnippet)

;;; .. EWW browser ..........................................................

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
  (setq shr-use-fonts            t
        shr-use-colors           nil
        shr-indentation          4
        shr-width                80
        shr-max-image-proportion 0.5
        shr-inhibit-images       nil
        eww-search-prefix        "https://duckduckgo.com/html/?q="
        eww-history-limit        100
        eww-download-directory   (expand-file-name "~/Downloads/")
        eww-browse-url-new-window-is-tab nil)
  (add-hook 'eww-mode-hook  #'variable-pitch-mode)
  (add-hook 'eww-after-render-hook #'eww-readable))

;;; .. Documentation — Eldoc Box ............................................

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-max-pixel-width  520)
  (eldoc-box-max-pixel-height 320)
  :config
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;;; .. LSP — Eglot ..........................................................

(use-package eglot
  :ensure nil
  :hook (java-mode . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c l d" . eldoc-doc-buffer)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l h" . eglot-inlay-hints-mode))
  :config
  (setq eglot-autoshutdown               t
        eglot-send-changes-idle-time     0.3
        eglot-sync-connect               nil
        eglot-connect-timeout            15
        eglot-events-buffer-size         0
        eglot-confirm-server-initiated-edits nil
        ;; Let JDTLS resolve imports etc. eagerly
        eglot-ignored-server-capabilities '())

  ;; JDTLS .................................................................
  (defvar my/jdtls-path        (expand-file-name "~/.local/share/jdtls"))
  (defvar my/jdtls-workspace   (expand-file-name "~/.cache/jdtls-workspace"))

  (unless (file-directory-p my/jdtls-workspace)
    (make-directory my/jdtls-workspace t))

  (defun my/jdtls-launcher-jar ()
    "Return the Eclipse Equinox launcher jar path or nil."
    (let ((plugins (expand-file-name "plugins" my/jdtls-path)))
      (when (file-directory-p plugins)
        (car (directory-files plugins t
                              "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$")))))

  (defun my/jdtls-workspace-dir ()
    "Per-project workspace directory."
    (let* ((root (or (when-let ((p (and (fboundp 'project-current)
                                        (project-current))))
                       (project-root p))
                     default-directory))
           (name (file-name-nondirectory (directory-file-name root))))
      (expand-file-name name my/jdtls-workspace)))

  (let ((jar  (my/jdtls-launcher-jar))
        (conf (expand-file-name "config_linux" my/jdtls-path)))
    (if (and jar (file-exists-p jar) (file-directory-p conf))
        (add-to-list 'eglot-server-programs
                     `(java-mode .
                       ("java"
                        "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                        "-Dosgi.bundles.defaultStartLevel=4"
                        "-Declipse.product=org.eclipse.jdt.ls.core.product"
                        "-Dlog.protocol=true" "-Dlog.level=ALL"
                        "-Xms256m" "-Xmx2048m"
                        "--add-modules=ALL-SYSTEM"
                        "--add-opens" "java.base/java.util=ALL-UNNAMED"
                        "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                        "-jar"           ,jar
                        "-configuration" ,conf
                        "-data"          ,(my/jdtls-workspace-dir))))
      (message "WARNING: JDTLS not found at %s" my/jdtls-path)))

  ;; Silence apply-workspace-edit confirmation dialog
  (advice-add 'eglot--apply-workspace-edit :around
              (lambda (fn &rest args)
                (let ((yes-or-no-p (lambda (&rest _) t)))
                  (apply fn args)))))

;; C / C++ via clangd
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd")))
  (add-hook 'c++-mode-hook #'eglot-ensure))

;;; .. Java mode ............................................................

(use-package cc-mode
  :ensure nil
  :mode (("\\.java\\'" . java-mode)
         ("\\.cpp\\'"  . c++-mode)
         ("\\.cc\\'"   . c++-mode)
         ("\\.h\\'"    . c++-mode))
  :bind (:map java-mode-map
         ("C-c i" . my/java-insert-common-imports))
  :config
  (setq c-basic-offset 4
        java-ts-mode-indent-offset 4)

  (defun my/java-insert-common-imports ()
    "Insert commonly used Java imports."
    (interactive)
    (let ((imports '("java.util.*" "java.io.*"
                     "java.nio.file.*" "java.util.stream.*")))
      (save-excursion
        (goto-char (point-min))
        (search-forward "package " nil t)
        (forward-line 1)
        (dolist (i imports)
          (insert (format "import %s;\n" i)))))))

(use-package compile
  :ensure nil
  :hook (java-mode . my/java-compile-setup)
  :config
  (defun my/java-compile-setup ()
    (cond
     ((file-exists-p "pom.xml")       (setq-local compile-command "mvn compile"))
     ((file-exists-p "build.gradle")  (setq-local compile-command "gradle build"))
     (t                               (setq-local compile-command "javac *.java")))))

;;; .. Project ..............................................................

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project" "pom.xml" "build.gradle")))

;;; .. Terminal — Eat .......................................................
;;
;; F4 → spawn (or jump to) an Eat terminal in a vertical split on the RIGHT.

(use-package eat
  :demand t
  :config
  (setq eat-kill-buffer-on-exit           t
        eat-enable-yank-to-terminal       t
        eat-enable-directory-tracking     t
        eat-enable-shell-command-history  t
        eat-enable-shell-prompt-annotation t
        eat-term-scrollback-size          nil)
  (with-eval-after-load 'eshell
    (eat-eshell-mode)))

(defun my/eat-toggle-right ()
  "Toggle an Eat terminal in a vertical split on the right side.
If the terminal buffer is already visible, close that window.
Otherwise open a new window to the right (≈30% of frame width)."
  (interactive)
  (let* ((buf-name "*eat-terminal*")
         (buf      (get-buffer buf-name))
         (win      (and buf (get-buffer-window buf))))
    (if win
        ;; Already visible → close it
        (delete-window win)
      ;; Not visible → open a right split
      (let* ((width (max 60 (/ (frame-width) 3)))
             (new-win (split-window-right (- width))))
        (select-window new-win)
        (if buf
            (switch-to-buffer buf)
          (eat))))))

(global-set-key (kbd "<f4>") #'my/eat-toggle-right)

;;; .. File management — Dired ..............................................
;;
;; Show ALL metadata: permissions, size (human-readable), date, owner.
;; We intentionally do NOT use dired-hide-details-mode so everything
;; is visible — columns are aligned by GNU coreutils ls.
;; Toggle details with ( if you ever need the clean view.

(use-package dired
  :ensure nil
  ;; No :hook dired-hide-details-mode — we want all columns visible
  :config
  (setq dired-listing-switches
        ;; -l long  -a all  -h human sizes  --time-style=long-iso  groups dirs first
        "--almost-all -l -h --time-style=long-iso --group-directories-first"
        dired-dwim-target                         t
        dired-kill-when-opening-new-dired-buffer  t
        dired-hide-details-hide-symlink-targets   nil
        dired-recursive-copies                    'always
        dired-recursive-deletes                   'top)
  ;; Reuse the same dired buffer when going up/down directories
  (put 'dired-find-alternate-file 'disabled nil))

;; Colourful dired — extra faces for permissions, dates, sizes, symlinks
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Nerd-icons in dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Subtree expansion (inline directory tree without opening new buffers)
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("<tab>"     . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle)))

;;; .. Eshell ...............................................................

(use-package eshell
  :ensure nil
  :config
  (add-hook 'eshell-mode-hook (lambda () (corfu-mode -1)))
  (setq eshell-visual-commands
        '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more")
        eshell-destroy-buffer-when-process-dies t))

;;; .. ANSI colour in compilation ...........................................

(use-package xterm-color
  :config
  (add-hook 'compilation-start-hook
            (lambda (proc)
              (when (process-live-p proc)
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (comint-output-filter
                    proc (xterm-color-filter string))))))))

;;; .. Keybindings ..........................................................

(global-set-key (kbd "C-c c")   #'compile)
(global-set-key (kbd "C-c r")   #'recompile)
(global-set-key (kbd "C-c e")   #'eshell)
(global-set-key (kbd "C-c k")   #'kill-this-buffer)
(global-set-key (kbd "M-o")     #'other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; <f4>  → eat terminal (defined above)
;; <f8>  → neotree-toggle (defined in neotree use-package :bind)
;; C-c n → neotree at project root (defined above)

;;; .. Load Org sub-configs .................................................

(defun my/load-config (file)
  "Load FILE from `user-emacs-directory', warn if missing."
  (let ((path (locate-user-emacs-file file)))
    (if (file-exists-p path)
        (load path nil t)
      (message "Config file not found: %s" path))))

(my/load-config "org-config.el")
(my/load-config "org-capture.el")
(my/load-config "org-export.el")

(message "init.el loaded.")

;;; init.el ends here
