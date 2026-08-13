;;; init.el --- Main configuration entry point -*- lexical-binding: t; -*-

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

;;; .. "Operating system" chrome: strip every bit of chrome Emacs ships with
;;; so the frame reads as an app shell, not a text editor with menus bolted
;;; on (this is the same instinct behind nano-emacs / emacs-solo / sunroom).

(setq inhibit-startup-screen        t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message       nil   ; no more "This buffer is for text..."
      frame-resize-pixelwise        t
      window-resize-pixelwise       t
      use-dialog-box                nil
      use-file-dialog                nil
      x-gtk-use-system-tooltips      nil)

(menu-bar-mode   -1)
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Generous, symmetric fringes read as "margin", not "gutter"
(fringe-mode '(16 . 16))
(setq-default cursor-type 'bar)
(blink-cursor-mode -1)

;; Protesilaos' spacious-padding — internal padding around the header/mode
;; line and window edges, the single biggest lever for making Emacs stop
;; looking like a cramped text editor and start looking like an app.
(use-package spacious-padding
  :demand t
  :custom
  (spacious-padding-widths
   '(:internal-border-width 20
     :header-line-width 4
     :mode-line-width 2
     :tab-width 4
     :right-divider-width 20
     :scroll-bar-width 0
     :fringe-width 16))
  :config (spacious-padding-mode 1))

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

;; Line numbers ONLY where they earn their keep: C / C++ / Java "class"
;; buffers and Org outlines. Terminals, dired, the minibuffer, shells, etc.
;; stay clean — no global text-mode/prog-mode hook anymore.
(setq display-line-numbers-type t)
(dolist (hook '(c-mode-hook c++-mode-hook rust-mode-hook elisp-mode-hook java-mode-hook org-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;;; .. Theme — lambda-themes (dark, faded palette) ..........................
;;
;; Not on MELPA, so install it straight from source with Emacs' built-in
;; package-vc machinery (Emacs 29+) via use-package's `:vc' keyword.
;;
;; NOTE: `:vc' requires Emacs 29's `package-vc' plus a use-package new
;; enough to understand the keyword (bundled with Emacs 30, or
;; `M-x package-install RET use-package RET' on 29 to get a recent one).
;; If `:vc' errors out on your build, replace it with
;; `(package-vc-install "https://codeberg.org/Lambda-Emacs/lambda-themes")`
;; run once interactively, and drop the `:vc' line from these two blocks.

(use-package lambda-themes
  :vc (:url "https://codeberg.org/Lambda-Emacs/lambda-themes" :rev :newest)
  :demand t
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch  t)
  :config
  (load-theme 'lambda-dark t))

;; Background transparency — 92% opaque
(add-to-list 'default-frame-alist '(alpha-background . 92))
(set-frame-parameter nil 'alpha-background 92)

;;; .. Modeline — lambda-line, moved to the TOP as a header-line ...........
;;
;; Intentionally NOT the README's stock recipe: prefix/status symbols,
;; segment set, and spacing below are our own take, and `my/lambda-line-*`
;; below adds a project-aware segment + a dedicated dired-mode format so it
;; reads more like a lightweight titlebar than a classic Emacs modeline.

(use-package lambda-line
  :vc (:url "https://codeberg.org/Lambda-Emacs/lambda-line" :rev :newest)
  :demand t
  :after lambda-themes
  :custom
  (lambda-line-position           'top)   ; header-line, not footer
  (lambda-line-abbrev             t)      ; short major-mode names
  (lambda-line-hspace             "  ")
  (lambda-line-prefix             t)
  (lambda-line-prefix-padding     nil)
  (lambda-line-status-invert      nil)
  (lambda-line-gui-ro-symbol      " ⨯")
  (lambda-line-gui-mod-symbol     " ●")
  (lambda-line-gui-rw-symbol      " ◦")
  (lambda-line-space-top          0.15)
  (lambda-line-space-bottom       -0.15)
  (lambda-line-symbol-position    0.1)
  (lambda-line-icon-time          nil)    ; skip the ClockFace-font dependency
  (lambda-line-github-notifications nil)
  :config
  (lambda-line-mode)
  ;; When the status-line lives up top, collapse the real mode-line to a
  ;; hairline divider so there's no duplicate information at the bottom.
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (set-face-attribute 'mode-line nil :height 0.1)
    (set-face-attribute 'mode-line-inactive nil :height 0.1))

  ;; NOTE: Dired sets its own buffer-local `header-line-format' (see the
  ;; Dired section below), which naturally takes precedence over
  ;; lambda-line's global default there — no extra wiring needed.
  )

;;; .. Font ..................................................................
;;
;; Global UI font = Plus Jakarta Sans (mode-line, dired, minibuffer, dashboard
;; — anything that should feel like app chrome, not a text buffer).
;; Iosevka is reserved for the one place that *is* a text buffer: actual
;; code-editing windows (`prog-mode`).  Dired therefore inherits the UI
;; font automatically since it is never remapped to Iosevka.

(defvar my/ui-font   "Plus Jakarta Sans")
(defvar my/code-font "Iosevka")

;; `default' is the UI font. Every buffer starts here (incl. Dired, eww,
;; magit, the minibuffer, etc.) — code buffers opt OUT via the hook below.
(when (member my/ui-font (font-family-list))
  (set-face-attribute 'default nil
                      :family my/ui-font
                      :height 138
                      :weight 'light))

;; Fixed-pitch face stays monospace so tables/alignment never break even
;; inside a variable-pitch UI buffer.
(when (member my/code-font (font-family-list))
  (set-face-attribute 'fixed-pitch nil :family my/code-font :weight 'light))

;; Code-editing buffers: remap the LOCAL `default' face to Iosevka. Using
;; `face-remap-add-relative' (buffer-local) instead of a global
;; `set-face-attribute' is what lets Dired keep the UI font while prog-mode
;; buffers get the coding font — same face symbol, different buffers.
(defun my/use-code-font ()
  (when (member my/code-font (font-family-list))
    (face-remap-add-relative 'default :family my/code-font :height 142 :weight 'light)))

(dolist (hook '(prog-mode-hook conf-mode-hook yaml-mode-hook eat-mode-hook eshell-mode-hook))
  (add-hook hook #'my/use-code-font))

;; UI chrome faces — mode-line/header-line, minibuffer, tabs — always the
;; UI font, never overridden by the code-font remap above.
(when (member my/ui-font (font-family-list))
  (dolist (face '(mode-line mode-line-inactive mode-line-buffer-id
                  header-line tab-line tab-bar minibuffer-prompt
                  completions-annotations))
    (set-face-attribute face nil :family my/ui-font :weight 'light :height 122)))

;; lambda-line draws its own face family (lambda-line-*); catch all of them
;; generically so we don't have to hand-enumerate every segment face and
;; they stay in sync if the package adds more in a future release.
(with-eval-after-load 'lambda-line
  (when (member my/ui-font (font-family-list))
    (dolist (face (face-list))
      (when (string-prefix-p "lambda-line" (symbol-name face))
        (set-face-attribute face nil :family my/ui-font :height 118)))))

;; Italic faces
(set-face-attribute 'italic nil :slant 'italic)

;; Ligatures for the code font (Iosevka ships a large ligature set) —
;; arrows, comparisons, etc. render as single glyphs in prog-mode buffers.
(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 'prog-mode
                          '("->" "->>" "<-" "<-<" "<->" "=>" "=>>" "<=" "<=>"
                            "==" "===" "!=" "!==" "<=" ">=" "&&" "||" "::" ":::"
                            "..." ".." "?." "?:" "//" "/*" "*/" "++" "--" ">>="
                            "<<=" "|>" "<|" "<|>" "www"))
  (global-ligature-mode t))

;;; .. Startup dashboard (nano-emacs' nano-splash idea, own implementation)
;;
;; A centered, chrome-free "boot screen" instead of *scratch* or the GNU
;; startup buffer: mark, a couple of stats, and one-key launchers into the
;; workflows this config actually optimizes for (project files, recent
;; files, terminal, Dired-as-file-manager). No mode-line, no line numbers,
;; no fringes — as close to a splash screen as a text buffer gets.

(defvar my/dashboard-buffer-name "*dashboard*")

(defun my/dashboard--center (s &optional width)
  "Pad string S with leading spaces so it sits in the middle of WIDTH."
  (let* ((width (or width (window-width)))
         (pad   (max 0 (/ (- width (string-width s)) 2))))
    (concat (make-string pad ?\s) s)))

(defun my/dashboard--insert-action (key label fn)
  (insert (my/dashboard--center
           (concat (propertize (format "  %s  " key) 'face 'bold)
                   (propertize label 'face 'shadow))))
  (local-set-key (kbd key) fn)
  (insert "\n"))

(defun my/dashboard-refresh (&optional _)
  "(Re)draw the dashboard buffer contents."
  (with-current-buffer (get-buffer-create my/dashboard-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dotimes (_ (max 2 (/ (window-height) 4))) (insert "\n"))
      (insert (my/dashboard--center
               (propertize "λ" 'face '(:height 3.0 :weight thin))) "\n\n")
      (insert (my/dashboard--center
               (propertize "GNU Emacs" 'face '(:height 1.4 :weight light)))
              "\n")
      (insert (my/dashboard--center
               (propertize (format-time-string "%A, %-d %B %Y")
                          'face '(:height 0.95 :weight light :inherit shadow)))
              "\n\n\n")
      (my/dashboard--insert-action "f" "Find file"     #'find-file)
      (my/dashboard--insert-action "r" "Recent files"  #'consult-recent-file)
      (my/dashboard--insert-action "p" "Project"       #'project-switch-project)
      (my/dashboard--insert-action "d" "Files (Dired)" (lambda () (interactive) (dired "~")))
      (my/dashboard--insert-action "t" "Terminal"      #'my/eat-toggle-right)
      (my/dashboard--insert-action "q" "Quit Emacs"    #'save-buffers-kill-terminal)
      (goto-char (point-min)))))

(define-derived-mode my/dashboard-mode special-mode "Dashboard"
  "Major mode for the startup dashboard."
  (setq-local mode-line-format         nil
              header-line-format       nil
              display-line-numbers     nil
              cursor-type              nil
              buffer-read-only         t
              truncate-lines           t)
  (buffer-disable-undo))

(defun my/dashboard ()
  "Open (or refresh) the startup dashboard buffer."
  (interactive)
  (let ((buf (get-buffer-create my/dashboard-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'my/dashboard-mode) (my/dashboard-mode)))
    (my/dashboard-refresh)
    (switch-to-buffer buf)))

(setq initial-buffer-choice #'my/dashboard)
;; Keep the centering correct if the frame gets resized while it's open —
;; but only redraw when the dashboard is actually the buffer in view.
(add-hook 'window-size-change-functions
          (lambda (_frame)
            (let ((buf (get-buffer my/dashboard-buffer-name)))
              (when (and buf (get-buffer-window buf))
                (my/dashboard-refresh)))))

;;; .. Frame title (project — buffer, on the Linux desktop border) .........
(setq frame-title-format
      '(:eval
        (let* ((buf     (or (buffer-file-name) (buffer-name)))
               (name    (file-name-nondirectory buf))
               (proj    (and (fboundp 'project-current) (project-current)))
               (proj-nm (when proj
                         (file-name-nondirectory
                          (directory-file-name (project-root proj))))))
          (if proj-nm
              (format "%s — %s" proj-nm name)
            name))))
 
;;; .. NeoTree (file sidebar) ...............................................
(use-package neotree
  :demand t
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-theme                   (if (display-graphic-p) 'nerd-icons 'arrow)
        neo-window-width            28
        neo-window-fixed-size       nil   ; let it resize
        neo-show-hidden-files       t
        neo-auto-indent-point       t
        neo-smart-open              t     ; jump to current file on open
        neo-vc-integration          '(face char)  ; VCS state in sidebar
        neo-mode-line-type          'none ; lambda-line handles it
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

;; Icons in the minibuffer (Vertico candidates + Marginalia annotations) —
;; keeps the "everything has an icon" language consistent with Dired/Corfu.
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
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

;; Jump to (and remember) directories from anywhere — including recent
;; Dired locations — without leaving the minibuffer. Great companion to a
;; Dired-as-file-manager workflow.
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :bind (("C-."   . embark-act)
         ("M-."   . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult :after (embark consult))

;; Editable grep results (works great with `consult-ripgrep' + embark
;; export -> wgrep-mode -> C-c C-c to apply edits across files).
(use-package wgrep
  :after consult
  :custom (wgrep-auto-save-buffer t))

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

;; Corfu's popup is a child-frame trick that only works under GUI Emacs.
;; corfu-terminal renders the same popup with overlays when Emacs is
;; started with -nw, so completion still works in the Eat/terminal workflow.
(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config (corfu-terminal-mode 1))

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

;;; .. Navigation & UX niceties .............................................

;; Jump to any visible char/word/line in ~2 keystrokes instead of hjkl-ing
;; or hunting with the mouse.
(use-package avy
  :bind (("C-:"   . avy-goto-char-timer)
         ("C-'"   . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

;; Numbered window labels; C-x o still works but jumps straight to a
;; labelled window when there are 3+, and behaves like plain other-window
;; with only two — a strict upgrade over the old M-o binding.
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys      '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope     'frame)
  (aw-background t))

;; Nicer, cross-referenced *Help* buffers (buttons for source, references,
;; the symbol's face, etc.) — a near-mandatory quality-of-life swap.
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h o" . helpful-symbol)
         :map helpful-mode-map
         ("q" . kill-buffer-and-window)))

;; Subtle "here's where point/the change landed" flash instead of a hard
;; jump-cut — cheap eye-candy that also doubles as real signal.
(use-package pulsar
  :demand t
  :custom
  (pulsar-pulse            t)
  (pulsar-delay            0.05)
  (pulsar-iterations       8)
  (pulsar-face             'pulsar-generic)
  (pulsar-highlight-face   'pulsar-generic)
  :config
  (pulsar-global-mode 1)
  (dolist (fn '(ace-window avy-goto-char-timer avy-goto-line
                consult-line recenter-top-bottom))
    (add-hook (intern (format "%s-hook" fn)) #'pulsar-pulse-line)))

;; Treat the Eat terminal, compilation output, flycheck errors, etc. as
;; dismissable "popups" (like a Docked panel in an IDE / OS) instead of
;; permanent window splits.
(use-package popper
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*eat-terminal\\*" "\\*eshell\\*" "\\*compilation\\*"
     "\\*Flycheck errors\\*" "\\*Warnings\\*" "\\*Messages\\*"
     help-mode compilation-mode))
  (popper-display-control 'user)
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

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

;;; .. File management — Dired, as a Thunar/Nautilus replacement ...........
;;
;; Show ALL metadata: permissions, size (human-readable), date, owner.
;; We intentionally do NOT use dired-hide-details-mode so everything
;; is visible — columns are aligned by GNU coreutils ls.
;; Toggle details with ( if you ever need the clean view.
;;
;; The goal for the rest of this section: everything a lightweight GUI
;; file manager gives you — a path breadcrumb, a preview pane, live
;; filtering, inline git status, "open with the right app", a trash can,
;; and a thumbnail/icon grid view — without ever leaving Dired.

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
        dired-recursive-deletes                   'top
        dired-auto-revert-buffer                  t   ; always show fresh contents
        delete-by-moving-to-trash                 t)  ; real file managers have a trash can
  ;; Reuse the same dired buffer when going up/down directories
  (put 'dired-find-alternate-file 'disabled nil)

  ;; ---- Custom breadcrumb header-line -------------------------------
  ;; Clickable path segments + a live selection/item count, in the UI
  ;; font (Plus Jakarta Sans) — this is what makes a Dired window read as
  ;; a titlebar/address-bar instead of a listing of `ls -l` output.
  (defface my/dired-breadcrumb-segment
    '((t :inherit header-line :weight semi-bold))
    "Face for a clickable path segment in the Dired breadcrumb.")
  (defface my/dired-breadcrumb-sep
    '((t :inherit header-line :weight light))
    "Face for the separator between Dired breadcrumb segments.")
  (defface my/dired-breadcrumb-meta
    '((t :inherit header-line :weight light :height 0.9))
    "Face for the item-count / selection info on the right of the breadcrumb.")

  (defun my/dired-breadcrumb--segment-keymap (path)
    (let ((map (make-sparse-keymap)))
      (define-key map [header-line mouse-1]
        (lambda () (interactive) (dired path)))
      map))

  (defun my/dired-breadcrumb--path ()
    "Build clickable, icon-prefixed path segments for `default-directory'."
    (let* ((root  (expand-file-name default-directory))
           (parts (split-string (directory-file-name root) "/"))
           (acc   "")
           (crumbs nil))
      (dolist (p parts)
        (setq acc (concat acc p "/"))
        (push (propertize (if (string-empty-p p) "/" p)
                          'face 'my/dired-breadcrumb-segment
                          'mouse-face 'header-line-highlight
                          'keymap (my/dired-breadcrumb--segment-keymap acc)
                          'help-echo acc)
              crumbs))
      (mapconcat #'identity (nreverse crumbs)
                (propertize " › " 'face 'my/dired-breadcrumb-sep))))

  (defun my/dired-breadcrumb--meta ()
    "Item count, and selection count/size when a region is marked."
    (let* ((total (max 0 (1- (count-lines (point-min) (point-max)))))
           (marked (length (dired-get-marked-files))))
      (propertize
       (if (> marked 0)
           (format "%d / %d selected" marked total)
         (format "%d items" total))
       'face 'my/dired-breadcrumb-meta)))

  (defun my/dired-header-line ()
    (setq header-line-format
          '((:eval
             (let* ((crumb (my/dired-breadcrumb--path))
                    (meta  (my/dired-breadcrumb--meta))
                    (win   (window-width)))
               (concat "  " crumb
                      (propertize " " 'display
                                  `(space :align-to (- right ,(+ 2 (length meta)))))
                      meta))))))

  (add-hook 'dired-mode-hook #'my/dired-header-line)
  (add-hook 'dired-after-readin-hook
            (lambda () (when (derived-mode-p 'dired-mode) (force-mode-line-update))))

  ;; dired-x: omit-mode toggle for a "hide dotfiles" button, like Thunar's
  ;; Ctrl+H, instead of always showing everything via --almost-all.
  (require 'dired-x)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")
        dired-omit-verbose nil)
  ;; "C-c h" rather than the bare "H" — H is dired-do-hardlink by default
  ;; and we don't want to shadow it. "^" already goes up a directory, so
  ;; there's no need to touch "-" (negative-argument) for that.
  (define-key dired-mode-map (kbd "C-c h") #'dired-omit-mode)
  (define-key dired-mode-map (kbd "~") (lambda () (interactive) (dired "~"))))

;; Colourful dired — extra faces for permissions, dates, sizes, symlinks
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Nerd-icons in dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Subtree expansion (inline directory tree without opening new buffers) —
;; the closest Dired gets to a collapsible tree-view sidebar.
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-line-prefix "  │ ")   ; visual guide, matches highlight-indent-guides
  :bind (:map dired-mode-map
         ("<tab>"     . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle)))

;; Live preview pane on the right: point at a file, see it (text, image,
;; or PDF) without opening it — the single biggest "this feels like a
;; real file manager" upgrade.
(use-package dired-preview
  :after dired
  :custom
  (dired-preview-delay      0.2)
  (dired-preview-max-size   (* 20 1024 1024))
  :bind (:map dired-mode-map ("M-p" . dired-preview-mode)))

;; Type to filter the current listing in place — Thunar's "type to
;; search"/filter box, but instant and regex-capable.
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; Inline git status (modified/staged/untracked) per file, toggled on
;; demand so it doesn't slow down huge directories.
(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

;; "Open with the right application" — double-RET/`E` hands media,
;; archives, office docs, etc. off to xdg-open-style external handlers
;; instead of trying (and failing) to open a video as text.
(use-package dired-open
  :after dired
  :custom
  (dired-open-extensions
   '(("mkv"  . "mpv")  ("mp4"  . "mpv")  ("webm" . "mpv")
     ("mp3"  . "mpv")  ("flac" . "mpv")
     ("png"  . "xdg-open") ("jpg" . "xdg-open") ("jpeg" . "xdg-open")
     ("gif"  . "xdg-open") ("svg" . "xdg-open")
     ("pdf"  . "xdg-open")))
  :bind (:map dired-mode-map ("C-c o" . dired-open-file)))

;; Browse/restore the system trash can — pairs with
;; `delete-by-moving-to-trash' above so deleting in Dired is non-destructive
;; by default, exactly like a GUI file manager.
(use-package trashed
  :commands trashed
  :custom
  (trashed-action-confirmer  'y-or-n-p)
  (trashed-use-header-line   t)
  (trashed-sort-key          '("Date deleted" . t)))
(global-set-key (kbd "C-c t") #'trashed)

;; ;; Thunar-style toolbar: a discoverable transient menu of every Dired
;; ;; action (mark, copy, permissions, subdir ops...) bound to one key,
;; ;; instead of memorising two dozen single-letter commands.
;; (use-package casual-dired
;;   :after dired
;;   :bind (:map dired-mode-map ("C-c m" . casual-dired-tmenu)))

;; Icon/grid ("thumbnail") view for a folder of images — the Dired
;; equivalent of switching a file manager from list view to icon view.
(use-package image-dired
  :ensure nil
  :commands (image-dired image-dired-dired-toggle-marked-thumbs)
  :custom
  (image-dired-thumb-size 128)
  (image-dired-thumb-margin 4)
  (image-dired-thumbnail-storage 'standard)
  ;; "C-c i" rather than the tempting "z" or "i" — both are already taken
  ;; by dired-do-compress / dired-maybe-insert-subdir and we don't want to
  ;; shadow either.
  :bind (:map dired-mode-map ("C-c i" . image-dired)))

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
;; M-o -> ace-window (defined in the Navigation & UX niceties section)
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; <f4>  → eat terminal (defined above)
;; <f8>  → neotree-toggle (defined in neotree use-package :bind)
;; C-c n → neotree at project root (defined above)
;;
;; New in this revision (see their own use-package blocks for full context):
;;   M-o        ace-window (labelled window jump)
;;   C-:  C-'   avy-goto-char-timer / avy-goto-line
;;   C-h f/v/k/x/o   helpful-* (replaces plain describe-*)
;;   C-` M-` C-M-`   popper-toggle / -cycle / -toggle-type (docked popups)
;;   C-x C-d    consult-dir   |   C-x C-j  consult-dir-jump-file
;;   C-c t      trashed (browse the system trash)
;;   In Dired:  ~ home  |  /  narrow  |  M-p preview  |  )  git-info
;;              C-c h  toggle dotfiles  |  C-c o  open with external app
;;              C-c i  icon/thumbnail grid  |  C-c m  casual menu
;;              <tab>/<backtab>  subtree toggle/cycle

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
