;; Vintage Emacs Configuration - Light Mode Classic Look

;; Package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install packages if not present (removed unavailable ones)
(defvar required-packages
  '(evil                    ; Vim keybindings
    helm                    ; Enhanced minibuffer completion
    company                ; Auto-completion
    vterm                  ; Terminal emulator
    which-key              ; Key binding hints
    projectile             ; Project management
    flycheck               ; Syntax checking
    lsp-mode               ; Language server protocol
    lsp-java               ; Java LSP
    yasnippet              ; Code snippets
    swiper                 ; Alternative to helm-swoop
    ))

;; Install packages with better error handling
(defun install-package-if-needed (package)
  "Install package if not already installed"
  (unless (package-installed-p package)
    (condition-case err
        (progn
          (unless package-archive-contents
            (package-refresh-contents))
          (package-install package)
          (message "Successfully installed %s" package))
      (error (message "Failed to install %s: %s" package err)))))

(mapcar 'install-package-if-needed required-packages)

;; Basic appearance settings - Vintage look
(menu-bar-mode -1)         ; Hide menu bar
(tool-bar-mode -1)         ; Hide toolbar
(scroll-bar-mode -1)       ; Hide scrollbar
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; Font configuration - Classic bitmap fonts
(defun set-vintage-font ()
  "Set a vintage bitmap font"
  (let ((fonts '("fixed"
                 "6x13"
                 "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1"
                 "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1"
                 "Monaco-10"
                 "Consolas-11"
                 "monospace-10")))
    (catch 'font-found
      (dolist (font fonts)
        (when (find-font (font-spec :name font))
          (set-frame-font font nil t)
          (throw 'font-found font)))
      ;; If no font found, use system default
      (message "No vintage font found, using system default"))))

(set-vintage-font)

;; LIGHT MODE Color scheme - exactly like the screenshot
(custom-set-faces
 ;; Main editor colors - light gray background, black text
 '(default ((t (:background "#e8e8e8" :foreground "#000000"))))
 '(cursor ((t (:background "#000000"))))
 '(region ((t (:background "#c0c0c0"))))
 
 ;; Mode line - darker gray
 '(mode-line ((t (:background "#c0c0c0" :foreground "#000000" :box (:line-width 1 :color "#888888")))))
 '(mode-line-inactive ((t (:background "#d0d0d0" :foreground "#666666" :box (:line-width 1 :color "#aaaaaa")))))
 
 ;; Minibuffer
 '(minibuffer-prompt ((t (:foreground "#000000" :weight bold))))
 
 ;; Syntax highlighting - subtle colors for light mode
 '(font-lock-comment-face ((t (:foreground "#008000" :slant italic))))     ; Green comments
 '(font-lock-string-face ((t (:foreground "#800080"))))                    ; Purple strings
 '(font-lock-keyword-face ((t (:foreground "#0000ff" :weight bold))))      ; Blue keywords
 '(font-lock-function-name-face ((t (:foreground "#8b0000" :weight bold)))) ; Dark red functions
 '(font-lock-type-face ((t (:foreground "#228b22" :weight bold))))         ; Forest green types
 '(font-lock-variable-name-face ((t (:foreground "#000000"))))             ; Black variables
 '(font-lock-constant-face ((t (:foreground "#008b8b"))))                  ; Dark cyan constants
 
 ;; Line numbers - like in screenshot
 '(line-number ((t (:foreground "#666666" :background "#e0e0e0"))))
 '(line-number-current-line ((t (:foreground "#000000" :background "#d0d0d0" :weight bold))))
 
 ;; Fringe (left margin)
 '(fringe ((t (:background "#e0e0e0"))))
 
 ;; Highlight current line - very subtle
 '(hl-line ((t (:background "#f0f0f0"))))
 
 ;; Parentheses matching
 '(show-paren-match ((t (:background "#ffff00" :weight bold))))
 '(show-paren-mismatch ((t (:background "#ff0000" :foreground "#ffffff" :weight bold)))))

;; Line numbers - old school style
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'absolute) ; Absolute line numbers like in screenshot
(setq display-line-numbers-width 4)

;; Evil mode (Vim keybindings)
(require 'evil)
(evil-mode 1)

;; Make Evil more vim-like
(setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)

;; Helm configuration - improved find and navigation (with error handling)
(when (require 'helm nil 'noerror)
  (helm-mode 1)
  
  ;; Key bindings
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  
  ;; Helm appearance for light mode
  (setq helm-display-header-line nil)
  (when (facep 'helm-source-header)
    (set-face-attribute 'helm-source-header nil :height 0.1))
  (custom-set-faces
   '(helm-selection ((t (:background "#c0c0c0" :foreground "#000000"))))
   '(helm-match ((t (:foreground "#ff0000" :weight bold))))
   '(helm-source-header ((t (:background "#b0b0b0" :foreground "#000000" :weight bold)))))
  (message "Helm loaded successfully"))

;; Use swiper for search (with error handling)
(when (require 'swiper nil 'noerror)
  (global-set-key (kbd "C-s") 'swiper)
  (message "Swiper loaded successfully"))

;; Fallback to built-in commands if packages aren't available
(unless (featurep 'helm)
  (message "Helm not available, using built-in alternatives")
  (global-set-key (kbd "C-x C-f") 'find-file)
  (global-set-key (kbd "C-x b") 'switch-to-buffer))

(unless (featurep 'swiper)
  (message "Swiper not available, using built-in search")
  (global-set-key (kbd "C-s") 'isearch-forward))

;; Company mode - autocompletion (with error handling)
(when (require 'company nil 'noerror)
  (global-company-mode 1)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  
  ;; Company colors for light mode
  (custom-set-faces
   '(company-preview ((t (:background "#d0d0d0" :foreground "#000000"))))
   '(company-preview-common ((t (:background "#d0d0d0" :foreground "#000000" :weight bold))))
   '(company-scrollbar-bg ((t (:background "#c0c0c0"))))
   '(company-scrollbar-fg ((t (:background "#888888"))))
   '(company-tooltip ((t (:background "#f0f0f0" :foreground "#000000" :box (:line-width 1 :color "#888888")))))
   '(company-tooltip-selection ((t (:background "#c0c0c0" :foreground "#000000"))))
   '(company-tooltip-common ((t (:foreground "#0000ff" :weight bold)))))
  (message "Company mode enabled"))

;; Which-key for key binding hints (with error handling)
(when (require 'which-key nil 'noerror)
  (which-key-mode 1)
  (setq which-key-popup-type 'minibuffer)
  (message "Which-key enabled"))

;; Projectile for project management (with error handling)
(when (require 'projectile nil 'noerror)
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (message "Projectile enabled"))

;; VTerm configuration
(when (require 'vterm nil 'noerror)
  (setq vterm-shell "/bin/bash")
  (global-set-key (kbd "C-c t") 'vterm))

;; Programming language support

;; C/C++ configuration
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

;; Java configuration
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

;; LSP Mode setup for modern IDE features (with error handling)
(when (require 'lsp-mode nil 'noerror)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'java-mode-hook #'lsp)
  (message "LSP mode enabled"))

;; LSP Java setup (with error handling)
(when (require 'lsp-java nil 'noerror)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (message "LSP Java enabled"))

;; Flycheck for syntax checking (with error handling)
(when (require 'flycheck nil 'noerror)
  (global-flycheck-mode)
  (message "Flycheck enabled"))

;; YASnippet for code snippets (with error handling)
(when (require 'yasnippet nil 'noerror)
  (yas-global-mode 1)
  (message "YASnippet enabled"))

;; Additional vintage customizations
(setq-default cursor-type 'block)        ; Block cursor like old terminals
(blink-cursor-mode 0)                    ; No blinking cursor
(setq ring-bell-function 'ignore)       ; No bell sound
(setq scroll-conservatively 100000)     ; Smooth scrolling
(setq scroll-preserve-screen-position t)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Highlight current line (very subtle)
(global-hl-line-mode 1)

;; Column indicator at 80 characters
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode 1)
(set-face-foreground 'fill-column-indicator "#cccccc")

;; Vintage status line
(setq-default mode-line-format
              '("%e" 
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; Disable auto-save and backup files for cleaner workspace
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Custom key bindings for vintage feel
(global-set-key (kbd "C-x C-b") 'ibuffer)  ; Better buffer list
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c g") 'goto-line)

;; Swiper configuration (replacement for helm-swoop)
(custom-set-faces
 '(swiper-line-face ((t (:background "#ffff00"))))
 '(swiper-match-face-1 ((t (:background "#c0c0c0"))))
 '(swiper-match-face-2 ((t (:background "#ffff00" :weight bold))))
 '(swiper-match-face-3 ((t (:background "#ff8c00" :weight bold))))
 '(swiper-match-face-4 ((t (:background "#ff1493" :weight bold)))))

;; Startup message
(defun vintage-startup-message ()
  "Display a vintage startup message"
  (with-current-buffer (get-buffer-create "*scratch*")
    (insert ";; Welcome to Vintage Emacs Configuration - Light Mode\n")
    (insert ";; Evil mode enabled - Vim keybindings active\n")
    (insert ";; C-c h for Helm commands\n")
    (insert ";; C-c t for terminal\n")
    (insert ";; C-s for search (swiper)\n\n")))

(add-hook 'emacs-startup-hook 'vintage-startup-message)

;; Save place in files
(save-place-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;; Window divider for that classic look
(window-divider-mode 1)
(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(set-face-foreground 'window-divider "#888888")
(set-face-foreground 'window-divider-first-pixel "#888888")
(set-face-foreground 'window-divider-last-pixel "#888888")

(message "configuration loaded successfully!")
