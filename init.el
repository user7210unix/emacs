;; ----------------------------
;; Package Management (MELPA)
;; ----------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ----------------------------
;; Performance tweaks
;; ----------------------------
;; Speed startup: bump GC threshold
(setq gc-cons-threshold (* 50 1000 1000))

;; Defer GC while typing commands
(defun my/defer-gc ()
  (setq gc-cons-threshold (* 100 1000 1000)))
(defun my/restore-gc ()
  (setq gc-cons-threshold (* 50 1000 1000)))

(add-hook 'minibuffer-setup-hook #'my/defer-gc)
(add-hook 'minibuffer-exit-hook  #'my/restore-gc)

;; ----------------------------
;; Company Mode — Global
;; ----------------------------
(use-package company
  :defer 1
  :diminish company-mode
  :hook
  ;; Enable company in programming buffers
  ((prog-mode . company-mode)
   ;; explicitly *not* in terminal or shell
   (term-mode . (lambda () (company-mode -1)))
   (eshell-mode . (lambda () (company-mode -1))))
  :custom
  (company-idle-delay 0.2)        ;; POPUP after 0.2s
  (company-minimum-prefix-length 1) ;; start early
  (company-tooltip-limit 12)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-global-modes '(not term-mode eshell-mode))
  :config
  ;; <tab> completes common part, TAB navigation
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (global-company-mode 1))

;; Optional: company-files for path completion
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-dabbrev-code))

;; ----------------------------
;; Vertico + Consult + Orderless — Modern completion
;; ----------------------------
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package consult
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer))

;; ----------------------------
;; Simple UI Tweaks
;; ----------------------------
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      make-backup-files nil)

;; yes or no to y or n
(defalias 'yes-or-no-p 'y-or-n-p)


;; Frame window maximized size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; indent vertical lines
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\│
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t
        highlight-indent-guides-auto-odd-face-perc 5
        highlight-indent-guides-auto-even-face-perc 10
        highlight-indent-guides-auto-character-face-perc 20))


;; Clean fringe & other UI minimalism
(tool-bar-mode -1)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'absolute)

;; Background Color
(when (display-graphic-p)
  (push '(background-color . "honeydew") default-frame-alist))

;; highlight matching paren
(show-paren-mode 1)

;; auto close bracket insertion
(electric-pair-mode 1)

;; Smooth scrolling
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq scroll-margin 8
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Italic comments
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil :slant 'italic)

;; Line Numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; global-hl-line-mode
(setq global-hl-line-sticky-flag t)

;; ----------------------------
;; Scratch buffer on kill
;; ----------------------------
(defun my/ensure-scratch ()
  "Make *scratch* buffer if none."
  (unless (get-buffer "*scratch*")
    (with-current-buffer (get-buffer-create "*scratch*")
      (lisp-interaction-mode))))
(add-hook 'kill-buffer-hook #'my/ensure-scratch)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company consult highlight-indent-guides marginalia orderless vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
