(push '(tool-bar-lines . 0) default-frame-alist)
(push '(width . 80) default-frame-alist)
(push '(height . 46) default-frame-alist)

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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(swiper dashboard tabbar session pod-mode muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode company color-theme-modern browse-kill-ring boxquote bm bar-cursor apache-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "MS  " :slant normal :weight regular :height 150 :width normal)))))

(global-hl-line-mode 1)

(when (>= emacs-major-version 28) (fido-vertical-mode))

;; use flex match.
(setq completion-styles '(flex))

(setq save-abbrevs 'silently)

;;(load-theme 'misterioso t)

(set-background-color "#EEFFF0")


;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)  ; Changed from 'relative to t
(setq display-line-numbers-width-start t)

;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Better scrolling
(setq scroll-margin 8
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; set default tab char's display width to 4 spaces
;; default is 8
(setq-default tab-width 4)

;; make tab key always call a indent command.
(setq-default tab-always-indent t)

;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; auto close bracket insertion
(electric-pair-mode 1)


(when (< emacs-major-version 25)
  (require 'saveplace)
  (setq-default save-place t))

(when (>= emacs-major-version 25)
  (save-place-mode 1))



;; Remove initial scratch message and "For information about GNU Emacs and the 
;; GNU system, type C-h C-a"
(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message "")))

;; Allow y/n instead of having to type yes/no
(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

;; highlight matching paren
(show-paren-mode 1)

;; highlight entire expression
;;(setq show-paren-style 'expression)

;; company package
(use-package company)

;; save minibuffer history
(savehist-mode 1)

;; Restore opened files
(progn
  (desktop-save-mode 1)
  ;; save when quit
  (setq desktop-save t)
  ;; no ask if crashed
  (setq desktop-load-locked-desktop t)

  (setq desktop-restore-frames t)

  (setq desktop-auto-save-timeout 300)

  (setq desktop-globals-to-save nil)
)

