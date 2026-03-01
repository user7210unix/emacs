;;; config-options.el --- Core Emacs options  -*- lexical-binding: t; -*-

;;; Code:

;; ─── Performance ─────────────────────────────────────────────────────────────
(setq gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; ─── Clean UI ────────────────────────────────────────────────────────────────
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)
(set-fringe-mode  10)
(blink-cursor-mode -1)


;; ─── Ligatures (CaskaydiaCove NF supports full Cascadia Code ligature set) ───
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "&&" "^=" "~~" "~@" "~=" "~>"
     "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|" "[|"
     "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:" ">="
     ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:" "<$"
     "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!" "##"
     "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:" "?="
     "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  (global-ligature-mode t))

;; ─── Line numbers ────────────────────────────────────────────────────────────
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'absolute)
(dolist (hook '(org-mode-hook term-mode-hook eshell-mode-hook
                shell-mode-hook vterm-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;; ─── Editing defaults ────────────────────────────────────────────────────────
(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 80
              truncate-lines t)

(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

(delete-selection-mode 1)
(electric-pair-mode    1)
(global-auto-revert-mode 1)
(recentf-mode  1)
(savehist-mode 1)
(save-place-mode 1)

;; ─── Scrolling ───────────────────────────────────────────────────────────────
(setq scroll-margin 6
      scroll-conservatively 101
      scroll-preserve-screen-position t
      fast-but-imprecise-scrolling t)

;; ─── Frame defaults ──────────────────────────────────────────────────────────
(setq frame-title-format '("%b — Emacs"))
(setq default-frame-alist
      '((min-height . 1)
        (min-width  . 1)
        (internal-border-width . 12)
        (vertical-scroll-bars   . nil)
        (horizontal-scroll-bars . nil)))

;; ─── Encoding ────────────────────────────────────────────────────────────────
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)

(provide 'config-options)
;;; config-options.el ends here
