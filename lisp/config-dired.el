;;; config-dired.el --- Dired + Dirvish  -*- lexical-binding: t; -*-

;;; Code:

;; ─── dired (built-in) ────────────────────────────────────────────────────────
(use-package dired
  :ensure nil :straight nil
  :custom
  ;; macOS doesn't support --group-directories-first, handled per-OS in config-os
  (dired-listing-switches
   (if (eq my/os 'macos)
       "-lAh"
     "-lAh --group-directories-first --no-group"))
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-x
  :ensure nil :straight nil
  :after dired
  :config
  (setq dired-omit-verbose nil
        dired-omit-files   "^\\.\\|^#"))

;; ─── Nerd icons in dired ─────────────────────────────────────────────────────
(use-package nerd-icons-dired
  :straight nil :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; ─── Dirvish ─────────────────────────────────────────────────────────────────
(use-package dirvish
  :straight nil :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes
   '(nerd-icons vc-state git-msg file-time file-size collapse subtree-state))
  (dirvish-mode-line-format
   '(:left  (sort file-time " " file-size symlink)
     :right (omit yank index)))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
  (dirvish-header-line-height 42)
  (dirvish-side-width 34)
  (dirvish-default-layout '(0 0.4 0.6))
  (dirvish-quick-access-entries
   `(("h" "~/"           "  Home")
     ("d" "~/Downloads/" "  Downloads")
     ("p" "~/Projects/"  "  Projects")
     ("c" ,(pcase my/os
             ('linux   "~/.config/")
             ('macos   "~/.config/")
             ('windows (expand-file-name "~/AppData/Roaming/")))
      "  Config")
     ("e" ,(expand-file-name user-emacs-directory) "  Emacs")))
  :config
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
  :bind
  ("<f8>"  . dirvish-side)
  ("C-c e" . dirvish-side)
  (:map dirvish-mode-map
        ("q"   . dirvish-quit)
        ("TAB" . dirvish-subtree-toggle)
        ("a"   . dirvish-quick-access)
        ("f"   . dirvish-file-info-menu)
        ("y"   . dirvish-yank-menu)
        ("N"   . dirvish-narrow)
        ("^"   . dirvish-history-jump)
        ("s"   . dirvish-quicksort)
        ("v"   . dirvish-vc-menu)
        ("M-f" . dirvish-history-go-forward)
        ("M-b" . dirvish-history-go-backward)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-t" . dirvish-layout-toggle)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump)))

;; ─── Projectile ──────────────────────────────────────────────────────────────
(use-package projectile
  :straight nil :ensure t
  :diminish
  :init
  (setq projectile-project-search-path
        (list "~/Projects/"
              (pcase my/os
                ('macos   "~/Developer/")
                ('windows (expand-file-name "~/source/"))
                (_        "~/.config/")))
        projectile-switch-project-action #'dirvish
        projectile-completion-system     'default)
  :config
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package consult-projectile
  :straight nil :ensure t
  :after (consult projectile)
  :bind (:map projectile-command-map ("p" . consult-projectile)))

(provide 'config-dired)
;;; config-dired.el ends here
