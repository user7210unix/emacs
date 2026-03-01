;;; config-search.el --- Search  -*- lexical-binding: t; -*-

;;; Code:

(use-package isearch
  :ensure nil :straight nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-allow-scroll t)
  (isearch-repeat-on-direction-change t))

;; rg.el uses transient internals — defer until transient is fully loaded
(use-package rg
  :straight nil :ensure t
  :after transient
  :commands (rg rg-menu rg-project rg-dwim)
  :config
  (rg-enable-default-bindings))

(use-package wgrep
  :straight nil :ensure t)

(use-package affe
  :straight nil :ensure t
  :after orderless
  :config
  (setq affe-find-command "fd --color=never -H --type f")
  (consult-customize affe-grep :preview-key "M-.")
  :bind
  ("M-s F" . affe-find)
  ("M-s G" . affe-grep))

(use-package deadgrep
  :straight nil :ensure t
  :bind ("M-s d" . deadgrep))

(provide 'config-search)
;;; config-search.el ends here
