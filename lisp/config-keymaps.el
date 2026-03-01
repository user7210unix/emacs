;;; config-keymaps.el --- Keybindings  -*- lexical-binding: t; -*-

;;; Code:

;; ─── Files ───────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-c f f") #'find-file)
(global-set-key (kbd "C-c f r") #'consult-recent-file)
(global-set-key (kbd "C-c f s") #'save-buffer)
(global-set-key (kbd "C-c f S") #'write-file)

;; ─── Buffers ─────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-c b b") #'consult-buffer)
(global-set-key (kbd "C-c b d") #'kill-current-buffer)
(global-set-key (kbd "C-c b n") #'next-buffer)
(global-set-key (kbd "C-c b p") #'previous-buffer)
(global-set-key (kbd "C-c b r") #'revert-buffer)

;; ─── Windows ─────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-c w s") #'split-window-below)
(global-set-key (kbd "C-c w v") #'split-window-right)
(global-set-key (kbd "C-c w d") #'delete-window)
(global-set-key (kbd "C-c w D") #'delete-other-windows)
(global-set-key (kbd "C-c w h") #'windmove-left)
(global-set-key (kbd "C-c w j") #'windmove-down)
(global-set-key (kbd "C-c w k") #'windmove-up)
(global-set-key (kbd "C-c w l") #'windmove-right)
(global-set-key (kbd "C-c w =") #'balance-windows)
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w r") #'winner-redo)

;; ─── Search ──────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-s")   #'consult-line)        ; fuzzy in-buffer (like Ctrl+F)
(global-set-key (kbd "C-S-s") #'consult-ripgrep)     ; project-wide (like Ctrl+Shift+F)
(global-set-key (kbd "M-s r") #'consult-ripgrep)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s f") #'affe-find)

;; ─── Explorer / Minimap ──────────────────────────────────────────────────────
(global-set-key (kbd "C-c e")  #'dirvish-side)
(global-set-key (kbd "<f8>")   #'dirvish-side)
(global-set-key (kbd "C-c m")  #'demap-toggle)
(global-set-key (kbd "<f9>")   #'demap-toggle)

;; ─── VSCode-feel bindings ─────────────────────────────────────────────────────
(global-set-key (kbd "C-`")   #'eat)                        ; terminal
(global-set-key (kbd "C-S-p") #'execute-extended-command)   ; command palette
(global-set-key (kbd "C-p")   #'projectile-find-file)       ; quick file open
(global-set-key (kbd "C-S-e") #'dirvish-side)               ; sidebar
(global-set-key (kbd "C-\\")  #'split-window-right)          ; split editor
(global-set-key (kbd "C-w")   #'kill-current-buffer)         ; close tab

;; ─── Git ─────────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-x g")   #'magit-status)
(global-set-key (kbd "C-c g g") #'magit-status)
(global-set-key (kbd "C-c g b") #'magit-blame)
(global-set-key (kbd "C-c g l") #'magit-log-current)
(global-set-key (kbd "C-c g d") #'magit-diff-buffer-file)

;; ─── LSP ─────────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-c l a") #'eglot-code-actions)
(global-set-key (kbd "C-c l r") #'eglot-rename)
(global-set-key (kbd "C-c l f") #'eglot-format-buffer)
(global-set-key (kbd "C-c l i") #'eglot-find-implementation)
(global-set-key (kbd "C-c l s") #'consult-eglot-symbols)
(global-set-key (kbd "M-.")     #'xref-find-definitions)
(global-set-key (kbd "M-,")     #'xref-go-back)
(global-set-key (kbd "M-?")     #'xref-find-references)

;; ─── Diagnostics ─────────────────────────────────────────────────────────────
(global-set-key (kbd "C-c d n") #'flymake-goto-next-error)
(global-set-key (kbd "C-c d p") #'flymake-goto-prev-error)
(global-set-key (kbd "C-c d l") #'flymake-show-buffer-diagnostics)

;; ─── Terminal ────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-c t t") #'eat)
(global-set-key (kbd "C-c t e") #'eshell)

;; ─── Misc ─────────────────────────────────────────────────────────────────────
(global-set-key (kbd "C-c ;")   #'comment-line)
(global-set-key (kbd "C-c :")   #'comment-dwim)
(global-set-key (kbd "C-/")     #'undo-only)
(global-set-key (kbd "C-?")     #'undo-redo)

;; ─── Smart C-a ───────────────────────────────────────────────────────────────
(defun my/smart-bol ()
  "Toggle between first non-whitespace and true beginning of line."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (= pt (point)) (beginning-of-line))))
(global-set-key (kbd "C-a") #'my/smart-bol)

;; ─── Shift+arrows = windmove ─────────────────────────────────────────────────
(windmove-default-keybindings 'shift)

;; ─── Winner mode ─────────────────────────────────────────────────────────────
(winner-mode 1)

(provide 'config-keymaps)
;;; config-keymaps.el ends here
