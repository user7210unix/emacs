;;; my-modeline.el --- Minimal modeline -*- lexical-binding: t; -*-

(require 'my-settings)
(require 'my-icons)
(require 'cl-lib)
(require 'subr-x)

(defface my/modeline-dim '((t :inherit shadow)) "Secondary modeline text.")
(defface my/modeline-modified '((t :inherit warning :weight bold)) "Unsaved marker.")
(defface my/modeline-error '((t :inherit error)) "Diagnostics errors.")
(defface my/modeline-warning '((t :inherit warning)) "Diagnostics warnings.")
(defface my/modeline-name '((t :weight bold)) "Buffer name.")

(defconst my/diagnostic-kinds
  '((error   . (:error eglot-error flymake-error))
    (warning . (:warning eglot-warning flymake-warning))))

(defun my/modeline-state ()
  (cond (buffer-read-only (my/icon #'nerd-icons-faicon "nf-fa-lock"))
        ((buffer-modified-p) (propertize "●" 'face 'my/modeline-modified))
        (t (propertize "○" 'face 'my/modeline-dim))))

(defun my/modeline-name ()
  (propertize (buffer-name) 'face 'my/modeline-name
              'help-echo (or buffer-file-name (buffer-name))))

(defun my/modeline-position ()
  (propertize "%l:%c" 'face 'my/modeline-dim))

(defun my/modeline-language ()
  (propertize (my/buffer-language) 'face 'my/modeline-dim))

(defun my/modeline-branch ()
  (when vc-mode
    (let ((branch (replace-regexp-in-string
                   "\\`[ ]*[^-:@]+[-:@]" "" (substring-no-properties vc-mode))))
      (concat (my/icon #'nerd-icons-octicon "nf-oct-git_branch") " "
              (propertize (truncate-string-to-width branch my/modeline-branch-max nil nil "…")
                          'face 'my/modeline-dim)))))

(defun my/diagnostic-count (kind)
  (let ((types (alist-get kind my/diagnostic-kinds)))
    (cl-count-if (lambda (d) (memq (flymake-diagnostic-type d) types))
                 (flymake-diagnostics))))

(defun my/modeline-counter (icon count face)
  (concat (my/icon #'nerd-icons-faicon icon) " " (propertize (number-to-string count) 'face face)))

(defun my/modeline-diagnostics ()
  (when (bound-and-true-p flymake-mode)
    (let ((errors (my/diagnostic-count 'error))
          (warnings (my/diagnostic-count 'warning)))
      (my/modeline-join
       (and (> errors 0)
            (my/modeline-counter "nf-fa-times_circle" errors 'my/modeline-error))
       (and (> warnings 0)
            (my/modeline-counter "nf-fa-exclamation_triangle" warnings 'my/modeline-warning))))))

(defun my/modeline-lsp ()
  (when (bound-and-true-p eglot--managed-mode)
    (my/icon #'nerd-icons-faicon "nf-fa-bolt")))

(defun my/modeline-join (&rest segments)
  (mapconcat #'identity (delq nil (delete "" segments)) "  "))

(defun my/modeline-left ()
  (my/modeline-join (concat " " (my/modeline-state))
                    (my/buffer-icon)
                    (my/modeline-name)
                    (my/modeline-position)))

(defun my/modeline-right ()
  (concat (my/modeline-join (my/modeline-diagnostics)
                            (my/modeline-lsp)
                            (my/modeline-branch)
                            (my/modeline-language))
          "  "))

(defun my/modeline-render ()
  (let* ((right (my/modeline-right))
         (gap (propertize " " 'display
                          `(space :align-to (- right ,(+ (string-width right)
                                                         my/modeline-right-slack))))))
    (concat (my/modeline-left) gap right)))

(defun my/modeline-pad-faces (&rest _)
  (dolist (face '(mode-line mode-line-inactive))
    (let ((bg (face-background face nil 'default)))
      (set-face-attribute face nil :box `(:line-width ,my/modeline-padding :color ,bg)))))

(setq-default mode-line-format '("%e" (:eval (my/modeline-render))))

(add-hook 'enable-theme-functions #'my/modeline-pad-faces)
(my/modeline-pad-faces)

(provide 'my-modeline)
;;; my-modeline.el ends here
