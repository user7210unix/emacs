;;; my-scratch.el --- A scratch buffer worth opening -*- lexical-binding: t; -*-

(require 'my-settings)

(defvar my/scratch-hints
  '(("C-x b" . "buffers") ("F4" . "terminal") ("F7" . "files")
    ("F8" . "sidebar") ("F9" . "browser") ("F10" . "theme")))

(defun my/startup-seconds ()
  (float-time (time-subtract after-init-time before-init-time)))

(defun my/scratch-hint-line ()
  (mapconcat (lambda (hint) (format "%s %s" (car hint) (cdr hint)))
             my/scratch-hints "  ·  "))

(defun my/scratch-banner ()
  (format ";; Emacs %s  ·  %d packages  ·  ready in %.2fs\n;; %s\n\n"
          emacs-version (length package-activated-list)
          (my/startup-seconds) (my/scratch-hint-line)))

(defun my/scratch-decorate ()
  (with-current-buffer (get-scratch-buffer-create)
    (let ((overlay (make-overlay (point-min) (point-min) nil t)))
      (overlay-put overlay 'before-string
                   (propertize (my/scratch-banner) 'face 'font-lock-comment-face)))))

(setq initial-major-mode 'lisp-interaction-mode
      initial-scratch-message nil)

(use-package persistent-scratch
  :hook (after-init . persistent-scratch-setup-default))

(add-hook 'emacs-startup-hook #'my/scratch-decorate)

(provide 'my-scratch)
;;; my-scratch.el ends here
