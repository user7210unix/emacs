;;; my-icons.el --- One icon API for modeline, sidebar and completion -*- lexical-binding: t; -*-

(require 'my-settings)
(require 'subr-x)

(use-package nerd-icons
  :demand t)

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config (nerd-icons-completion-mode 1))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(defun my/icon (fn name &rest args)
  "Call icon FN with NAME, returning \"\" when the glyph is unknown."
  (or (ignore-errors (apply fn name args)) ""))

(defun my/eaf-app-name (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'eaf-mode)
         (bound-and-true-p eaf--buffer-app-name))))

(defun my/eaf-icon (app height)
  (pcase (assoc app my/eaf-app-icons)
    (`(,_ ,fn ,name) (funcall fn name :height height))))

(defun my/buffer-icon (&optional buffer height)
  "Nerd icon string for BUFFER, empty when no icon applies."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((height (or height my/modeline-icon-height))
           (icon (ignore-errors
                   (cond ((my/eaf-app-name) (my/eaf-icon (my/eaf-app-name) height))
                         (buffer-file-name
                          (nerd-icons-icon-for-file buffer-file-name :height height))
                         (t (nerd-icons-icon-for-mode major-mode :height height))))))
      (if (stringp icon) icon ""))))

(defun my/buffer-language ()
  "Human readable name of the current major mode."
  (let ((name (if (consp mode-name) (car mode-name) mode-name)))
    (string-trim (if (stringp name) name (format-mode-line name)))))

(provide 'my-icons)
;;; my-icons.el ends here
