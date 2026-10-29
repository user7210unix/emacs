;;; my-ui.el --- Theme and fonts -*- lexical-binding: t; -*-

(require 'my-settings)

(defconst my/ui-faces
  '(mode-line mode-line-inactive mode-line-buffer-id
    header-line tab-line minibuffer-prompt variable-pitch))

(defun my/font-available-p (family)
  (member family (font-family-list)))

(defun my/apply-fonts (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (my/font-available-p my/font-code)
      (set-face-attribute 'default nil :family my/font-code
                          :height my/font-code-height :weight 'regular)
      (set-face-attribute 'fixed-pitch nil :family my/font-code))
    (when (my/font-available-p my/font-ui)
      (dolist (face my/ui-faces)
        (set-face-attribute face nil :family my/font-ui
                            :height my/font-ui-height
                            :weight my/font-ui-weight)))))

(defun my/load-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (my/apply-fonts))

(defun my/theme-toggle ()
  (interactive)
  (let ((next (or (cadr (memq (car custom-enabled-themes) my/themes))
                  (car my/themes))))
    (my/load-theme next)
    (message "Theme: %s" next)))

(add-to-list 'custom-theme-load-path my/theme-dir)
(add-to-list 'load-path my/theme-dir)

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/apply-fonts)
  (add-hook 'emacs-startup-hook #'my/apply-fonts))

(with-demoted-errors "Theme: %S"
  (my/load-theme my/theme))

(setq frame-title-format '("%b"))
(global-set-key (kbd "<f10>") #'my/theme-toggle)

(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode '("<--" "-->" "=>" "->" "!=" "==" "===" "!==" ">=" "<="
                "&&" "||" "::" "..." "<>" "++" "//" "/*" "*/")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?│)
  (highlight-indent-guides-responsive 'top))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom (diff-hl-draw-borders nil))

(use-package magit
  :bind ("C-x g" . magit-status))

(provide 'my-ui)
;;; my-ui.el ends here
