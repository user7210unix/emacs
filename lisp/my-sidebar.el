;;; my-sidebar.el --- Borderless buffer sidebar -*- lexical-binding: t; -*-

(require 'my-settings)
(require 'my-icons)
(require 'seq)
(require 'subr-x)

(defface my/sidebar-group '((t :inherit shadow :weight bold :height 0.85))
  "Group header in the sidebar.")
(defface my/sidebar-current '((t :inherit (bold font-lock-keyword-face)))
  "Buffer shown in the main window.")
(defface my/sidebar-modified '((t :inherit warning))
  "Unsaved marker.")

(defconst my/sidebar-buffer-name " *sidebar*")

(defvar my/sidebar-folded my/sidebar-folded-groups)
(defvar my/sidebar--timer nil)
(defvar my/sidebar--focus nil)

(defun my/buffer-system-p (buffer)
  (let ((name (buffer-name buffer)))
    (seq-some (lambda (re) (string-match-p re name)) my/buffer-hidden-patterns)))

(defun my/buffer-internal-p (buffer)
  (or (string-prefix-p " " (buffer-name buffer))
      (eq buffer (get-buffer my/sidebar-buffer-name))))

(defun my/sidebar--apps-p (buffer)
  (my/eaf-app-name buffer))

(defun my/sidebar--code-p (buffer)
  (with-current-buffer buffer
    (and buffer-file-name (derived-mode-p 'prog-mode))))

(defun my/sidebar--docs-p (buffer)
  (buffer-local-value 'buffer-file-name buffer))

(defconst my/sidebar-groups
  '((system "System" my/buffer-system-p)
    (apps   "Apps"   my/sidebar--apps-p)
    (code   "Code"   my/sidebar--code-p)
    (docs   "Docs"   my/sidebar--docs-p)
    (misc   "Misc"   always)))

(defun my/sidebar--group-of (buffer)
  (car (seq-find (lambda (group) (funcall (nth 2 group) buffer)) my/sidebar-groups)))

(defun my/sidebar--buckets ()
  (let ((buffers (seq-remove #'my/buffer-internal-p (buffer-list))))
    (mapcar (lambda (group)
              (cons group (seq-filter (lambda (b) (eq (my/sidebar--group-of b) (car group)))
                                      buffers)))
            my/sidebar-groups)))

(defun my/sidebar--insert-group (group buffers)
  (let* ((id (car group))
         (folded (memq id my/sidebar-folded)))
    (insert (propertize (format "%s %s  %d\n" (if folded "▸" "▾") (upcase (nth 1 group))
                                (length buffers))
                        'face 'my/sidebar-group 'my/sidebar-group id))
    (unless folded
      (dolist (buffer buffers)
        (my/sidebar--insert-buffer buffer)))))

(defun my/sidebar--insert-buffer (buffer)
  (let ((face (if (eq buffer my/sidebar--focus) 'my/sidebar-current 'default))
        (dirty (and (buffer-file-name buffer) (buffer-modified-p buffer))))
    (insert (propertize
             (concat " " (my/buffer-icon buffer my/sidebar-icon-height) " "
                     (propertize (buffer-name buffer) 'face face)
                     (if dirty (propertize " ●" 'face 'my/sidebar-modified) "")
                     "\n")
             'my/sidebar-buffer buffer))))

(defun my/sidebar-render ()
  (when-let ((buf (get-buffer my/sidebar-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (pcase-dolist (`(,group . ,buffers) (my/sidebar--buckets))
          (when buffers
            (my/sidebar--insert-group group buffers)))
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun my/sidebar--remember-focus ()
  (let ((buffer (window-buffer (selected-window))))
    (unless (my/buffer-internal-p buffer)
      (setq my/sidebar--focus buffer))))

(defun my/sidebar--schedule (&rest _)
  (when (get-buffer-window my/sidebar-buffer-name)
    (my/sidebar--remember-focus)
    (when (timerp my/sidebar--timer) (cancel-timer my/sidebar--timer))
    (setq my/sidebar--timer
          (run-with-idle-timer my/sidebar-refresh-delay nil #'my/sidebar-render))))

(defun my/sidebar--target-window ()
  (or (get-mru-window nil nil t) (selected-window)))

(defun my/sidebar-visit ()
  (interactive)
  (if-let ((buffer (get-text-property (point) 'my/sidebar-buffer)))
      (when (buffer-live-p buffer)
        (select-window (my/sidebar--target-window))
        (switch-to-buffer buffer))
    (my/sidebar-toggle-group)))

(defun my/sidebar-click (event)
  (interactive "e")
  (mouse-set-point event)
  (my/sidebar-visit))

(defun my/sidebar-toggle-group ()
  (interactive)
  (when-let ((id (get-text-property (line-beginning-position) 'my/sidebar-group)))
    (setq my/sidebar-folded (if (memq id my/sidebar-folded)
                                (remq id my/sidebar-folded)
                              (cons id my/sidebar-folded)))
    (my/sidebar-render)))

(defun my/sidebar-kill ()
  (interactive)
  (when-let ((buffer (get-text-property (point) 'my/sidebar-buffer)))
    (kill-buffer buffer)
    (my/sidebar-render)))

(defvar-keymap my/sidebar-mode-map
  :parent special-mode-map
  "RET" #'my/sidebar-visit
  "<mouse-1>" #'my/sidebar-click
  "TAB" #'my/sidebar-toggle-group
  "k" #'my/sidebar-kill
  "n" #'next-line
  "p" #'previous-line
  "g" #'my/sidebar-render
  "q" #'my/sidebar-hide)

(define-derived-mode my/sidebar-mode special-mode "Sidebar"
  (setq cursor-type nil
        truncate-lines t
        mode-line-format nil
        header-line-format nil
        line-spacing my/sidebar-line-spacing)
  (face-remap-add-relative 'default :family my/font-ui :height my/sidebar-text-scale)
  (hl-line-mode 1))

(defun my/sidebar--window-parameters ()
  '((no-other-window . t)
    (no-delete-other-windows . t)
    (mode-line-format . none)))

(defun my/sidebar-show ()
  (let ((buffer (get-buffer-create my/sidebar-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/sidebar-mode) (my/sidebar-mode)))
    (let ((window (display-buffer-in-side-window
                   buffer `((side . left) (slot . 0)
                            (window-width . ,my/sidebar-width)
                            (preserve-size . (t . nil))
                            (window-parameters . ,(my/sidebar--window-parameters))))))
      (set-window-fringes window 0 0)
      (set-window-dedicated-p window t))
    (my/sidebar--remember-focus)
    (my/sidebar-render)))

(defun my/sidebar-hide ()
  (interactive)
  (when-let ((window (get-buffer-window my/sidebar-buffer-name)))
    (delete-window window)))

(defun my/sidebar-toggle ()
  (interactive)
  (if (get-buffer-window my/sidebar-buffer-name)
      (my/sidebar-hide)
    (my/sidebar-show)))

(add-hook 'buffer-list-update-hook #'my/sidebar--schedule)
(add-hook 'window-selection-change-functions #'my/sidebar--schedule)
(add-hook 'after-save-hook #'my/sidebar--schedule)
(add-hook 'first-change-hook #'my/sidebar--schedule)

(global-set-key (kbd "<f8>") #'my/sidebar-toggle)

(provide 'my-sidebar)
;;; my-sidebar.el ends here
