;;; my-eaf.el --- Emacs Application Framework: browser, terminal, files, viewers -*- lexical-binding: t; -*-

(require 'my-settings)
(require 'my-icons)
(require 'seq)

(add-to-list 'load-path my/eaf-dir)

(defun my/eaf-require-apps ()
  (dolist (app my/eaf-apps)
    (unless (require app nil t)
      (message "EAF app not installed: %s" app))))

(use-package eaf
  :ensure nil
  :if (file-directory-p my/eaf-dir)
  :demand t
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (eaf-browser-dark-mode "follow")
  (eaf-pdf-dark-mode "follow")
  (eaf-browser-default-search-engine my/eaf-search-engine)
  (eaf-pyqterminal-font-family my/font-code)
  (browse-url-browser-function #'eaf-open-browser)
  :config
  (my/eaf-require-apps)
  (setq eaf-mode-line-format (default-value 'mode-line-format))
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding))

(defun my/eaf-buffer (app)
  (seq-find (lambda (buffer) (equal (my/eaf-app-name buffer) app)) (buffer-list)))

(defun my/eaf-window (app)
  (seq-find (lambda (window) (equal (my/eaf-app-name (window-buffer window)) app))
            (window-list nil 'nomini)))

(defun my/terminal-toggle ()
  "Show the EAF terminal in a right split, or close it when visible."
  (interactive)
  (if-let ((window (my/eaf-window my/eaf-terminal-app)))
      (delete-window window)
    (select-window (split-window-right (- (round (* (frame-width) my/terminal-width)))))
    (if-let ((buffer (my/eaf-buffer my/eaf-terminal-app)))
        (switch-to-buffer buffer)
      (eaf-open-pyqterminal))))

(defun my/file-manager (&optional directory)
  "Tile the EAF file manager on the left, leaving the right for previews."
  (interactive)
  (delete-other-windows)
  (eaf-open (expand-file-name (or directory default-directory)) "file-manager"))

(defun my/eaf-buffer-p (buffer &rest _)
  (when-let ((buffer (get-buffer buffer)))
    (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'eaf-mode)))

(defun my/display-beside-file-manager (buffer _alist)
  "Show previewable EAF BUFFER in the pane right of the file manager."
  (when-let* ((app (my/eaf-app-name buffer))
              ((member app my/eaf-preview-apps))
              (manager (my/eaf-window "file-manager")))
    (let ((pane (or (window-in-direction 'right manager)
                    (split-window manager
                                  (round (* (window-total-width manager) my/file-manager-width))
                                  'right))))
      (set-window-buffer pane buffer)
      pane)))

(add-to-list 'display-buffer-alist
             '(my/eaf-buffer-p my/display-beside-file-manager))

(defun my/thumbnails ()
  "Thumbnail grid of the images in the current directory."
  (interactive)
  (image-dired default-directory))

(use-package image-dired
  :ensure nil
  :custom
  (image-dired-thumbnail-storage 'standard)
  (image-dired-thumb-size my/thumbnail-size))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "--almost-all -l -h --time-style=long-iso --group-directories-first")
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(defun my/dired-title-font ()
  "Names in the last column take the UI font without breaking column alignment."
  (dolist (face '(dired-header dired-directory dired-symlink
                  diredfl-dir-name diredfl-file-name))
    (when (facep face)
      (face-remap-add-relative face :family my/font-ui))))
(add-hook 'dired-mode-hook #'my/dired-title-font)

(bind-keys ("<f4>" . my/terminal-toggle)
           ("<f7>" . my/file-manager)
           ("<f9>" . eaf-open-browser)
           ("C-x d" . my/file-manager)
           ("C-c w" . eaf-open-browser)
           ("C-c T" . my/thumbnails))

(provide 'my-eaf)
;;; my-eaf.el ends here
