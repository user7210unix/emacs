;;; my-org.el --- Org: appearance and workflow -*- lexical-binding: t; -*-

(require 'my-settings)
(require 'cl-lib)

(defun my/org-file (name)
  (expand-file-name name my/org-dir))

(defconst my/org-todo-faces
  '(("TODO" . (:inherit font-lock-keyword-face :weight bold))
    ("NEXT" . (:inherit font-lock-function-name-face :weight bold))
    ("PROG" . (:inherit warning :weight bold))
    ("WAIT" . (:inherit shadow))
    ("HOLD" . (:inherit shadow))
    ("DONE" . (:inherit font-lock-comment-face))
    ("KILL" . (:inherit font-lock-comment-face))))

(defconst my/org-priority-faces
  '((?A . (:inherit error :weight bold))
    (?B . (:inherit warning :weight bold))
    (?C . (:inherit success))))

(defun my/org-style-headings ()
  (cl-loop for scale in my/org-heading-scales
           for level from 1
           do (set-face-attribute (intern (format "org-level-%d" level)) nil
                                  :inherit 'variable-pitch :weight 'bold :height scale))
  (set-face-attribute 'org-document-title nil
                      :inherit 'variable-pitch :weight 'bold :height my/org-title-scale))

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory my/org-dir)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-use-fast-todo-selection 'expert)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-image-actual-width nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROG(p!)" "WAIT(w@/!)" "HOLD(h@)"
                                 "|" "DONE(d!)" "KILL(k@)")))
  (org-todo-keyword-faces my/org-todo-faces)
  (org-priority-faces my/org-priority-faces)
  (org-tag-alist '((:startgroup) ("@home" . ?h) ("@work" . ?w) ("@errands" . ?e) (:endgroup)
                   ("note" . ?n) ("idea" . ?i) ("review" . ?r) ("drill" . ?d)))
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  :config
  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t) (python . t) (js . t) (java . t) (C . t) (sql . t)))
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

(with-eval-after-load 'org
  (my/org-style-headings))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :custom (mixed-pitch-set-height t)
  :config
  (dolist (face '(org-block org-code org-table org-verbatim org-meta-line
                  org-tag org-property-value org-checkbox org-formula))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star ["◉" "○" "●" "○" "●"])
  (org-modern-table t)
  (org-modern-horizontal-rule t))

(provide 'my-org)
;;; my-org.el ends here
