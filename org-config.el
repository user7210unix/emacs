;;; org-config.el --- Org-mode: appearance & workflow -*- lexical-binding: t; -*-
;;
;; Loaded by init.el.  Covers:
;;   • Core Org settings & indentation
;;   • Typography / mixed-pitch fonts
;;   • Custom heading faces (doom-oceanic-next palette)
;;   • TODO keywords and priorities
;;   • Babel (literate programming)
;;   • Inline images

;;;  Core 

(use-package org
  :ensure nil
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :config

  ;;  Basics 
  (setq
   org-startup-indented        t    ; virtual indentation (org-indent-mode)
   org-startup-folded          'content  ; show first-level on open
   org-hide-emphasis-markers   t    ; hide *bold* markers, show result
   org-hide-leading-stars      t
   org-pretty-entities         t    ; LaTeX → Unicode (α β ∑)
   org-ellipsis                " ▾"
   org-use-fast-todo-selection 'expert
   org-log-done                'time ; timestamp when TODO → DONE
   org-log-into-drawer         t
   org-return-follows-link     t    ; RET opens links
   org-image-actual-width      nil  ; respect #+ATTR_ORG :width
   org-cycle-separator-lines   1
   org-src-fontify-natively    t    ; syntax-highlight src blocks
   org-src-tab-acts-natively   t
   org-confirm-babel-evaluate  nil
   org-edit-src-content-indentation 0)

  ;;  TODO keywords & faces 
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"   "NEXT(n)"  "PROG(p!)"
           "WAIT(w@/!)" "HOLD(h@)"
           "|"
           "DONE(d!)"  "KILL(k@)")))

  (setq org-todo-keyword-faces
        '(("TODO"  . (:foreground "#c594c5" :weight bold))   ; purple
          ("NEXT"  . (:foreground "#5fb3b3" :weight bold))   ; teal
          ("PROG"  . (:foreground "#fac863" :weight bold))   ; yellow
          ("WAIT"  . (:foreground "#a7adba" :weight normal)) ; grey
          ("HOLD"  . (:foreground "#a7adba" :weight normal))
          ("DONE"  . (:foreground "#6a9fb5" :weight normal)) ; faded blue
          ("KILL"  . (:foreground "#6a9fb5" :weight normal))))

  (setq org-priority-faces
        '((?A . (:foreground "#ec5f67" :weight bold))    ; red
          (?B . (:foreground "#fac863" :weight bold))    ; yellow
          (?C . (:foreground "#5fb3b3"))))               ; teal

  ;;  Tags 
  (setq org-tag-alist
        '((:startgroup)
          ("@home" . ?h) ("@work" . ?w) ("@errands" . ?e)
          (:endgroup)
          ("note" . ?n) ("idea" . ?i) ("review" . ?r) ("drill" . ?d)))

  ;;  Clocking 
  (setq org-clock-persist         'history
        org-clock-in-resume       t
        org-clock-out-remove-zero-time-clocks t
        org-duration-format       '((special . h:mm)))
  (org-clock-persistence-insinuate)

  ;;  Babel languages 
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t)
     (java       . t)
     (C          . t)
     (sql        . t)))

  ;;  Inline images auto-display 
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

;;;  Typography: mixed-pitch + custom heading faces 
;;
;; In Org buffers we use a proportional face for prose and keep the
;; monospace face for code, tables, and verbatim text.

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  ;; Keep monospace for these faces even in mixed-pitch mode
  (dolist (f '(org-block org-code org-table org-verbatim
               org-meta-line org-tag org-property-value
               org-checkbox org-formula))
    (add-to-list 'mixed-pitch-fixed-pitch-faces f)))

;; Custom heading scale + colour using doom-oceanic-next palette
(with-eval-after-load 'org
  (let* ((variable-font (if (member "Iosevka Aile" (font-family-list))
                            "Iosevka Aile"
                          "Sans Serif"))
         (heading-attrs `(:inherit default :font ,variable-font
                          :weight bold)))
    (custom-set-faces
     `(org-level-1 ((t (,@heading-attrs :height 1.4
                        :foreground "#6699cc"))))   ; soft blue
     `(org-level-2 ((t (,@heading-attrs :height 1.25
                        :foreground "#5fb3b3"))))   ; teal
     `(org-level-3 ((t (,@heading-attrs :height 1.12
                        :foreground "#c594c5"))))   ; purple
     `(org-level-4 ((t (,@heading-attrs :height 1.0
                        :foreground "#fac863"))))   ; yellow
     `(org-level-5 ((t (,@heading-attrs :height 1.0
                        :foreground "#a7adba"))))   ; grey
     ;; Document title
     `(org-document-title ((t (:font ,variable-font :height 1.6
                               :weight bold :foreground "#cdd3de")))))))

;; org-modern: replaces bullets / priorities / dates with cleaner glyphs.
;; Package is on GNU ELPA (not MELPA) — pin it there explicitly so
;; package.el doesn't try a stale MELPA build URL.
(use-package org-modern
  :ensure t
  :pin gnu
  :after org
  ;; Only activate inside actual Org buffers to avoid the
  ;; "org-element-at-point cannot be used in non-Org buffer" warning
  ;; that fires when the hook runs in *scratch* (lisp-interaction-mode).
  :hook ((org-mode              . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :config
  (setq org-modern-star             ["◉" "○" "●" "○" "●"]
        org-modern-todo             t
        org-modern-priority         t
        org-modern-tag              t
        org-modern-table            t
        org-modern-block-name       t
        org-modern-keyword          t
        org-modern-timestamp        t
        org-modern-horizontal-rule  t
        org-modern-indent           nil)) ; keep nil — conflicts with org-indent-mode

;;; org-config.el ends here
