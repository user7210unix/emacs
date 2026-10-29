;;; my-org-capture.el --- Org capture templates and agenda -*- lexical-binding: t; -*-

(require 'my-org)

(defconst my/org-code-languages "javascript|typescript|java|python|bash|elisp")

(defun my/org-capture-template (key label target body &rest options)
  `(,key ,label entry ,target ,body ,@options))

(defun my/org-inbox (heading)
  `(file+headline ,(my/org-file "inbox.org") ,heading))

(defun my/org-capture-templates ()
  (list
   (my/org-capture-template
    "t" "Task" (my/org-inbox "Inbox")
    "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
    :prepend t :empty-lines 1)
   (my/org-capture-template
    "T" "Task + context" (my/org-inbox "Inbox")
    "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %a\n:END:\n"
    :prepend t :empty-lines 1)
   (my/org-capture-template
    "n" "Note" (my/org-inbox "Notes")
    "* %? :note:\n%U\n"
    :prepend t :empty-lines 1)
   (my/org-capture-template
    "j" "Journal" `(file+datetree ,(my/org-file "journal.org"))
    "* %<%H:%M> %?\n"
    :tree-type 'week)
   (my/org-capture-template
    "s" "Code snippet" (my/org-inbox "Snippets")
    (format "* %%?\n#+BEGIN_SRC %%^{Language|%s}\n%%i\n#+END_SRC\n" my/org-code-languages)
    :empty-lines 1)
   (my/org-capture-template
    "m" "Meeting" `(file+datetree ,(my/org-file "work.org"))
    "* %? :meeting:\n%U\nAttendees: \n** Notes\n** Actions\n"
    :clock-in t :clock-resume t)))

(defconst my/org-agenda-commands
  '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
      (todo "PROG" ((org-agenda-overriding-header "In progress")))
      (todo "WAIT" ((org-agenda-overriding-header "Waiting / blocked")))))
    ("n" "Next actions" todo "NEXT")
    ("w" "Work items" tags-todo "+@work")
    ("W" "Weekly review"
     ((agenda "" ((org-agenda-span 'week)))
      (stuck "")
      (todo "WAIT" nil)
      (todo "HOLD" nil)))))

(with-eval-after-load 'org
  (setq org-default-notes-file (my/org-file "inbox.org")
        org-agenda-files (mapcar #'my/org-file my/org-agenda-file-names)
        org-capture-templates (my/org-capture-templates)))

(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'current-window
        org-agenda-span 'week
        org-agenda-start-with-log-mode t
        org-agenda-log-mode-items '(closed clock)
        org-agenda-block-separator ?─
        org-agenda-time-grid '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "◀── now ──────────────────────"
        org-agenda-custom-commands my/org-agenda-commands))

(bind-keys ("C-c a" . org-agenda)
           ("C-c C" . org-capture))

(provide 'my-org-capture)
;;; my-org-capture.el ends here
