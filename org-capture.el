;;; org-capture.el --- Org capture templates & agenda -*- lexical-binding: t; -*-
;;
;; Loaded by init.el.  Covers:
;;   • Org directory / inbox file
;;   • Capture templates (tasks, notes, journal, code snippets)
;;   • Agenda views

;;;  Directories 

;; Centralise everything under ~/org/.  Change ONLY this variable.
(defvar my/org-dir (expand-file-name "~/org/"))

(defun my/org-file (name)
  "Return absolute path to NAME inside `my/org-dir'."
  (expand-file-name name my/org-dir))

;;;  Files 

(with-eval-after-load 'org
  (setq
   org-directory            my/org-dir
   org-default-notes-file   (my/org-file "inbox.org")

   ;; Agenda scans these files
   org-agenda-files
   (mapcar #'my/org-file
           '("inbox.org"
             "projects.org"
             "journal.org"
             "work.org"))))

;;;  Capture templates 

(with-eval-after-load 'org
  (setq org-capture-templates
        `(;;  Quick task 
          ("t" "Task" entry
           (file+headline ,(my/org-file "inbox.org") "Inbox")
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :prepend t :empty-lines 1)

          ;;  Task with link to current context 
          ("T" "Task + context" entry
           (file+headline ,(my/org-file "inbox.org") "Inbox")
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :SOURCE: %a\n  :END:\n"
           :prepend t :empty-lines 1)

          ;;  Fleeting note 
          ("n" "Note" entry
           (file+headline ,(my/org-file "inbox.org") "Notes")
           "* %? :note:\n  %U\n"
           :prepend t :empty-lines 1)

          ;;  Daily journal 
          ("j" "Journal" entry
           (file+datetree ,(my/org-file "journal.org"))
           "* %<%H:%M> %?\n"
           :tree-type week)

          ;;  Code snippet 
          ("s" "Code snippet" entry
           (file+headline ,(my/org-file "inbox.org") "Snippets")
           "* %?\n  :PROPERTIES:\n  :LANGUAGE: %(completing-read \"Language: \" '(\"java\" \"python\" \"bash\" \"elisp\"))\n  :END:\n  #+BEGIN_SRC %(org-entry-get nil \"LANGUAGE\")\n  %i\n  #+END_SRC\n"
           :empty-lines 1)

          ;;  Meeting / work note 
          ("m" "Meeting" entry
           (file+datetree ,(my/org-file "work.org"))
           "* %? :meeting:\n  %U\n  Attendees: \n  ** Notes\n  ** Actions\n"
           :clock-in t :clock-resume t))))

;;;  Agenda 

(with-eval-after-load 'org-agenda
  (setq
   org-agenda-window-setup          'current-window
   org-agenda-span                  'week
   org-agenda-start-with-log-mode   t
   org-agenda-log-mode-items        '(closed clock)
   org-agenda-block-separator       ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────"

   ;; Custom agenda views
   org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next actions")))
       (todo "PROG"
             ((org-agenda-overriding-header "In progress")))
       (todo "WAIT"
             ((org-agenda-overriding-header "Waiting / blocked")))))

     ("n" "Next actions" todo "NEXT")
     ("w" "Work items" tags-todo "+@work")
     ("W" "Weekly review"
      ((agenda "" ((org-agenda-span 'week)))
       (stuck "")
       (todo "WAIT" nil)
       (todo "HOLD" nil))))))

;; Quick agenda keybinding
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c C") #'org-capture)

;;; org-capture.el ends here
