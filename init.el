;;; init.el --- Entry point -*- lexical-binding: t; -*-
;;
;;  early-init.el   frame + startup tuning
;;  lisp/my-*.el    one concern per module, all tunables in my-settings.el

(require 'package)

(setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 3) ("nongnu" . 2) ("melpa" . 1)))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(eval-when-compile (require 'use-package))
(require 'bind-key)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(defvar my/modules
  '(my-settings my-core my-ui my-icons my-modeline my-completion my-sidebar
    my-prog my-eaf my-org my-org-capture my-org-export my-scratch
    ;; my-java
    ))

(dolist (module my/modules)
  (with-demoted-errors "Module failed: %S"
    (require module)))

;;; init.el ends here
