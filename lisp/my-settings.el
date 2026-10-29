;;; my-settings.el --- Every tunable in one place -*- lexical-binding: t; -*-

(defvar my/theme 'hannover-night)
(defvar my/themes '(hannover-night hannover-day))
(defvar my/theme-dir (locate-user-emacs-file "hannover-theme/"))

(defvar my/font-code "Iosevka Nerd Font")
(defvar my/font-ui "Plus Jakarta Sans")
(defvar my/font-code-height 132)
(defvar my/font-ui-height 120)
(defvar my/font-ui-weight 'light)
(defvar my/line-spacing 0.2)

(defvar my/fill-column 80)
(defvar my/tab-width 4)
(defvar my/js-indent 2)
(defvar my/fringe-width 8)

(defvar my/modeline-icon-height 1.3)
(defvar my/modeline-padding 6)
(defvar my/modeline-right-slack 1)
(defvar my/modeline-branch-max 24)

(defvar my/sidebar-width 26)
(defvar my/sidebar-text-scale 0.92)
(defvar my/sidebar-line-spacing 0.12)
(defvar my/sidebar-icon-height 1.0)
(defvar my/sidebar-refresh-delay 0.12)
(defvar my/sidebar-folded-groups '(system))

(defvar my/file-manager-width 0.3)
(defvar my/terminal-width 0.34)
(defvar my/terminal-height 0.3)

(defvar my/eaf-dir (locate-user-emacs-file "site-lisp/emacs-application-framework/"))
(defvar my/eaf-apps
  '(eaf-browser eaf-pyqterminal eaf-file-manager eaf-image-viewer
    eaf-pdf-viewer eaf-video-player eaf-music-player))
(defvar my/eaf-terminal-app "pyqterminal")
(defvar my/eaf-preview-apps '("image-viewer" "pdf-viewer" "video-player"))
(defvar my/thumbnail-size 160)
(defvar my/eaf-search-engine "duckduckgo")

(defvar my/eaf-app-icons
  '(("browser"       nerd-icons-faicon   "nf-fa-globe")
    ("pyqterminal"   nerd-icons-codicon  "nf-cod-terminal")
    ("file-manager"  nerd-icons-mdicon   "nf-md-folder")
    ("image-viewer"  nerd-icons-mdicon   "nf-md-image")
    ("pdf-viewer"    nerd-icons-mdicon   "nf-md-file_pdf_box")
    ("video-player"  nerd-icons-mdicon   "nf-md-movie")
    ("music-player"  nerd-icons-mdicon   "nf-md-music")))

(defvar my/buffer-hidden-patterns
  '("\\*Messages\\*" "\\*Async-native-compile-log\\*" "\\*Native-compile-Log\\*"
    "\\*Warnings\\*" "\\*Compile-Log\\*" "\\*Flymake log\\*" "\\*EGLOT"
    "\\*eaf\\*" "\\*Completions\\*" "\\*tramp" "\\*Echo Area" "\\*scratch-backup"))

(defvar my/eglot-modes
  '(js-mode js-ts-mode typescript-ts-mode tsx-ts-mode
    c-mode c-ts-mode c++-mode c++-ts-mode))

(defvar my/js-modes '(js-mode js-ts-mode typescript-ts-mode tsx-ts-mode))

(defvar my/org-dir (expand-file-name "~/org/"))
(defvar my/org-agenda-file-names '("inbox.org" "projects.org" "journal.org" "work.org"))
(defvar my/org-heading-scales '(1.4 1.25 1.12 1.0 1.0))
(defvar my/org-title-scale 1.6)

(provide 'my-settings)
;;; my-settings.el ends here
