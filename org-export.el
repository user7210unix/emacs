;;; org-export.el --- Org export configuration -*- lexical-binding: t; -*-
;;
;; Loaded by init.el.  Covers:
;;   - HTML export (clean, self-contained, readable)
;;   - LaTeX / PDF export (via pdflatex or xelatex)
;;   - ox-pandoc (optional, for .docx / .epub)
;;   - Reveal.js presentations via org-reveal

;;; HTML export
(with-eval-after-load 'ox-html
  (setq
   ;; Self-contained HTML with inline CSS (no external deps)
   org-html-htmlize-output-type    'inline-css
   org-html-self-contained-links   t
   org-html-head-include-default-style t
   org-html-head-include-scripts   nil   ; no jQuery etc.
   org-html-validation-link        nil
   org-html-doctype                "html5"
   org-html-html5-fancy            t

   ;; Optional: override with a clean minimal CSS
   org-html-head
   "<style>
     body{max-width:780px;margin:2em auto;font-family:sans-serif;
          line-height:1.7;color:#cdd3de;background:#1b2b34}
     pre,code{font-family:monospace;background:#1f313d;padding:.2em .4em;
              border-radius:3px}
     pre{padding:1em;overflow-x:auto}
     h1,h2,h3{color:#6699cc}
     a{color:#5fb3b3}
   </style>"))

;;; ── LaTeX / PDF export ───────────────────────────────────────────────────

(with-eval-after-load 'ox-latex
  (setq
   ;; Use xelatex for better Unicode/font support
   org-latex-compiler              "xelatex"
   org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f")
   org-latex-listings              'minted   ; syntax highlighting in PDF
   org-latex-packages-alist
   '(("" "minted" t)
     ("" "xcolor" t)
     ("margin=2cm" "geometry" t))
   ;; Clean up intermediate LaTeX files after export
   org-latex-logfiles-extensions
   '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist"
     "idx" "log" "nav" "out" "ptc" "run.xml" "snm"
     "toc" "vrb" "xdv")))

;; A nice default article class
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("elegant-article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{fontspec}
\\setmainfont{Linux Libertine O}
\\setsansfont{Linux Biolinum O}
\\setmonofont{JetBrains Mono}[Scale=0.9]"
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

;;; ox-pandoc (optional: DOCX, EPUB)
;;
;; Requires `pandoc' to be on PATH.  Install separately.
;; Comment the whole block out if you don't need Word/EPUB export.

(use-package ox-pandoc
  :after ox
  :config
  (setq org-pandoc-options              nil
        org-pandoc-options-for-docx     '((reference-doc . nil))
        org-pandoc-options-for-epub     '((epub-stylesheet . nil))))

;;; Reveal.js presentations
;;
;; org-reveal (original) has been unmaintained since 2019 and is NOT on
;; MELPA. The live, maintained replacement is org-re-reveal (MELPA).
;; Usage: C-c C-e v v  (write HTML)   C-c C-e v b  (write + open browser)
;; reveal.js loaded from CDN — no local copy required.

(use-package org-re-reveal
  :after ox
  :config
  (setq org-re-reveal-root        "https://cdn.jsdelivr.net/npm/reveal.js"
        org-re-reveal-theme       "moon"
        org-re-reveal-transition  "slide"
        org-re-reveal-hlevel      1
        org-re-reveal-plugins     '(markdown highlight zoom notes)))

;;; org-export.el ends here
