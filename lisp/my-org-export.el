;;; my-org-export.el --- Org export: HTML, PDF, Pandoc, reveal.js -*- lexical-binding: t; -*-

(require 'my-org)

(defconst my/org-html-css
  "<style>
body{max-width:780px;margin:2em auto;font-family:sans-serif;line-height:1.7;color:#cdd3de;background:#1b2b34}
pre,code{font-family:monospace;background:#1f313d;padding:.2em .4em;border-radius:3px}
pre{padding:1em;overflow-x:auto}
h1,h2,h3{color:#6699cc}
a{color:#5fb3b3}
</style>")

(defconst my/xelatex-command
  "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")

(with-eval-after-load 'ox-html
  (setq org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style t
        org-html-head-include-scripts nil
        org-html-validation-link nil
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-head my/org-html-css))

(with-eval-after-load 'ox-latex
  (setq org-latex-compiler "xelatex"
        org-latex-pdf-process (list my/xelatex-command my/xelatex-command)
        org-latex-src-block-backend 'minted
        org-latex-packages-alist '(("" "minted" t) ("" "xcolor" t) ("margin=2cm" "geometry" t)))
  (add-to-list 'org-latex-classes
               '("elegant-article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{fontspec}
\\setmainfont{Linux Libertine O}
\\setsansfont{Linux Biolinum O}
\\setmonofont{JetBrains Mono}[Scale=0.9]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package ox-pandoc
  :after ox)

(use-package org-re-reveal
  :after ox
  :custom
  (org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-re-reveal-theme "moon")
  (org-re-reveal-transition "slide")
  (org-re-reveal-plugins '(markdown highlight zoom notes)))

(provide 'my-org-export)
;;; my-org-export.el ends here
