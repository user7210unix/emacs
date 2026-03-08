;;; forest-night-theme.el --- Forest Night Ethereal Magic theme for Emacs

;; Author: Generated from Forest Night VSCode theme
;; Version: 1.0.0
;; Keywords: faces, theme, dark

;;; Commentary:
;; A dark, minimal theme inspired by the Forest Night Ethereal Magic VSCode theme.
;; Features a deep forest-toned background with teal, purple, and warm accent colors.

;;; Code:

(deftheme forest-night
  "Forest Night — Ethereal Magic. A minimal dark theme with forest tones.")

(let (;; Base palette
      (bg          "#1a2125")
      (bg-alt      "#222a30")
      (bg-popup    "#1a2125")
      (bg-sel      "#3a4a55")
      (bg-hover    "#2d3540")
      (bg-inactive "#252e38")
      (border      "#2a2e33")

      (fg          "#c9d1d9")
      (fg-dim      "#8fa1b3")
      (fg-muted    "#6b7280")
      (fg-faint    "#4a5568")

      ;; Accent colors
      (teal        "#4ECDC4")
      (blue        "#66D9EF")
      (purple      "#9B59B6")
      (green       "#8FBC8F")
      (orange      "#F39C12")
      (red         "#c78a7a")
      (pink        "#E91E63")
      (peach       "#FFB74D")

      ;; Semantic
      (cursor      "#6b8fa3")
      (line-num    "#4a5568")
      (line-num-a  "#6b8fa3"))

  (custom-theme-set-faces
   'forest-night

   ;; ─── Core ───────────────────────────────────────────────────────────────────
   `(default                        ((t (:background ,bg :foreground ,fg))))
   `(cursor                         ((t (:background ,cursor))))
   `(region                         ((t (:background ,bg-sel))))
   `(secondary-selection            ((t (:background ,bg-inactive))))
   `(highlight                      ((t (:background ,bg-hover))))
   `(hl-line                        ((t (:background ,bg-alt))))
   `(fringe                         ((t (:background ,bg :foreground ,fg-faint))))
   `(vertical-border                ((t (:foreground ,border))))
   `(window-divider                 ((t (:foreground ,border))))
   `(window-divider-first-pixel     ((t (:foreground ,border))))
   `(window-divider-last-pixel      ((t (:foreground ,border))))
   `(minibuffer-prompt              ((t (:foreground ,teal :weight bold))))
   `(trailing-whitespace            ((t (:background ,red))))
   `(whitespace-tab                 ((t (:foreground ,fg-faint))))
   `(whitespace-space               ((t (:foreground ,fg-faint))))
   `(whitespace-newline             ((t (:foreground ,fg-faint))))
   `(fill-column-indicator          ((t (:foreground ,border))))

   ;; ─── Line Numbers ────────────────────────────────────────────────────────────
   `(line-number                    ((t (:background ,bg :foreground ,line-num))))
   `(line-number-current-line       ((t (:background ,bg :foreground ,line-num-a :weight bold))))

   ;; ─── Font Lock ───────────────────────────────────────────────────────────────
   `(font-lock-comment-face         ((t (:foreground ,fg-faint :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-faint :slant italic))))
   `(font-lock-doc-face             ((t (:foreground ,fg-muted :slant italic))))
   `(font-lock-doc-markup-face      ((t (:foreground ,teal))))
   `(font-lock-string-face          ((t (:foreground ,teal))))
   `(font-lock-keyword-face         ((t (:foreground ,orange :weight bold))))
   `(font-lock-builtin-face         ((t (:foreground ,purple :weight bold))))
   `(font-lock-function-name-face   ((t (:foreground ,purple))))
   `(font-lock-variable-name-face   ((t (:foreground ,fg))))
   `(font-lock-type-face            ((t (:foreground ,red))))
   `(font-lock-constant-face        ((t (:foreground ,green))))
   `(font-lock-number-face          ((t (:foreground ,green))))
   `(font-lock-operator-face        ((t (:foreground ,fg-dim))))
   `(font-lock-punctuation-face     ((t (:foreground ,fg-dim))))
   `(font-lock-delimiter-face       ((t (:foreground ,fg-dim))))
   `(font-lock-bracket-face         ((t (:foreground ,fg))))
   `(font-lock-negation-char-face   ((t (:foreground ,red))))
   `(font-lock-preprocessor-face    ((t (:foreground ,orange :slant italic))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,red :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,red))))
   `(font-lock-warning-face         ((t (:foreground ,orange :weight bold))))
   `(font-lock-misc-punctuation-face ((t (:foreground ,fg-dim))))
   `(font-lock-property-name-face   ((t (:foreground ,blue))))
   `(font-lock-property-use-face    ((t (:foreground ,blue))))

   ;; ─── Search & Replace ────────────────────────────────────────────────────────
   `(isearch                        ((t (:background ,purple :foreground ,bg :weight bold))))
   `(isearch-fail                   ((t (:background ,red :foreground ,bg))))
   `(lazy-highlight                 ((t (:background "#9B59B620" :foreground ,fg))))
   `(match                          ((t (:background "#9B59B630" :foreground ,fg))))
   `(query-replace                  ((t (:inherit isearch))))

   ;; ─── Mode Line ───────────────────────────────────────────────────────────────
   `(mode-line                      ((t (:background ,bg-sel :foreground ,fg :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive             ((t (:background ,bg :foreground ,fg-muted :box (:line-width 1 :color ,border)))))
   `(mode-line-buffer-id            ((t (:foreground ,teal :weight bold))))
   `(mode-line-buffer-id-inactive   ((t (:foreground ,fg-muted))))
   `(mode-line-emphasis             ((t (:foreground ,orange :weight bold))))
   `(mode-line-highlight            ((t (:foreground ,green :weight bold))))

   ;; ─── Header Line ─────────────────────────────────────────────────────────────
   `(header-line                    ((t (:background ,bg-alt :foreground ,fg-dim))))

   ;; ─── Links ───────────────────────────────────────────────────────────────────
   `(link                           ((t (:foreground ,blue :underline t))))
   `(link-visited                   ((t (:foreground ,purple :underline t))))
   `(button                         ((t (:foreground ,teal :underline t))))

   ;; ─── Errors / Warnings ───────────────────────────────────────────────────────
   `(error                          ((t (:foreground ,red :weight bold))))
   `(warning                        ((t (:foreground ,orange :weight bold))))
   `(success                        ((t (:foreground ,green :weight bold))))

   ;; ─── Flycheck / Flymake ──────────────────────────────────────────────────────
   `(flycheck-error                 ((t (:underline (:style wave :color ,red)))))
   `(flycheck-warning               ((t (:underline (:style wave :color ,orange)))))
   `(flycheck-info                  ((t (:underline (:style wave :color ,teal)))))
   `(flymake-error                  ((t (:underline (:style wave :color ,red)))))
   `(flymake-warning                ((t (:underline (:style wave :color ,orange)))))
   `(flymake-note                   ((t (:underline (:style wave :color ,teal)))))

   ;; ─── Compilation ─────────────────────────────────────────────────────────────
   `(compilation-error              ((t (:foreground ,red :weight bold))))
   `(compilation-warning            ((t (:foreground ,orange :weight bold))))
   `(compilation-info               ((t (:foreground ,teal))))
   `(compilation-mode-line-run      ((t (:foreground ,orange))))
   `(compilation-mode-line-exit     ((t (:foreground ,green))))
   `(compilation-mode-line-fail     ((t (:foreground ,red))))
   `(compilation-line-number        ((t (:foreground ,green))))
   `(compilation-column-number      ((t (:foreground ,fg-muted))))

   ;; ─── Diffs ───────────────────────────────────────────────────────────────────
   `(diff-header                    ((t (:foreground ,fg-dim :weight bold))))
   `(diff-file-header               ((t (:foreground ,blue :weight bold))))
   `(diff-hunk-header               ((t (:foreground ,purple))))
   `(diff-added                     ((t (:background "#8FBC8F10" :foreground ,green))))
   `(diff-removed                   ((t (:background "#c78a7a10" :foreground ,red))))
   `(diff-changed                   ((t (:background "#F39C1210" :foreground ,orange))))
   `(diff-refine-added              ((t (:background "#8FBC8F30"))))
   `(diff-refine-removed            ((t (:background "#c78a7a30"))))
   `(diff-refine-changed            ((t (:background "#F39C1230"))))

   ;; ─── Ediff ───────────────────────────────────────────────────────────────────
   `(ediff-current-diff-A           ((t (:background "#c78a7a20"))))
   `(ediff-current-diff-B           ((t (:background "#8FBC8F20"))))
   `(ediff-current-diff-C           ((t (:background "#F39C1220"))))
   `(ediff-fine-diff-A              ((t (:background "#c78a7a40"))))
   `(ediff-fine-diff-B              ((t (:background "#8FBC8F40"))))
   `(ediff-fine-diff-C              ((t (:background "#F39C1240"))))
   `(ediff-even-diff-A              ((t (:background ,bg-alt))))
   `(ediff-even-diff-B              ((t (:background ,bg-alt))))
   `(ediff-odd-diff-A               ((t (:background ,bg-hover))))
   `(ediff-odd-diff-B               ((t (:background ,bg-hover))))

   ;; ─── Org Mode ────────────────────────────────────────────────────────────────
   `(org-level-1                    ((t (:foreground ,orange :weight bold :height 1.3))))
   `(org-level-2                    ((t (:foreground ,teal :weight bold :height 1.2))))
   `(org-level-3                    ((t (:foreground ,blue :weight bold :height 1.1))))
   `(org-level-4                    ((t (:foreground ,green :weight semi-bold))))
   `(org-level-5                    ((t (:foreground ,purple))))
   `(org-level-6                    ((t (:foreground ,red))))
   `(org-level-7                    ((t (:foreground ,peach))))
   `(org-level-8                    ((t (:foreground ,fg-dim))))
   `(org-document-title             ((t (:foreground ,teal :weight bold :height 1.4))))
   `(org-document-info              ((t (:foreground ,fg-muted))))
   `(org-document-info-keyword      ((t (:foreground ,fg-faint))))
   `(org-done                       ((t (:foreground ,green :weight bold))))
   `(org-todo                       ((t (:foreground ,orange :weight bold))))
   `(org-headline-done              ((t (:foreground ,fg-muted :strike-through t))))
   `(org-link                       ((t (:foreground ,blue :underline t))))
   `(org-code                       ((t (:foreground ,teal :background ,bg-alt))))
   `(org-verbatim                   ((t (:foreground ,teal :background ,bg-alt))))
   `(org-block                      ((t (:background ,bg-alt :extend t))))
   `(org-block-begin-line           ((t (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-block-end-line             ((t (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-date                       ((t (:foreground ,purple))))
   `(org-tag                        ((t (:foreground ,fg-muted :weight normal))))
   `(org-table                      ((t (:foreground ,blue))))
   `(org-formula                    ((t (:foreground ,orange))))
   `(org-special-keyword            ((t (:foreground ,fg-faint :slant italic))))
   `(org-priority                   ((t (:foreground ,red))))
   `(org-checkbox                   ((t (:foreground ,teal))))
   `(org-footnote                   ((t (:foreground ,purple))))
   `(org-ellipsis                   ((t (:foreground ,fg-faint))))

   ;; ─── Markdown ────────────────────────────────────────────────────────────────
   `(markdown-header-face-1         ((t (:foreground ,orange :weight bold :height 1.3))))
   `(markdown-header-face-2         ((t (:foreground ,teal :weight bold :height 1.2))))
   `(markdown-header-face-3         ((t (:foreground ,blue :weight bold :height 1.1))))
   `(markdown-header-face-4         ((t (:foreground ,green :weight semi-bold))))
   `(markdown-header-face-5         ((t (:foreground ,purple))))
   `(markdown-header-face-6         ((t (:foreground ,red))))
   `(markdown-bold-face             ((t (:foreground ,fg :weight bold))))
   `(markdown-italic-face           ((t (:foreground ,fg :slant italic))))
   `(markdown-code-face             ((t (:foreground ,teal :background ,bg-alt))))
   `(markdown-pre-face              ((t (:foreground ,teal :background ,bg-alt))))
   `(markdown-link-face             ((t (:foreground ,blue :underline t))))
   `(markdown-url-face              ((t (:foreground ,fg-muted :underline t))))
   `(markdown-list-face             ((t (:foreground ,orange))))
   `(markdown-blockquote-face       ((t (:foreground ,fg-muted :slant italic))))
   `(markdown-hr-face               ((t (:foreground ,border))))
   `(markdown-inline-code-face      ((t (:foreground ,teal))))

   ;; ─── Completion (Corfu / Company / Vertico) ──────────────────────────────────
   `(completions-common-part        ((t (:foreground ,teal :weight bold))))
   `(completions-first-difference   ((t (:foreground ,blue :weight bold))))

   `(company-tooltip                ((t (:background ,bg-popup :foreground ,fg))))
   `(company-tooltip-common         ((t (:foreground ,teal :weight bold))))
   `(company-tooltip-selection      ((t (:background ,bg-hover :foreground ,fg))))
   `(company-tooltip-annotation     ((t (:foreground ,fg-muted :slant italic))))
   `(company-scrollbar-bg           ((t (:background ,border))))
   `(company-scrollbar-fg           ((t (:background ,fg-muted))))
   `(company-preview-common         ((t (:foreground ,teal :slant italic))))

   `(corfu-default                  ((t (:background ,bg-popup :foreground ,fg))))
   `(corfu-current                  ((t (:background ,bg-hover :foreground ,fg))))
   `(corfu-bar                      ((t (:background ,fg-muted))))
   `(corfu-border                   ((t (:background ,border))))
   `(corfu-annotations              ((t (:foreground ,fg-muted))))

   ;; ─── Vertico / Selectrum / Ivy ───────────────────────────────────────────────
   `(vertico-current                ((t (:background ,bg-sel :foreground ,fg :weight bold))))

   `(ivy-current-match              ((t (:background ,bg-sel :foreground ,fg :weight bold))))
   `(ivy-minibuffer-match-face-1    ((t (:foreground ,teal :weight bold))))
   `(ivy-minibuffer-match-face-2    ((t (:foreground ,blue :weight bold))))
   `(ivy-minibuffer-match-face-3    ((t (:foreground ,orange :weight bold))))
   `(ivy-minibuffer-match-face-4    ((t (:foreground ,purple :weight bold))))
   `(ivy-remote                     ((t (:foreground ,teal))))

   `(consult-preview-line           ((t (:background ,bg-alt))))
   `(consult-highlight-match        ((t (:background "#9B59B630"))))

   ;; ─── Orderless ───────────────────────────────────────────────────────────────
   `(orderless-match-face-0         ((t (:foreground ,teal :weight bold))))
   `(orderless-match-face-1         ((t (:foreground ,blue :weight bold))))
   `(orderless-match-face-2         ((t (:foreground ,orange :weight bold))))
   `(orderless-match-face-3         ((t (:foreground ,purple :weight bold))))

   ;; ─── Treemacs / Dired ────────────────────────────────────────────────────────
   `(dired-directory                ((t (:foreground ,blue :weight bold))))
   `(dired-symlink                  ((t (:foreground ,teal))))
   `(dired-ignored                  ((t (:foreground ,fg-faint))))
   `(dired-flagged                  ((t (:foreground ,red :weight bold))))
   `(dired-marked                   ((t (:foreground ,orange :weight bold))))
   `(dired-header                   ((t (:foreground ,teal :weight bold))))

   ;; ─── Magit ───────────────────────────────────────────────────────────────────
   `(magit-section-heading          ((t (:foreground ,teal :weight bold))))
   `(magit-section-highlight        ((t (:background ,bg-alt))))
   `(magit-section-heading-selection ((t (:foreground ,orange :weight bold))))
   `(magit-diff-file-heading        ((t (:foreground ,blue :weight bold))))
   `(magit-diff-file-heading-highlight ((t (:foreground ,blue :background ,bg-alt :weight bold))))
   `(magit-diff-hunk-heading        ((t (:foreground ,purple :background ,bg-alt))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,purple :background ,bg-hover))))
   `(magit-diff-added               ((t (:foreground ,green :background "#8FBC8F10"))))
   `(magit-diff-removed             ((t (:foreground ,red :background "#c78a7a10"))))
   `(magit-diff-added-highlight     ((t (:foreground ,green :background "#8FBC8F20"))))
   `(magit-diff-removed-highlight   ((t (:foreground ,red :background "#c78a7a20"))))
   `(magit-diff-context             ((t (:foreground ,fg-muted))))
   `(magit-diff-context-highlight   ((t (:foreground ,fg :background ,bg-alt))))
   `(magit-hash                     ((t (:foreground ,teal))))
   `(magit-tag                      ((t (:foreground ,orange))))
   `(magit-branch-local             ((t (:foreground ,green))))
   `(magit-branch-remote            ((t (:foreground ,blue))))
   `(magit-branch-current           ((t (:foreground ,teal :weight bold))))
   `(magit-log-author               ((t (:foreground ,purple))))
   `(magit-log-date                 ((t (:foreground ,fg-muted))))
   `(magit-process-ok               ((t (:foreground ,green))))
   `(magit-process-ng               ((t (:foreground ,red))))
   `(magit-blame-heading            ((t (:background ,bg-alt :foreground ,fg-muted))))
   `(magit-blame-summary            ((t (:foreground ,blue))))
   `(magit-blame-hash               ((t (:foreground ,teal))))
   `(magit-blame-date               ((t (:foreground ,purple))))

   ;; ─── Git Gutter / Diff-HL ────────────────────────────────────────────────────
   `(diff-hl-insert                 ((t (:foreground ,green :background ,green))))
   `(diff-hl-delete                 ((t (:foreground ,red :background ,red))))
   `(diff-hl-change                 ((t (:foreground ,orange :background ,orange))))
   `(git-gutter:added               ((t (:foreground ,green :weight bold))))
   `(git-gutter:deleted             ((t (:foreground ,red :weight bold))))
   `(git-gutter:modified            ((t (:foreground ,orange :weight bold))))

   ;; ─── LSP / Eglot ─────────────────────────────────────────────────────────────
   `(eglot-highlight-symbol-face    ((t (:background "#4ECDC420"))))
   `(eglot-diagnostic-tag-deprecated-face ((t (:strike-through t :foreground ,fg-muted))))
   `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,fg-faint))))

   `(lsp-face-highlight-textual     ((t (:background "#4ECDC420"))))
   `(lsp-face-highlight-read        ((t (:background "#4ECDC420"))))
   `(lsp-face-highlight-write       ((t (:background "#9B59B620"))))

   ;; ─── Treesitter ──────────────────────────────────────────────────────────────
   `(treesit-font-lock-face         ((t nil)))

   ;; ─── Eldoc ───────────────────────────────────────────────────────────────────
   `(eldoc-highlight-function-argument ((t (:foreground ,orange :weight bold))))

   ;; ─── Rainbow Delimiters ──────────────────────────────────────────────────────
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,teal))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,peach))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,fg-dim))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,red :weight bold :underline t))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,red :weight bold :underline t))))

   ;; ─── Highlight Indent Guides ─────────────────────────────────────────────────
   `(highlight-indent-guides-character-face ((t (:foreground ,border))))
   `(highlight-indent-guides-top-character-face ((t (:foreground ,teal))))
   `(highlight-indent-guides-stack-character-face ((t (:foreground ,border))))

   ;; ─── Which-Key ───────────────────────────────────────────────────────────────
   `(which-key-key-face             ((t (:foreground ,teal :weight bold))))
   `(which-key-separator-face       ((t (:foreground ,fg-faint))))
   `(which-key-command-description-face ((t (:foreground ,fg))))
   `(which-key-group-description-face ((t (:foreground ,orange))))
   `(which-key-local-map-description-face ((t (:foreground ,blue))))

   ;; ─── Helpful / Help ──────────────────────────────────────────────────────────
   `(helpful-heading                ((t (:foreground ,orange :weight bold :height 1.2))))

   ;; ─── Pulse ───────────────────────────────────────────────────────────────────
   `(pulse-highlight-start-face     ((t (:background ,bg-sel))))

   ;; ─── Term / VTerm ────────────────────────────────────────────────────────────
   `(term-color-black               ((t (:foreground ,bg :background ,bg))))
   `(term-color-blue                ((t (:foreground ,teal :background ,teal))))
   `(term-color-cyan                ((t (:foreground ,teal :background ,teal))))
   `(term-color-green               ((t (:foreground ,green :background ,green))))
   `(term-color-magenta             ((t (:foreground ,purple :background ,purple))))
   `(term-color-red                 ((t (:foreground ,pink :background ,pink))))
   `(term-color-white               ((t (:foreground ,fg :background ,fg))))
   `(term-color-yellow              ((t (:foreground ,orange :background ,orange))))
   `(term-default-fg-color          ((t (:inherit term-color-white))))
   `(term-default-bg-color          ((t (:inherit term-color-black))))

   ;; ─── Popup / Tooltip ─────────────────────────────────────────────────────────
   `(popup-face                     ((t (:background ,bg-popup :foreground ,fg))))
   `(popup-menu-selection-face      ((t (:background ,bg-sel :foreground ,fg))))
   `(tooltip                        ((t (:background ,bg-popup :foreground ,fg))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'forest-night)

;;; forest-night-theme.el ends here
