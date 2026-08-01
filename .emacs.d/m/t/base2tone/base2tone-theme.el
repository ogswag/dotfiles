;;; base2tone-theme.el --- 2tone-based emacs color themes -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Alexander Zakharov
;; URL: https://github.com/ogswag/base2tone.el
;; Keywords: faces, themes
;; License: GPL-3

;;; Commentary:

;; Base2Tone (by Bram de Haan, https://github.com/atelierbram/Base2Tone) is a
;; family of duotone color schemes: each scheme is built from two hues plus a
;; neutral ramp, expressed as 32 swatches.
;;
;; These implementation is modeled structurally on base16-theme.el
;; (central core + thin theme stubs).

;;; Code:

(defun base2tone-theme--color (key colors)
  "Resolve KEY to a hex string using the swatch plist COLORS.
A keyword swatch reference like `:a0' is replaced by its hex value; the
special key `:none' becomes `unspecified'; anything else is returned as-is
so face attribute values (numbers, booleans, symbols) pass through."
  (cond
   ((eq key :none) 'unspecified)
   ((and (keywordp key) (plist-member colors key)) (plist-get colors key))
   (t key)))

(defun base2tone-theme--spec (spec colors)
  "Transform a face attribute plist SPEC, resolving swatch keys via COLORS."
  (let (out)
    (while spec
      (let ((attr (car spec))
            (val  (cadr spec)))
        (setq out (append out (list attr
                                    (if (and (memq attr '(:box :underline))
                                             (consp val))
                                        (base2tone-theme--spec val colors)
                                      (base2tone-theme--color val colors)))))
        (setq spec (cddr spec))))
    out))

;; Role anchors are taken directly from the official Base2Tone VSCode themes
;; (https://github.com/atelierbram/Base2Tone-VSCode-Themes), so colors are
;; well-fit for code editing:
;;                                dark   light
;;   background                    a0     c7
;;   default foreground            a7     c0
;;   cursor                        b3     b5
;;   selection / line highlight    a1     c6
;;   line number / active          a2/a6  c5/c1
;;   comment (italic)              a2     c4
;;   punctuation                   a3     c4
;;   string                        d7     b0
;;   keyword / storage.type        d7     d0
;;   keyword.operator              d4     d2
;;   constant / number             d6     b0
;;   function name                 b4     c1
;;   builtin / support.function    b7     c1
;;   variable                      b0     b0
;;   type / support.type           c2     c2
;;   property name                 d4     d3
;;   attribute name (italic)       b3     b3
;;   tag                           b0     c1
;;   error / warning (editor)      b1/b2  b1/b5
;;   heading                       b2     b2

(defconst base2tone-theme--faces
  '(;; basic UI
    (default            (:background :a0 :foreground :a7)
                        (:background :c7 :foreground :c0))
    (cursor             (:background :b3) (:background :b5))
    (region             (:background :a2 :extend t) (:background :c5 :extend t))
    (highlight          (:background :a2) (:background :c5))
    (hl-line            (:background :a1 :extend t) (:background :c6 :extend t))
    (secondary-selection (:background :a2) (:background :c5))
    (fringe             (:background :a0 :foreground :a3)
                        (:background :c7 :foreground :c4))
    (vertical-border    (:foreground :a2) (:foreground :c5))
    (window-divider     (:foreground :a2) (:foreground :c5))
    (minibuffer-prompt  (:foreground :b3 :weight bold) (:foreground :b5 :weight bold))
    (shadow             (:foreground :a4) (:foreground :c3))
    (link               (:foreground :b3 :underline t) (:foreground :b5 :underline t))
    (link-visited       (:foreground :b5 :underline t) (:foreground :d2 :underline t))
    (escape-glyph       (:foreground :d4) (:foreground :d2))
    (trailing-whitespace (:background :b1) (:background :b1))
    (tooltip            (:background :a1 :foreground :a7) (:background :c6 :foreground :c0))

    ;; mode line / header
    (mode-line          (:background :a2 :foreground :a7 :box nil)
                        (:background :c5 :foreground :c0 :box nil))
    (mode-line-inactive (:background :a1 :foreground :a4 :box nil)
                        (:background :c6 :foreground :c2 :box nil))
    (mode-line-emphasis (:foreground :b4 :weight bold) (:foreground :d0 :weight bold))
    (mode-line-buffer-id (:foreground :a7 :weight bold) (:foreground :c0 :weight bold))
    (header-line        (:background :a1 :foreground :a6) (:background :c6 :foreground :c1))

    ;; search / matching
    (isearch            (:background :d6 :foreground :a0 :weight bold)
                        (:background :d2 :foreground :c7 :weight bold))
    (isearch-fail       (:background :a1 :foreground :b1) (:background :c6 :foreground :b1))
    (lazy-highlight     (:background :a3 :foreground :a7) (:background :c4 :foreground :c7))
    (match              (:background :a3 :foreground :a7) (:background :c4 :foreground :c7))
    (show-paren-match   (:background :b3 :foreground :a0 :weight bold)
                        (:background :b5 :foreground :c7 :weight bold))
    (show-paren-mismatch (:background :b1 :foreground :a0 :weight bold)
                         (:background :b1 :foreground :c7 :weight bold))

    ;; status faces
    (error              (:foreground :b1 :weight bold) (:foreground :b1 :weight bold))
    (warning            (:foreground :b2 :weight bold) (:foreground :b5 :weight bold))
    (success            (:foreground :d6 :weight bold) (:foreground :d2 :weight bold))

    ;; font-lock
    (font-lock-comment-face            (:foreground :a2 :slant italic)
                                       (:foreground :c4 :slant italic))
    (font-lock-comment-delimiter-face  (:foreground :a3) (:foreground :c3))
    (font-lock-doc-face                (:foreground :a4 :slant italic)
                                       (:foreground :c3 :slant italic))
    (font-lock-string-face             (:foreground :d7) (:foreground :b0))
    (font-lock-doc-markup-face         (:foreground :d5) (:foreground :d2))
    (font-lock-keyword-face            (:foreground :d7) (:foreground :d0))
    (font-lock-builtin-face            (:foreground :b7) (:foreground :c1))
    (font-lock-function-name-face      (:foreground :b4) (:foreground :c1))
    (font-lock-variable-name-face      (:foreground :b0) (:foreground :b0))
    (font-lock-type-face               (:foreground :c2) (:foreground :c2))
    (font-lock-constant-face           (:foreground :d6) (:foreground :d1))
    (font-lock-number-face             (:foreground :d6) (:foreground :b0))
    (font-lock-preprocessor-face       (:foreground :d4) (:foreground :d2))
    (font-lock-negation-char-face      (:foreground :d4) (:foreground :d2))
    (font-lock-warning-face            (:foreground :b2 :weight bold)
                                       (:foreground :b5 :weight bold))
    (font-lock-regexp-grouping-backslash (:foreground :d4) (:foreground :d3))
    (font-lock-regexp-grouping-construct (:foreground :d5) (:foreground :d2))
    (font-lock-property-name-face      (:foreground :d4) (:foreground :d3))
    (font-lock-property-use-face       (:foreground :d4) (:foreground :d3))
    (font-lock-punctuation-face        (:foreground :a3) (:foreground :c4))
    (font-lock-bracket-face            (:foreground :a4) (:foreground :c3))
    (font-lock-operator-face           (:foreground :d4) (:foreground :d2))

    ;; line numbers
    (line-number               (:background :a0 :foreground :a2)
                               (:background :c7 :foreground :c5))
    (line-number-current-line  (:background :a1 :foreground :a6 :weight bold)
                               (:background :c6 :foreground :c1 :weight bold))
    (fill-column-indicator     (:foreground :a2) (:foreground :c5))

    ;; completion / minibuffer
    (completions-common-part      (:foreground :b4) (:foreground :d0))
    (completions-first-difference (:foreground :d7 :weight bold)
                                  (:foreground :d0 :weight bold))
    (completions-annotations      (:foreground :a4 :slant italic)
                                  (:foreground :c3 :slant italic))

    ;; isearch/whitespace/parens
    (whitespace-space     (:foreground :a3) (:foreground :c4))
    (whitespace-tab       (:foreground :a3) (:foreground :c4))
    (whitespace-newline   (:foreground :a3) (:foreground :c4))
    (whitespace-trailing  (:background :b1) (:background :b1))

    ;; org-mode
    (org-level-1       (:foreground :b4 :weight bold) (:foreground :b0 :weight bold))
    (org-level-2       (:foreground :d6 :weight bold) (:foreground :d1 :weight bold))
    (org-level-3       (:foreground :b6 :weight bold) (:foreground :d0 :weight bold))
    (org-level-4       (:foreground :d4 :slant italic) (:foreground :d2 :slant italic))
    (org-level-5       (:foreground :b5 :slant italic) (:foreground :b2 :slant italic))
    (org-level-6       (:foreground :d5 :slant italic) (:foreground :d3 :slant italic))
    (org-level-7       (:foreground :b3 :slant italic) (:foreground :b3 :slant italic))
    (org-level-8       (:foreground :a5 :slant italic) (:foreground :c2 :slant italic))
    (org-document-title (:foreground :a7 :weight bold) (:foreground :c0 :weight bold))
    (org-document-info (:foreground :b4) (:foreground :d0))
    (org-link          (:foreground :b3 :underline t) (:foreground :b5 :underline t))
    (org-date          (:foreground :b5 :underline t) (:foreground :d2 :underline t))
    (org-todo          (:foreground :b1 :weight bold) (:foreground :b1 :weight bold))
    (org-done          (:foreground :d6 :weight bold) (:foreground :d2 :weight bold))
    (org-headline-done (:foreground :a4) (:foreground :c3))
    (org-tag           (:foreground :a4 :slant italic) (:foreground :c3 :slant italic))
    (org-code          (:foreground :d7) (:foreground :b0))
    (org-verbatim      (:foreground :d7) (:foreground :b0))
    (org-block         (:background :a1 :extend t) (:background :c6 :extend t))
    (org-block-begin-line (:background :a1 :foreground :a3 :extend t)
                          (:background :c6 :foreground :c4 :extend t))
    (org-block-end-line   (:background :a1 :foreground :a3 :extend t)
                          (:background :c6 :foreground :c4 :extend t))
    (org-meta-line     (:foreground :a4) (:foreground :c3))
    (org-table         (:foreground :a6) (:foreground :c1))
    (org-special-keyword (:foreground :a4) (:foreground :c3))

    ;; markdown
    (markdown-header-face-1   (:foreground :b4 :weight bold) (:foreground :b0 :weight bold))
    (markdown-header-face-2   (:foreground :d6 :weight bold) (:foreground :d1 :weight bold))
    (markdown-header-face-3   (:foreground :b6 :weight bold) (:foreground :d0 :weight bold))
    (markdown-header-face-4   (:foreground :d4 :slant italic) (:foreground :d2 :slant italic))
    (markdown-header-face-5   (:foreground :b5 :slant italic) (:foreground :b2 :slant italic))
    (markdown-header-face-6   (:foreground :d5 :slant italic) (:foreground :d3 :slant italic))
    (markdown-link-face       (:foreground :b3 :underline t) (:foreground :b5 :underline t))
    (markdown-url-face        (:foreground :b5) (:foreground :d2))
    (markdown-code-face       (:background :a1 :foreground :d7)
                              (:background :c6 :foreground :b0))
    (markdown-inline-code-face (:foreground :d7) (:foreground :b0))
    (markdown-pre-face        (:foreground :d7) (:foreground :b0))
    (markdown-blockquote-face (:foreground :a4 :slant italic) (:foreground :c3 :slant italic))
    (markdown-bold-face       (:foreground :c1 :weight bold) (:foreground :c1 :weight bold))
    (markdown-italic-face     (:slant italic) (:slant italic))

    ;; diff / diff-hl
    (diff-added        (:foreground :d6) (:foreground :d2))
    (diff-removed      (:foreground :b1) (:foreground :b1))
    (diff-changed      (:foreground :d4) (:foreground :d3))
    (diff-header       (:background :a1 :foreground :a6) (:background :c6 :foreground :c1))
    (diff-file-header  (:foreground :a7 :weight bold) (:foreground :c0 :weight bold))
    (diff-hunk-header  (:background :a1 :foreground :a4) (:background :c6 :foreground :c3))
    (diff-refine-added   (:background :a2 :foreground :d6) (:background :c5 :foreground :d2))
    (diff-refine-removed (:background :a2 :foreground :b1) (:background :c5 :foreground :b1))
    (diff-hl-insert    (:background :d6 :foreground :d6) (:background :d2 :foreground :d2))
    (diff-hl-delete    (:background :b1 :foreground :b1) (:background :b1 :foreground :b1))
    (diff-hl-change    (:background :d4 :foreground :d4) (:background :d3 :foreground :d3))

    ;; magit
    (magit-section-heading      (:foreground :b4 :weight bold) (:foreground :b0 :weight bold))
    (magit-section-highlight    (:background :a1 :extend t) (:background :c6 :extend t))
    (magit-branch-local         (:foreground :b4) (:foreground :b0))
    (magit-branch-remote        (:foreground :d6) (:foreground :d2))
    (magit-hash                 (:foreground :a4) (:foreground :c3))
    (magit-diff-added           (:background :a1 :foreground :d6)
                                (:background :c6 :foreground :d2))
    (magit-diff-added-highlight (:background :a2 :foreground :d7)
                                (:background :c5 :foreground :d2))
    (magit-diff-removed         (:background :a1 :foreground :b1)
                                (:background :c6 :foreground :b1))
    (magit-diff-removed-highlight (:background :a2 :foreground :b2)
                                  (:background :c5 :foreground :b1))
    (magit-diff-context-highlight (:background :a1 :foreground :a5)
                                  (:background :c6 :foreground :c3))
    (magit-diff-hunk-heading    (:background :a2 :foreground :a5)
                                (:background :c5 :foreground :c2))
    (magit-diff-hunk-heading-highlight (:background :a3 :foreground :a7)
                                       (:background :c4 :foreground :c0))

    ;; flycheck / flymake
    (flycheck-error    (:underline (:style wave :color :b1)) (:underline (:style wave :color :b1)))
    (flycheck-warning  (:underline (:style wave :color :b2)) (:underline (:style wave :color :b5)))
    (flycheck-info     (:underline (:style wave :color :d6)) (:underline (:style wave :color :d2)))
    (flymake-error     (:underline (:style wave :color :b1)) (:underline (:style wave :color :b1)))
    (flymake-warning   (:underline (:style wave :color :b2)) (:underline (:style wave :color :b5)))
    (flymake-note      (:underline (:style wave :color :d6)) (:underline (:style wave :color :d2)))

    ;; company
    (company-tooltip            (:background :a1 :foreground :a7)
                                (:background :c6 :foreground :c0))
    (company-tooltip-selection  (:background :a3 :foreground :a7)
                                (:background :c4 :foreground :c7))
    (company-tooltip-common     (:foreground :d7 :weight bold)
                                (:foreground :d0 :weight bold))
    (company-tooltip-annotation (:foreground :a4) (:foreground :c3))
    (company-scrollbar-bg       (:background :a2) (:background :c5))
    (company-scrollbar-fg       (:background :b3) (:background :b5))

    ;; which-key / misc -------------------------------------------------
    (which-func        (:foreground :b4) (:foreground :d0))
    (dired-directory   (:foreground :b4 :weight bold) (:foreground :b0 :weight bold))
    (compilation-mode-line-fail (:foreground :b1) (:foreground :b1))
    (compilation-mode-line-exit (:foreground :d6) (:foreground :d2)))
  "Base2Tone face table: (FACE DARK-SPEC LIGHT-SPEC), specs use swatch keywords.")

(defun base2tone-theme-define (name colors variant)
  "Apply the Base2Tone faces for theme NAME using swatch plist COLORS.
VARIANT is `dark' or `light' and selects which spec column to use."
  (let ((idx (if (eq variant 'light) 2 1))
        (frame-bg (if (eq variant 'light) 'light 'dark)))
    (apply #'custom-theme-set-faces name
           (mapcar
            (lambda (entry)
              (let ((face (nth 0 entry))
                    (spec (nth idx entry)))
                (list face `((t ,(base2tone-theme--spec spec colors))))))
            base2tone-theme--faces))
    (custom-theme-set-variables
     name
     `(frame-background-mode ',frame-bg))))

(provide 'base2tone-theme)
;;; base2tone-theme.el ends here
