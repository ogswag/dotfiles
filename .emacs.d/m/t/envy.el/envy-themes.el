;;; envy-themes.el --- Shared engine for the Envy light themes -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: ported from envy.nvim (Alexander Zakharov), originally vim-envy by
;;         Gadzhi Kharkharov.
;; URL: https://github.com/ogswag/envy.el
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0.0
;; Keywords: faces, themes
;; SPDX-License-Identifier: MPL-2.0

;;; Commentary:
;;
;; Envy is a calm, light theme.  This is an Emacs port of the Neovim
;; colorscheme `envy.nvim'.  It ships two variants that share one palette and
;; an identical UI, differing only in how syntax tokens are coloured:
;;
;;   envy           minimal / near-monochrome (black fg, bold keywords, grey
;;                  italic comments, green strings, blue numbers).  The
;;                  original look.
;;   envy-colorful  moderate extra syntax colour (purple keywords, blue
;;                  functions, cyan types, red constants, muted properties).
;;
;; This file is the shared engine.  The user-facing themes live in
;; `envy-theme.el' and `envy-colorful-theme.el', which simply require this and
;; expand the `envy-themes-deftheme' macro.  Load either with, e.g.:
;;
;;   (add-to-list 'custom-theme-load-path "/path/to/envy.el/")
;;   (load-theme 'envy t)            ; or 'envy-colorful
;;
;; The palette is ported verbatim from envy.nvim/lua/envy/palette.lua and the
;; per-token specs from groups/syntax.lua `M.tokens'.

;;; Code:

(defconst envy-themes-palette
  '((bg      . "#eeeeee")   ; background
    (fg      . "#000000")   ; foreground / black
    (black   . "#000000")
    (brblack . "#144E40")   ; muted fg (properties / params in colorful)
    (blue    . "#005fd7")
    (lblue   . "#afd7ff")   ; visual / search / matchparen bg
    (green   . "#005f00")
    (red     . "#d70000")
    (lred    . "#ffafaf")   ; error bg
    (orange  . "#d75f00")   ; "yellow" in the original (regexp / warnings)
    (lyellow . "#ffd787")   ; warning bg
    (purple  . "#350075")
    (pink    . "#ffafff")
    (cyan    . "#005670")
    (lcyan   . "#afd7af")   ; hint bg
    (white   . "#ffffff")
    (grey    . "#6c6c6c")   ; comments
    (lgrey1  . "#e4e4e4")   ; cursorline / colorcolumn / folds
    (lgrey2  . "#c6c6c6")   ; nontext / tabline / statuslinenc
    (lgrey3  . "#b2b2b2"))  ; statusline bg
  "The Envy colour palette, ported verbatim from envy.nvim.")

(defun envy-themes--faces (variant)
  "Return the face spec list for VARIANT (`envy' or `colorful').
Each element is suitable as an argument to `custom-theme-set-faces'."
  (let* ((p envy-themes-palette)
         (bg		(cdr (assq 'bg p)))
         (fg		(cdr (assq 'fg p)))
         (black	(cdr (assq 'black p)))
         (brblack	(cdr (assq 'brblack p)))
         (blue		(cdr (assq 'blue p)))
         (lblue		(cdr (assq 'lblue p)))
         (green	(cdr (assq 'green p)))
         (red		(cdr (assq 'red p)))
         (lred		(cdr (assq 'lred p)))
         (orange	(cdr (assq 'orange p)))
         (lyellow	(cdr (assq 'lyellow p)))
         (purple	(cdr (assq 'purple p)))
         (cyan		(cdr (assq 'cyan p)))
         (lcyan		(cdr (assq 'lcyan p)))
         (white	(cdr (assq 'white p)))
         (grey		(cdr (assq 'grey p)))
         (lgrey1	(cdr (assq 'lgrey1 p)))
         (lgrey2	(cdr (assq 'lgrey2 p)))
         (lgrey3	(cdr (assq 'lgrey3 p)))
         (colorful (eq variant 'colorful))
         ;; Per-variant token specs
         (tk-keyword (if colorful `(:foreground ,fg :weight bold)
                       `(:foreground ,fg :weight bold)))
         (tk-func    (if colorful `(:foreground ,blue) `(:foreground ,fg)))
         (tk-type    `(:foreground ,cyan))
         (tk-const   `(:foreground ,cyan))
         (tk-string  `(:foreground ,green))
         (tk-number  `(:foreground ,blue))
         ;; `property' and `parameter' share a spec (brblack in colorful);
	 ;; Emacs has no distinct parameter face, so both use `tk-prop'.
         (tk-prop    (if colorful `(:foreground ,brblack) `(:foreground ,fg)))
         (tk-op      `(:foreground ,fg))
         (tk-ident   `(:foreground ,fg))
         (tk-special `(:foreground ,fg))
         (tk-comment `(:foreground "#42668F" :background "#DDE3E9" :slant italic))
         (tk-macro   `(:foreground ,purple))
         (tk-regexp  `(:foreground ,orange)))
    (list
     ;;; -------------------------------------------------------------------
     ;;; Core editor / UI (variant-independent; from groups/editor.lua)
     ;;; -------------------------------------------------------------------
     `(default              ((t (:foreground ,fg :background ,bg))))
     `(cursor               ((t (:background ,fg))))
     `(fringe               ((t (:foreground ,grey :background ,lgrey1))))
     `(region               ((t (:background ,lblue :extend t))))
     `(highlight            ((t (:background ,lgrey1))))
     `(hl-line              ((t (:background ,lgrey1 :extend t))))
     `(cursor-line          ((t (:background ,lgrey1 :extend t))))
     `(secondary-selection  ((t (:background ,lgrey2 :extend t))))
     `(lazy-highlight       ((t (:background ,lblue))))
     `(isearch              ((t (:background ,lblue :weight bold))))
     `(isearch-fail         ((t (:foreground ,white :background ,red))))
     `(match                ((t (:background ,lblue :weight bold))))
     `(show-paren-match     ((t (:background ,lblue :weight bold))))
     `(show-paren-match-expression ((t (:background ,lgrey1))))
     `(show-paren-mismatch  ((t (:foreground ,white :background ,red :weight bold))))
     `(trailing-whitespace  ((t (:background ,lred))))
     `(escape-glyph         ((t (:foreground ,blue))))
     `(homoglyph            ((t (:foreground ,blue))))
     `(nobreak-space        ((t (:foreground ,lgrey2 :underline t))))
     `(nobreak-hyphen       ((t (:foreground ,lgrey2))))
     `(minibuffer-prompt    ((t (:foreground ,blue :weight bold))))
     `(shadow               ((t (:foreground ,grey))))
     `(error                ((t (:foreground ,red :weight bold))))
     `(warning              ((t (:foreground ,orange :weight bold))))
     `(success              ((t (:foreground ,green :weight bold))))
     `(tooltip              ((t (:foreground ,fg :background ,white))))
     `(link                 ((t (:foreground ,blue :underline t))))
     `(link-visited         ((t (:foreground ,purple :underline t))))
     `(button               ((t (:inherit link))))
     `(help-key-binding     ((t (:foreground ,blue :background ,lgrey1))))
     `(highlight-quoted-symbol ((t (:foreground ,green))))

     ;; Line numbers / fill column / dividers
     `(line-number              ((t (:inherit default :foreground ,grey :background ,lgrey1))))
     `(line-number-current-line ((t (:inherit default :foreground ,grey :background ,lgrey1 :weight bold))))
     `(line-number-major-tick   ((t (:inherit default :foreground ,fg :background ,lgrey1))))
     `(line-number-minor-tick   ((t (:inherit default :foreground ,grey :background ,lgrey1))))
     `(fill-column-indicator    ((t (:inherit default :foreground ,lgrey2))))
     `(vertical-border          ((t (:foreground ,lgrey2))))
     `(window-divider           ((t (:foreground ,lgrey2))))
     `(window-divider-first-pixel ((t (:foreground ,lgrey2))))
     `(window-divider-last-pixel  ((t (:foreground ,lgrey2))))
     `(separator-line           ((t (:foreground ,lgrey2))))

     ;; whitespace-mode
     `(whitespace-space       ((t (:foreground ,lgrey2))))
     `(whitespace-tab         ((t (:foreground ,lgrey2))))
     `(whitespace-newline     ((t (:foreground ,lgrey2))))
     `(whitespace-hspace      ((t (:foreground ,lgrey2))))
     `(whitespace-indentation ((t (:foreground ,lgrey2))))
     `(whitespace-empty       ((t (:background ,lyellow))))
     `(whitespace-line        ((t (:background ,lgrey1))))
     `(whitespace-trailing    ((t (:background ,lred))))
     `(whitespace-big-indent  ((t (:background ,lred))))

     ;; Popup / completion menus (Pmenu)
     `(tooltip                ((t (:foreground ,fg :background ,white))))
     `(completions-common-part      ((t (:foreground ,blue :weight bold))))
     `(completions-first-difference ((t (:weight bold))))
     `(completions-annotations      ((t (:inherit shadow))))
     `(completions-group-title      ((t (:foreground ,grey :slant italic))))
     `(file-name-shadow             ((t (:inherit shadow))))

     ;; Mode line (StatusLine + lualine accents)
     `(mode-line            ((t (:foreground ,fg :background ,lgrey3
                                             :box (:line-width 1 :color ,lgrey3 :style released-button)))))
     `(mode-line-active     ((t (:inherit mode-line))))
     `(mode-line-inactive   ((t (:foreground ,grey :background ,lgrey2
                                             :box (:line-width 1 :color ,lgrey2)))))
     `(mode-line-emphasis   ((t (:foreground ,blue :weight bold))))
     `(mode-line-highlight  ((t (:background ,lblue))))
     `(mode-line-buffer-id  ((t (:weight bold))))
     `(header-line          ((t (:foreground ,fg :background ,lgrey2))))
     `(header-line-highlight ((t (:background ,lblue))))

     ;; Tab bar / tab line (built-in; used by treemacs-tab-bar)
     `(tab-bar              ((t (:foreground ,fg :background ,lgrey2))))
     `(tab-bar-tab          ((t (:foreground ,fg :background ,bg :weight bold
                                             :box (:line-width 2 :color ,bg)))))
     `(tab-bar-tab-inactive ((t (:foreground ,grey :background ,lgrey2
                                             :box (:line-width 2 :color ,lgrey2)))))
     `(tab-bar-tab-group-current  ((t (:foreground ,fg :weight bold))))
     `(tab-bar-tab-group-inactive ((t (:foreground ,grey))))
     `(tab-line             ((t (:foreground ,fg :background ,lgrey2))))
     `(tab-line-tab         ((t (:foreground ,fg :background ,bg))))
     `(tab-line-tab-current ((t (:foreground ,fg :background ,bg :weight bold))))
     `(tab-line-tab-inactive ((t (:foreground ,grey :background ,lgrey2))))
     `(tab-line-highlight   ((t (:background ,lblue))))

     ;; Misc UI
     `(widget-field        ((t (:background ,white :box (:line-width 1 :color ,lgrey2)))))
     `(widget-button       ((t (:inherit button))))
     `(widget-inactive     ((t (:inherit shadow))))
     `(custom-button       ((t (:foreground ,fg :background ,lgrey1
                                            :box (:line-width 1 :color ,lgrey2)))))
     `(custom-button-mouse ((t (:foreground ,fg :background ,lblue
                                            :box (:line-width 1 :color ,lgrey2)))))
     `(custom-button-pressed ((t (:foreground ,white :background ,blue
                                              :box (:line-width 1 :color ,blue)))))
     `(custom-group-tag    ((t (:foreground ,blue :weight bold :height 1.2))))
     `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
     `(custom-state        ((t (:foreground ,green))))
     `(next-error          ((t (:background ,lblue))))
     `(next-error-message  ((t (:background ,lgrey1))))
     `(pulse-highlight-start-face ((t (:background ,lblue))))

     ;;; -------------------------------------------------------------------
     ;;; font-lock (the syntax layer; reuses the per-variant token specs).
     ;;; Emacs 30 tree-sitter modes reuse these faces, so they are covered.
     ;;; -------------------------------------------------------------------
     `(font-lock-comment-face           ((t ,tk-comment)))
     `(font-lock-comment-delimiter-face ((t ,tk-comment)))
     `(font-lock-doc-face               ((t ,tk-comment)))
     `(font-lock-doc-markup-face        ((t ,tk-macro)))
     `(font-lock-string-face            ((t ,tk-string)))
     `(font-lock-keyword-face           ((t ,tk-keyword)))
     `(font-lock-builtin-face           ((t ,tk-keyword)))
     `(font-lock-function-name-face     ((t ,tk-func)))
     `(font-lock-function-call-face     ((t ,tk-func)))
     `(font-lock-variable-name-face     ((t ,tk-ident)))
     `(font-lock-variable-use-face      ((t ,tk-ident)))
     `(font-lock-type-face              ((t ,tk-type)))
     `(font-lock-constant-face          ((t ,tk-const)))
     `(font-lock-number-face            ((t ,tk-number)))
     `(font-lock-property-name-face     ((t ,tk-prop)))
     `(font-lock-property-use-face      ((t ,tk-prop)))
     `(font-lock-preprocessor-face      ((t ,tk-macro)))
     `(font-lock-macro-name-face        ((t ,tk-macro)))
     `(font-lock-operator-face          ((t ,tk-op)))
     `(font-lock-delimiter-face         ((t ,tk-op)))
     `(font-lock-punctuation-face       ((t ,tk-op)))
     `(font-lock-bracket-face           ((t (:foreground ,fg))))
     `(font-lock-misc-punctuation-face  ((t ,tk-op)))
     `(font-lock-escape-face            ((t ,tk-special)))
     `(font-lock-negation-char-face     ((t ,tk-op)))
     `(font-lock-regexp-face            ((t ,tk-regexp)))
     `(font-lock-regexp-grouping-backslash ((t ,tk-regexp)))
     `(font-lock-regexp-grouping-construct ((t ,tk-regexp)))
     `(font-lock-label-face             ((t ,tk-special)))
     `(font-lock-warning-face           ((t (:foreground ,red :weight bold))))
     `(elisp-shorthand-font-lock-face   ((t (:foreground ,purple))))

     ;;; -------------------------------------------------------------------
     ;;; Diagnostics / LSP (groups/lsp.lua)
     ;;; -------------------------------------------------------------------
     `(flymake-error        ((t (:underline (:style wave :color ,red)))))
     `(flymake-warning      ((t (:underline (:style wave :color ,orange)))))
     `(flymake-note         ((t (:underline (:style wave :color ,cyan)))))
     `(flymake-error-echo   ((t (:foreground ,red))))
     `(flymake-warning-echo ((t (:foreground ,orange))))
     `(flymake-note-echo    ((t (:foreground ,cyan))))
     `(flymake-end-of-line-diagnostics-face ((t (:inherit shadow :height 0.85))))
     `(compilation-error    ((t (:foreground ,red :weight bold))))
     `(compilation-warning  ((t (:foreground ,orange :weight bold))))
     `(compilation-info     ((t (:foreground ,cyan))))
     `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))
     `(compilation-mode-line-run  ((t (:foreground ,orange))))
     `(compilation-mode-line-exit ((t (:foreground ,green :weight bold))))
     `(compilation-line-number    ((t (:foreground ,grey))))
     `(compilation-column-number  ((t (:foreground ,grey))))

     ;; flyspell / jinx (the user's spell-checker)
     `(flyspell-incorrect   ((t (:underline (:style wave :color ,red)))))
     `(flyspell-duplicate   ((t (:underline (:style wave :color ,orange)))))
     `(jinx-misspelled      ((t (:underline (:style wave :color ,red)))))
     `(jinx-highlight       ((t (:background ,lblue))))
     `(jinx-accept          ((t (:inherit shadow))))

     ;; eglot
     `(eglot-highlight-symbol-face        ((t (:background ,lblue))))
     `(eglot-mode-line                    ((t (:foreground ,blue :weight bold))))
     `(eglot-inlay-hint-face              ((t (:foreground ,grey :background ,lgrey1 :slant italic :height 0.9))))
     `(eglot-parameter-hint-face          ((t (:inherit eglot-inlay-hint-face))))
     `(eglot-type-hint-face               ((t (:inherit eglot-inlay-hint-face))))
     `(eglot-diagnostic-tag-unnecessary-face ((t (:inherit shadow :underline (:style wave :color ,grey)))))
     `(eglot-diagnostic-tag-deprecated-face  ((t (:foreground ,grey :strike-through t))))

     ;; xref
     `(xref-file-header     ((t (:foreground ,fg :weight bold))))
     `(xref-line-number     ((t (:foreground ,grey))))
     `(xref-match           ((t (:background ,lblue :weight bold))))

     ;;; -------------------------------------------------------------------
     ;;; Completion stack (corfu / vertico / orderless / consult / …)
     ;;; -------------------------------------------------------------------
     ;; corfu (Pmenu)
     `(corfu-default     ((t (:foreground ,fg :background ,white))))
     `(corfu-current     ((t (:foreground ,fg :background ,lgrey1 :weight bold))))
     `(corfu-bar         ((t (:background ,lgrey2))))
     `(corfu-border      ((t (:background ,lgrey2))))
     `(corfu-popupinfo   ((t (:inherit corfu-default))))
     `(corfu-deprecated  ((t (:inherit shadow :strike-through t))))
     `(corfu-annotations ((t (:inherit shadow))))

     ;; vertico
     `(vertico-current        ((t (:background ,lgrey1 :weight bold :extend t))))
     `(vertico-group-title    ((t (:foreground ,grey :weight bold :slant italic))))
     `(vertico-group-separator ((t (:foreground ,lgrey2 :strike-through t))))
     `(vertico-mouse          ((t (:background ,lblue))))
     `(vertico-quick1         ((t (:foreground ,white :background ,blue :weight bold))))
     `(vertico-quick2         ((t (:foreground ,white :background ,purple :weight bold))))

     ;; orderless (matching groups)
     `(orderless-match-face-0 ((t (:foreground ,blue :weight bold))))
     `(orderless-match-face-1 ((t (:foreground ,purple :weight bold))))
     `(orderless-match-face-2 ((t (:foreground ,cyan :weight bold))))
     `(orderless-match-face-3 ((t (:foreground ,green :weight bold))))

     ;; marginalia
     `(marginalia-key           ((t (:foreground ,blue))))
     `(marginalia-documentation ((t (:inherit font-lock-comment-face))))
     `(marginalia-file-name     ((t (:inherit shadow))))
     `(marginalia-file-priv-dir ((t (:foreground ,blue))))
     `(marginalia-file-priv-read  ((t (:foreground ,green))))
     `(marginalia-file-priv-write ((t (:foreground ,orange))))
     `(marginalia-file-priv-exec  ((t (:foreground ,red))))
     `(marginalia-number        ((t (:foreground ,blue))))
     `(marginalia-size          ((t (:foreground ,grey))))
     `(marginalia-date          ((t (:foreground ,grey))))
     `(marginalia-mode          ((t (:foreground ,grey))))
     `(marginalia-modified      ((t (:foreground ,orange))))
     `(marginalia-type          ((t (:foreground ,cyan))))
     `(marginalia-char          ((t (:foreground ,purple))))
     `(marginalia-symbol        ((t (:foreground ,cyan))))

     ;; consult
     `(consult-line-number         ((t (:foreground ,grey))))
     `(consult-line-number-prefix  ((t (:foreground ,grey))))
     `(consult-line-number-wrapped ((t (:foreground ,orange))))
     `(consult-preview-line        ((t (:background ,lgrey1))))
     `(consult-preview-match       ((t (:background ,lblue :weight bold))))
     `(consult-preview-cursor      ((t (:background ,lblue))))
     `(consult-async-split         ((t (:foreground ,red))))
     `(consult-async-running       ((t (:foreground ,orange))))
     `(consult-async-finished      ((t (:foreground ,green))))
     `(consult-key                 ((t (:foreground ,blue))))
     `(consult-imenu-prefix        ((t (:inherit shadow))))
     `(consult-bookmark            ((t (:foreground ,blue))))
     `(consult-file                ((t (:foreground ,fg))))
     `(consult-separator           ((t (:foreground ,lgrey2))))

     ;; embark
     `(embark-keybinding   ((t (:foreground ,blue))))
     `(embark-target       ((t (:background ,lblue))))
     `(embark-collect-group-title  ((t (:foreground ,grey :weight bold :slant italic))))
     `(embark-collect-marked       ((t (:foreground ,purple))))
     `(embark-verbose-indicator-documentation ((t (:inherit font-lock-comment-face))))

     ;; which-key
     `(which-key-key-face                 ((t (:foreground ,blue))))
     `(which-key-group-description-face   ((t (:foreground ,fg :weight bold))))
     `(which-key-command-description-face ((t (:foreground ,fg))))
     `(which-key-separator-face           ((t (:inherit shadow))))
     `(which-key-note-face                ((t (:inherit shadow))))
     `(which-key-local-map-description-face ((t (:foreground ,purple))))
     `(which-key-highlighted-command-face ((t (:foreground ,blue :underline t))))
     `(which-key-docstring-face           ((t (:inherit shadow))))

     ;; nerd-icons-completion / dired (icon faces fall back to fg)

     ;;; -------------------------------------------------------------------
     ;;; Git (magit / diff-mode / diff-hl / git-commit / smerge / ediff)
     ;;; -------------------------------------------------------------------
     `(magit-section-heading        ((t (:foreground ,fg :weight bold))))
     `(magit-section-heading-selection ((t (:foreground ,blue :weight bold))))
     `(magit-section-highlight      ((t (:background ,lgrey1 :extend t))))
     `(magit-section-secondary-heading ((t (:foreground ,grey :weight bold))))
     `(magit-header-line            ((t (:foreground ,fg :background ,lgrey2 :weight bold))))
     `(magit-diff-added             ((t (:foreground ,green :background ,lgrey1 :extend t))))
     `(magit-diff-added-highlight   ((t (:foreground ,green :background ,lcyan :extend t))))
     `(magit-diff-removed           ((t (:foreground ,red :background ,lgrey1 :extend t))))
     `(magit-diff-removed-highlight ((t (:foreground ,red :background ,lred :extend t))))
     `(magit-diff-context           ((t (:foreground ,grey :extend t))))
     `(magit-diff-context-highlight ((t (:foreground ,grey :background ,lgrey1 :extend t))))
     `(magit-diff-hunk-heading      ((t (:foreground ,grey :background ,lgrey1 :weight bold :extend t))))
     `(magit-diff-hunk-heading-highlight ((t (:foreground ,fg :background ,lgrey2 :weight bold :extend t))))
     `(magit-diff-hunk-heading-selection ((t (:foreground ,blue :background ,lgrey2 :weight bold :extend t))))
     `(magit-diff-lines-heading     ((t (:foreground ,white :background ,blue :extend t))))
     `(magit-diff-file-heading      ((t (:foreground ,fg :weight bold))))
     `(magit-diff-file-heading-highlight ((t (:foreground ,fg :background ,lgrey1 :weight bold))))
     `(magit-diffstat-added         ((t (:foreground ,green))))
     `(magit-diffstat-removed       ((t (:foreground ,red))))
     `(magit-branch-local           ((t (:foreground ,purple :weight bold))))
     `(magit-branch-remote          ((t (:foreground ,green :weight bold))))
     `(magit-branch-current         ((t (:foreground ,purple :weight bold :box (:line-width 1 :color ,purple)))))
     `(magit-head                   ((t (:foreground ,purple :weight bold))))
     `(magit-tag                    ((t (:foreground ,orange))))
     `(magit-hash                   ((t (:foreground ,purple))))
     `(magit-log-author             ((t (:foreground ,blue))))
     `(magit-log-date               ((t (:foreground ,grey))))
     `(magit-log-graph              ((t (:foreground ,grey))))
     `(magit-dimmed                 ((t (:inherit shadow))))
     `(magit-filename               ((t (:foreground ,fg))))
     `(magit-process-ok             ((t (:foreground ,green :weight bold))))
     `(magit-process-ng             ((t (:foreground ,red :weight bold))))
     `(magit-bisect-good            ((t (:foreground ,green))))
     `(magit-bisect-bad             ((t (:foreground ,red))))
     `(magit-bisect-skip            ((t (:foreground ,orange))))
     `(magit-cherry-equivalent      ((t (:foreground ,purple))))
     `(magit-cherry-unmatched       ((t (:foreground ,cyan))))
     `(magit-signature-good         ((t (:foreground ,green))))
     `(magit-signature-bad          ((t (:foreground ,red :weight bold))))
     `(magit-signature-untrusted    ((t (:foreground ,orange))))
     `(magit-reflog-commit          ((t (:foreground ,green))))
     `(magit-reflog-amend           ((t (:foreground ,purple))))
     `(magit-reflog-merge           ((t (:foreground ,green))))
     `(magit-reflog-checkout        ((t (:foreground ,blue))))
     `(magit-reflog-reset           ((t (:foreground ,red))))
     `(magit-reflog-rebase          ((t (:foreground ,purple))))
     `(magit-reflog-cherry-pick     ((t (:foreground ,green))))
     `(magit-blame-heading          ((t (:foreground ,fg :background ,lgrey1 :weight bold))))
     `(magit-blame-date             ((t (:foreground ,blue :background ,lgrey1))))
     `(magit-blame-name             ((t (:foreground ,purple :background ,lgrey1))))
     `(magit-blame-hash             ((t (:foreground ,purple :background ,lgrey1))))

     ;; diff-mode
     `(diff-added          ((t (:foreground ,green :background ,lgrey1 :extend t))))
     `(diff-removed        ((t (:foreground ,red :background ,lgrey1 :extend t))))
     `(diff-changed        ((t (:foreground ,orange :background ,lgrey1 :extend t))))
     `(diff-changed-unspecified ((t (:foreground ,orange :background ,lgrey1 :extend t))))
     `(diff-indicator-added   ((t (:foreground ,green :background ,lgrey1))))
     `(diff-indicator-removed ((t (:foreground ,red :background ,lgrey1))))
     `(diff-indicator-changed ((t (:foreground ,orange :background ,lgrey1))))
     `(diff-refine-added   ((t (:foreground ,green :background ,lcyan))))
     `(diff-refine-removed ((t (:foreground ,red :background ,lred))))
     `(diff-refine-changed ((t (:foreground ,orange :background ,lyellow))))
     `(diff-header         ((t (:foreground ,fg :background ,lgrey1 :extend t))))
     `(diff-file-header    ((t (:foreground ,fg :weight bold :extend t))))
     `(diff-hunk-header    ((t (:foreground ,grey :background ,lgrey1 :weight bold :extend t))))
     `(diff-function       ((t (:foreground ,grey))))
     `(diff-context        ((t (:foreground ,grey))))
     `(diff-nonexistent    ((t (:foreground ,red :weight bold))))

     ;; diff-hl (fringe)
     `(diff-hl-insert       ((t (:foreground ,green :background ,green))))
     `(diff-hl-change       ((t (:foreground ,orange :background ,orange))))
     `(diff-hl-delete       ((t (:foreground ,red :background ,red))))
     `(diff-hl-reverted-hunk-highlight ((t (:foreground ,white :background ,red))))
     `(diff-hl-dired-insert ((t (:foreground ,green))))
     `(diff-hl-dired-change ((t (:foreground ,orange))))
     `(diff-hl-dired-delete ((t (:foreground ,red))))

     ;; git-commit / log-edit
     `(git-commit-summary             ((t (:foreground ,green))))
     `(git-commit-overflow            ((t (:inherit error))))
     `(git-commit-nonempty-second-line ((t (:foreground ,orange))))
     `(git-commit-comment-heading     ((t (:foreground ,fg :weight bold))))
     `(git-commit-comment-file        ((t (:foreground ,blue))))
     `(git-commit-comment-branch-local  ((t (:foreground ,purple :weight bold))))
     `(git-commit-comment-branch-remote ((t (:foreground ,green :weight bold))))
     `(git-commit-comment-action      ((t (:inherit shadow))))
     `(log-edit-summary               ((t (:foreground ,green))))
     `(log-edit-header                ((t (:foreground ,fg :weight bold))))

     ;; smerge
     `(smerge-upper         ((t (:background ,lred :extend t))))
     `(smerge-lower         ((t (:background ,lcyan :extend t))))
     `(smerge-base          ((t (:background ,lyellow :extend t))))
     `(smerge-markers       ((t (:background ,lgrey1 :weight bold :extend t))))
     `(smerge-refined-added   ((t (:foreground ,green :background ,lcyan))))
     `(smerge-refined-removed ((t (:foreground ,red :background ,lred))))

     ;; ediff
     `(ediff-current-diff-A    ((t (:background ,lred :extend t))))
     `(ediff-current-diff-B    ((t (:background ,lcyan :extend t))))
     `(ediff-current-diff-C    ((t (:background ,lyellow :extend t))))
     `(ediff-current-diff-Ancestor ((t (:background ,lgrey1 :extend t))))
     `(ediff-fine-diff-A       ((t (:foreground ,red :background ,lred :weight bold))))
     `(ediff-fine-diff-B       ((t (:foreground ,green :background ,lcyan :weight bold))))
     `(ediff-fine-diff-C       ((t (:foreground ,orange :background ,lyellow :weight bold))))
     `(ediff-even-diff-A       ((t (:background ,lgrey1 :extend t))))
     `(ediff-even-diff-B       ((t (:background ,lgrey1 :extend t))))
     `(ediff-even-diff-C       ((t (:background ,lgrey1 :extend t))))
     `(ediff-odd-diff-A        ((t (:background ,lgrey1 :extend t))))
     `(ediff-odd-diff-B        ((t (:background ,lgrey1 :extend t))))
     `(ediff-odd-diff-C        ((t (:background ,lgrey1 :extend t))))

     ;;; -------------------------------------------------------------------
     ;;; File trees, tabs, buffer lists, side panels
     ;;; -------------------------------------------------------------------
     ;; treemacs (+ treemacs-tab-bar via built-in tab-bar above)
     `(treemacs-root-face            ((t (:foreground ,fg :weight bold :height 1.1))))
     `(treemacs-root-unreadable-face ((t (:foreground ,red :weight bold))))
     `(treemacs-root-remote-face     ((t (:foreground ,blue :weight bold))))
     `(treemacs-directory-face       ((t (:foreground ,fg))))
     `(treemacs-directory-collapsed-face ((t (:foreground ,fg))))
     `(treemacs-file-face            ((t (:foreground ,fg))))
     `(treemacs-tags-face            ((t (:foreground ,fg))))
     `(treemacs-term-node-face       ((t (:foreground ,blue :weight bold))))
     `(treemacs-fringe-indicator-face ((t (:foreground ,blue))))
     `(treemacs-on-success-pulse-face ((t (:foreground ,white :background ,green))))
     `(treemacs-on-failure-pulse-face ((t (:foreground ,white :background ,red))))
     `(treemacs-git-added-face       ((t (:foreground ,green))))
     `(treemacs-git-modified-face    ((t (:foreground ,orange))))
     `(treemacs-git-renamed-face     ((t (:foreground ,blue))))
     `(treemacs-git-conflict-face    ((t (:foreground ,red :weight bold))))
     `(treemacs-git-ignored-face     ((t (:foreground ,lgrey2))))
     `(treemacs-git-untracked-face   ((t (:foreground ,grey))))
     `(treemacs-git-unmodified-face  ((t (:foreground ,fg))))
     `(treemacs-git-commit-diff-face ((t (:foreground ,orange))))
     `(treemacs-window-background-face ((t (:background ,white))))
     `(treemacs-hl-line-face         ((t (:background ,lgrey1 :extend t))))
     `(treemacs-peek-mode-indicator-face ((t (:foreground ,white :background ,blue))))

     ;; centaur-tabs
     `(centaur-tabs-default            ((t (:foreground ,grey :background ,lgrey2))))
     `(centaur-tabs-unselected         ((t (:foreground ,grey :background ,lgrey2))))
     `(centaur-tabs-selected           ((t (:foreground ,fg :background ,bg :weight bold))))
     `(centaur-tabs-unselected-modified ((t (:foreground ,orange :background ,lgrey2))))
     `(centaur-tabs-selected-modified  ((t (:foreground ,orange :background ,bg :weight bold))))
     `(centaur-tabs-active-bar-face    ((t (:background ,blue))))
     `(centaur-tabs-modified-marker-selected   ((t (:foreground ,orange :background ,bg))))
     `(centaur-tabs-modified-marker-unselected ((t (:foreground ,orange :background ,lgrey2))))
     `(centaur-tabs-close-selected     ((t (:foreground ,red :background ,bg))))
     `(centaur-tabs-close-unselected   ((t (:foreground ,grey :background ,lgrey2))))
     `(centaur-tabs-close-mouse-face   ((t (:foreground ,red))))
     `(centaur-tabs-name-mouse-face    ((t (:foreground ,blue :weight bold))))

     ;; dired
     `(dired-directory   ((t (:foreground ,blue :weight bold))))
     `(dired-header      ((t (:foreground ,fg :weight bold))))
     `(dired-symlink     ((t (:foreground ,cyan))))
     `(dired-broken-symlink ((t (:foreground ,white :background ,red))))
     `(dired-mark        ((t (:foreground ,purple :weight bold))))
     `(dired-marked      ((t (:foreground ,purple :weight bold))))
     `(dired-flagged     ((t (:foreground ,red :weight bold))))
     `(dired-perm-write  ((t (:foreground ,orange))))
     `(dired-set-id      ((t (:foreground ,red))))
     `(dired-special     ((t (:foreground ,cyan))))
     `(dired-ignored     ((t (:inherit shadow))))
     `(dired-warning     ((t (:foreground ,orange :weight bold))))

     ;; bufler
     `(bufler-group          ((t (:foreground ,fg :weight bold))))
     `(bufler-path           ((t (:foreground ,grey))))
     `(bufler-dim            ((t (:inherit shadow))))
     `(bufler-buffer         ((t (:foreground ,fg))))
     `(bufler-buffer-special ((t (:foreground ,cyan))))
     `(bufler-mode           ((t (:foreground ,grey))))
     `(bufler-size           ((t (:foreground ,grey))))
     `(bufler-vc             ((t (:foreground ,green))))

     ;; speedbar / sr-speedbar
     `(speedbar-directory-face ((t (:foreground ,blue :weight bold))))
     `(speedbar-file-face      ((t (:foreground ,fg))))
     `(speedbar-selected-face  ((t (:foreground ,red :underline t))))
     `(speedbar-highlight-face ((t (:background ,lblue))))
     `(speedbar-button-face    ((t (:foreground ,grey))))
     `(speedbar-tag-face       ((t (:foreground ,green))))
     `(speedbar-separator-face ((t (:foreground ,white :background ,blue))))

     ;; ibuffer
     `(ibuffer-locked-buffer ((t (:foreground ,red))))

     ;; solaire-mode (side / "non-real" buffers get the whiter background)
     `(solaire-default-face     ((t (:foreground ,fg :background ,white))))
     `(solaire-fringe-face      ((t (:background ,white))))
     `(solaire-line-number-face ((t (:foreground ,grey :background ,white))))
     `(solaire-hl-line-face     ((t (:background ,lgrey2 :extend t))))
     `(solaire-region-face      ((t (:background ,lblue))))
     `(solaire-mode-line-face          ((t (:inherit mode-line))))
     `(solaire-mode-line-inactive-face ((t (:inherit mode-line-inactive))))
     `(solaire-header-line-face ((t (:inherit header-line))))

     ;;; -------------------------------------------------------------------
     ;;; doom-modeline
     ;;; -------------------------------------------------------------------
     `(doom-modeline-bar               ((t (:background ,blue))))
     `(doom-modeline-bar-inactive      ((t (:background ,lgrey2))))
     `(doom-modeline-buffer-file       ((t (:foreground ,fg :weight bold))))
     `(doom-modeline-buffer-modified   ((t (:foreground ,orange :weight bold))))
     `(doom-modeline-buffer-path       ((t (:foreground ,grey))))
     `(doom-modeline-project-dir       ((t (:foreground ,blue :weight bold))))
     `(doom-modeline-project-root-dir  ((t (:foreground ,grey))))
     `(doom-modeline-buffer-major-mode ((t (:foreground ,fg :weight bold))))
     `(doom-modeline-buffer-minor-mode ((t (:foreground ,grey))))
     `(doom-modeline-info              ((t (:foreground ,green))))
     `(doom-modeline-warning           ((t (:foreground ,orange))))
     `(doom-modeline-urgent            ((t (:foreground ,red :weight bold))))
     `(doom-modeline-debug             ((t (:foreground ,grey))))
     `(doom-modeline-highlight         ((t (:foreground ,white :background ,blue))))
     `(doom-modeline-panel             ((t (:foreground ,white :background ,blue :weight bold))))
     `(doom-modeline-time              ((t (:foreground ,grey))))
     `(doom-modeline-host              ((t (:foreground ,grey :slant italic))))
     `(doom-modeline-input-method      ((t (:foreground ,purple :weight bold))))
     `(doom-modeline-spc-face          ((t (:foreground ,grey))))
     `(doom-modeline-lsp-success       ((t (:foreground ,green))))
     `(doom-modeline-lsp-warning       ((t (:foreground ,orange))))
     `(doom-modeline-lsp-error         ((t (:foreground ,red))))
     `(doom-modeline-lsp-running       ((t (:foreground ,orange))))
     `(doom-modeline-bar-icon          ((t (:foreground ,blue))))
     `(doom-modeline-notification      ((t (:foreground ,red))))
     `(doom-modeline-evil-normal-state  ((t (:foreground ,blue :weight bold))))
     `(doom-modeline-evil-insert-state  ((t (:foreground ,green :weight bold))))
     `(doom-modeline-evil-visual-state  ((t (:foreground ,purple :weight bold))))
     `(doom-modeline-evil-replace-state ((t (:foreground ,red :weight bold))))
     `(doom-modeline-evil-operator-state ((t (:foreground ,cyan :weight bold))))
     `(doom-modeline-evil-motion-state  ((t (:foreground ,cyan :weight bold))))
     `(doom-modeline-evil-emacs-state   ((t (:foreground ,orange :weight bold))))

     ;;; -------------------------------------------------------------------
     ;;; Editing aids, navigation, languages
     ;;; -------------------------------------------------------------------
     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,orange))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,purple))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,cyan))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,brblack))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,red))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,grey))))
     `(rainbow-delimiters-unmatched-face ((t (:foreground ,white :background ,red :weight bold))))
     `(rainbow-delimiters-mismatched-face ((t (:foreground ,white :background ,red :weight bold))))

     ;; avy
     `(avy-lead-face        ((t (:foreground ,white :background ,red :weight bold))))
     `(avy-lead-face-0      ((t (:foreground ,white :background ,blue :weight bold))))
     `(avy-lead-face-1      ((t (:foreground ,white :background ,orange :weight bold))))
     `(avy-lead-face-2      ((t (:foreground ,white :background ,purple :weight bold))))
     `(avy-background-face  ((t (:foreground ,grey))))
     `(avy-goto-char-timer-face ((t (:background ,lyellow :weight bold))))

     ;; outline (reused by outli + markdown/org headers)
     `(outline-1 ((t (:foreground ,fg :weight bold))))
     `(outline-2 ((t (:foreground ,blue :weight bold))))
     `(outline-3 ((t (:foreground ,purple :weight bold))))
     `(outline-4 ((t (:foreground ,cyan :weight bold))))
     `(outline-5 ((t (:foreground ,green :weight bold))))
     `(outline-6 ((t (:foreground ,orange :weight bold))))
     `(outline-7 ((t (:foreground ,red :weight bold))))
     `(outline-8 ((t (:foreground ,grey :weight bold))))
     `(outline-minor-0 ((t (:background ,lgrey1))))

     ;; markdown-mode
     `(markdown-header-face            ((t (:foreground ,fg :weight bold))))
     `(markdown-header-face-1          ((t (:inherit outline-1 :height 1.3))))
     `(markdown-header-face-2          ((t (:inherit outline-2 :height 1.2))))
     `(markdown-header-face-3          ((t (:inherit outline-3 :height 1.1))))
     `(markdown-header-face-4          ((t (:inherit outline-4))))
     `(markdown-header-face-5          ((t (:inherit outline-5))))
     `(markdown-header-face-6          ((t (:inherit outline-6))))
     `(markdown-header-delimiter-face  ((t (:foreground ,grey))))
     `(markdown-header-rule-face       ((t (:foreground ,grey))))
     `(markdown-bold-face              ((t (:weight bold))))
     `(markdown-italic-face            ((t (:slant italic))))
     `(markdown-strike-through-face    ((t (:strike-through t))))
     `(markdown-code-face              ((t (:foreground ,grey :background ,lgrey1 :extend t))))
     `(markdown-pre-face               ((t (:foreground ,grey :background ,lgrey1 :extend t))))
     `(markdown-inline-code-face       ((t (:foreground ,grey :background ,lgrey1))))
     `(markdown-link-face              ((t (:foreground ,blue :underline t))))
     `(markdown-url-face               ((t (:foreground ,blue))))
     `(markdown-plain-url-face         ((t (:foreground ,blue :underline t))))
     `(markdown-link-title-face        ((t (:foreground ,grey))))
     `(markdown-list-face              ((t ,tk-keyword)))
     `(markdown-blockquote-face        ((t (:inherit font-lock-comment-face :slant italic))))
     `(markdown-markup-face            ((t (:foreground ,grey))))
     `(markdown-gfm-checkbox-face      ((t (:foreground ,blue))))
     `(markdown-table-face             ((t (:foreground ,blue))))
     `(markdown-metadata-key-face      ((t (:foreground ,grey))))
     `(markdown-metadata-value-face    ((t (:foreground ,fg))))
     `(markdown-html-tag-name-face     ((t (:foreground ,fg :weight bold))))
     `(markdown-html-attr-name-face    ((t (:foreground ,fg))))

     ;; org-mode (built-in)
     `(org-level-1 ((t (:inherit outline-1 :height 1.3))))
     `(org-level-2 ((t (:inherit outline-2 :height 1.2))))
     `(org-level-3 ((t (:inherit outline-3 :height 1.1))))
     `(org-level-4 ((t (:inherit outline-4))))
     `(org-level-5 ((t (:inherit outline-5))))
     `(org-level-6 ((t (:inherit outline-6))))
     `(org-level-7 ((t (:inherit outline-7))))
     `(org-level-8 ((t (:inherit outline-8))))
     `(org-document-title    ((t (:foreground ,fg :weight bold :height 1.4))))
     `(org-document-info     ((t (:foreground ,grey))))
     `(org-document-info-keyword ((t (:foreground ,grey))))
     `(org-block             ((t (:background ,lgrey1 :extend t))))
     `(org-block-begin-line  ((t (:foreground ,grey :background ,lgrey1 :extend t))))
     `(org-block-end-line    ((t (:foreground ,grey :background ,lgrey1 :extend t))))
     `(org-code              ((t (:foreground ,grey :background ,lgrey1))))
     `(org-verbatim          ((t (:foreground ,grey :background ,lgrey1))))
     `(org-table             ((t (:foreground ,blue))))
     `(org-formula           ((t (:foreground ,orange))))
     `(org-link              ((t (:foreground ,blue :underline t))))
     `(org-footnote          ((t (:foreground ,cyan :underline t))))
     `(org-todo              ((t (:foreground ,red :weight bold))))
     `(org-done              ((t (:foreground ,green :weight bold))))
     `(org-headline-todo     ((t (:foreground ,fg))))
     `(org-headline-done     ((t (:foreground ,grey :strike-through t))))
     `(org-date              ((t (:foreground ,cyan :underline t))))
     `(org-special-keyword   ((t (:foreground ,grey))))
     `(org-drawer            ((t (:foreground ,grey))))
     `(org-property-value    ((t (:foreground ,fg))))
     `(org-tag               ((t (:foreground ,grey :weight bold))))
     `(org-priority          ((t (:foreground ,orange :weight bold))))
     `(org-checkbox          ((t (:foreground ,blue :weight bold))))
     `(org-ellipsis          ((t (:foreground ,grey :underline nil))))
     `(org-warning           ((t (:foreground ,red :weight bold))))
     `(org-agenda-structure  ((t (:foreground ,blue :weight bold))))
     `(org-agenda-date       ((t (:foreground ,fg :weight bold))))
     `(org-agenda-date-today ((t (:foreground ,blue :weight bold :underline t))))
     `(org-agenda-date-weekend ((t (:foreground ,grey :weight bold))))
     `(org-scheduled         ((t (:foreground ,green))))
     `(org-scheduled-today   ((t (:foreground ,green :weight bold))))
     `(org-scheduled-previously ((t (:foreground ,orange))))
     `(org-upcoming-deadline ((t (:foreground ,orange))))
     `(org-hide              ((t (:foreground ,bg :background ,bg))))

     ;; AUCTeX / font-latex
     `(font-latex-math-face         ((t (:foreground ,blue))))
     `(font-latex-sedate-face       ((t (:foreground ,grey))))
     `(font-latex-script-char-face  ((t (:foreground ,grey))))
     `(font-latex-string-face       ((t ,tk-string)))
     `(font-latex-warning-face      ((t (:foreground ,red :weight bold))))
     `(font-latex-bold-face         ((t (:foreground ,fg :weight bold))))
     `(font-latex-italic-face       ((t (:foreground ,fg :slant italic))))
     `(font-latex-verbatim-face     ((t (:inherit font-lock-comment-face))))
     `(font-latex-doctex-documentation-face ((t (:inherit font-lock-comment-face))))
     `(font-latex-doctex-preprocessor-face  ((t (:foreground ,purple))))
     `(font-latex-sectioning-0-face ((t (:foreground ,fg :weight bold :height 1.4))))
     `(font-latex-sectioning-1-face ((t (:foreground ,fg :weight bold :height 1.3))))
     `(font-latex-sectioning-2-face ((t (:foreground ,fg :weight bold :height 1.2))))
     `(font-latex-sectioning-3-face ((t (:foreground ,fg :weight bold :height 1.1))))
     `(font-latex-sectioning-4-face ((t (:foreground ,fg :weight bold))))
     `(font-latex-sectioning-5-face ((t (:foreground ,grey :weight bold))))
     `(font-latex-slide-title-face  ((t (:foreground ,fg :weight bold :height 1.3))))
     `(TeX-error-description-error   ((t (:foreground ,red :weight bold))))
     `(TeX-error-description-warning ((t (:foreground ,orange :weight bold))))
     `(TeX-error-description-tex-said ((t (:foreground ,grey))))

     ;; highlight-defined (elisp)
     `(highlight-defined-function-name-face         ((t ,tk-func)))
     `(highlight-defined-builtin-function-name-face ((t ,tk-keyword)))
     `(highlight-defined-special-form-name-face     ((t (:foreground ,purple))))
     `(highlight-defined-macro-name-face            ((t (:foreground ,purple))))
     `(highlight-defined-variable-name-face         ((t (:foreground ,fg))))
     `(highlight-defined-face-name-face             ((t (:foreground ,cyan))))

     ;; goggles (pulse modified region)
     `(goggles-added   ((t (:background ,lcyan))))
     `(goggles-changed ((t (:background ,lyellow))))
     `(goggles-removed ((t (:background ,lred))))

     ;; show-inactive-region
     `(show-inactive-region-face ((t (:background ,lgrey1 :extend t))))

     ;; yasnippet
     `(yas-field-highlight-face ((t (:background ,lgrey1))))

     ;; page-break-lines
     `(page-break-lines ((t (:foreground ,lgrey2))))

     ;; pdf-tools
     `(pdf-isearch-match    ((t (:background ,lblue))))
     `(pdf-isearch-lazy     ((t (:background ,lcyan))))
     `(pdf-isearch-batch    ((t (:background ,lyellow))))
     `(pdf-links-read-link  ((t (:foreground ,white :background ,red))))

     ;; which-function
     `(which-func ((t (:foreground ,blue))))

     ;; hl-todo / generic TODO keyword face (built-in font-lock + outli notes)
     `(hl-todo ((t (:foreground ,cyan :weight bold))))

     ;; info / help
     `(info-title-1     ((t (:foreground ,fg :weight bold :height 1.3))))
     `(info-title-2     ((t (:foreground ,fg :weight bold :height 1.2))))
     `(info-title-3     ((t (:foreground ,fg :weight bold :height 1.1))))
     `(info-title-4     ((t (:foreground ,fg :weight bold))))
     `(info-menu-header ((t (:foreground ,fg :weight bold))))
     `(info-menu-star   ((t (:foreground ,red))))
     `(info-node        ((t (:foreground ,blue :weight bold))))
     `(info-xref        ((t (:inherit link))))
     `(info-xref-visited ((t (:inherit link-visited))))
     `(help-argument-name ((t (:foreground ,blue :slant italic))))
     `(Info-quoted      ((t (:inherit font-lock-constant-face))))

     ;; ANSI / term colours (vterm, eshell, comint)
     `(ansi-color-black          ((t (:foreground ,black   :background ,black))))
     `(ansi-color-red            ((t (:foreground ,red     :background ,red))))
     `(ansi-color-green          ((t (:foreground ,green   :background ,green))))
     `(ansi-color-yellow         ((t (:foreground ,orange  :background ,orange))))
     `(ansi-color-blue           ((t (:foreground ,blue    :background ,blue))))
     `(ansi-color-magenta        ((t (:foreground ,purple  :background ,purple))))
     `(ansi-color-cyan           ((t (:foreground ,cyan    :background ,cyan))))
     `(ansi-color-white          ((t (:foreground ,lgrey3  :background ,lgrey3))))
     `(ansi-color-bright-black   ((t (:foreground ,brblack :background ,brblack))))
     `(ansi-color-bright-red     ((t (:foreground ,red     :background ,red))))
     `(ansi-color-bright-green   ((t (:foreground ,green   :background ,green))))
     `(ansi-color-bright-yellow  ((t (:foreground ,orange  :background ,orange))))
     `(ansi-color-bright-blue    ((t (:foreground ,blue    :background ,blue))))
     `(ansi-color-bright-magenta ((t (:foreground ,purple  :background ,purple))))
     `(ansi-color-bright-cyan    ((t (:foreground ,cyan    :background ,cyan))))
     `(ansi-color-bright-white   ((t (:foreground ,white   :background ,white))))

     ;; term
     `(term-color-black   ((t (:foreground ,black  :background ,black))))
     `(term-color-red     ((t (:foreground ,red    :background ,red))))
     `(term-color-green   ((t (:foreground ,green  :background ,green))))
     `(term-color-yellow  ((t (:foreground ,orange :background ,orange))))
     `(term-color-blue    ((t (:foreground ,blue   :background ,blue))))
     `(term-color-magenta ((t (:foreground ,purple :background ,purple))))
     `(term-color-cyan    ((t (:foreground ,cyan   :background ,cyan))))
     `(term-color-white   ((t (:foreground ,lgrey3 :background ,lgrey3)))))))

(defun envy-themes--apply (theme variant)
  "Apply the Envy faces for VARIANT to THEME, and set frame/terminal colours."
  (apply #'custom-theme-set-faces theme (envy-themes--faces variant))
  (let* ((p envy-themes-palette)
         (g (lambda (k) (cdr (assq k p)))))
    (custom-theme-set-variables
     theme
     `(frame-background-mode 'light)
     ;; ANSI colour vector for term/ansi-color consumers (mirrors palette.terminal)
     `(ansi-color-names-vector
       [,(funcall g 'black) ,(funcall g 'red) ,(funcall g 'green)
        ,(funcall g 'orange) ,(funcall g 'blue) ,(funcall g 'purple)
        ,(funcall g 'cyan) ,(funcall g 'lgrey3)]))))

(defmacro envy-themes-deftheme (name docstring variant)
  "Define an Envy theme NAME with DOCSTRING for VARIANT (`envy' or `colorful')."
  (declare (indent defun))
  `(progn
     (deftheme ,name ,docstring)
     (envy-themes--apply ',name ',variant)
     (provide-theme ',name)
     (when (and (boundp 'custom-theme-load-path) load-file-name)
       (add-to-list 'custom-theme-load-path
                    (file-name-directory load-file-name)))))

(provide 'envy-themes)
;;; envy-themes.el ends here
