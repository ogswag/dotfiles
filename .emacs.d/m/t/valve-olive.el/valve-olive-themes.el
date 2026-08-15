;;; valve-olive-themes.el --- Shared engine for the Valve Olive themes -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Alexander Zakharov
;; URL: https://github.com/ogswag/valve-olive.el
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0.0
;; Keywords: faces, themes
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Valve Olive is a dark olive-green theme whose palette is lifted from the
;; classic ~2010 Steam skin.
;;
;;	* valve-olive           the standard olive backgrounds;
;;	* valve-olive-darker   the same hue shifted darker;
;;
;; This file is the shared engine.  The user-facing themes live in
;; `valve-olive-theme.el' and `valve-olive-darker-theme.el', which simply
;; require this and expand the `valve-olive-themes-deftheme' macro.
;;; Code:

(defconst valve-olive-themes-palette
  '(;; Backgrounds (dark -> light), tuned to the Steam UI.
    (bg-dark          . "#363d2e")  ; floats, popups, gutter column, inactive modeline
    (bg               . "#3e4636")  ; Normal / main editor
    (bg-light         . "#474f3d")  ; hl-line, modeline, folds
    (bg-sel           . "#515a44")  ; Pmenu selection, quickfix, LSP references
    (bg-hl            . "#5a6349")  ; subtle highlight / borders
    (border           . "#5a6349")

    ;; A near-black used for text-on-accent (cursor text, search fg).
    (black            . "#2f3527")

    ;; Foregrounds.
    (fg               . "#fbfbfa")  ; near-white, main text
    (fg-dim           . "#c2c4b0")  ; muted text, punctuation, inactive
    (comment          . "#8f9579")  ; green-gray comments (subdued but legible)
    (comment-contrast . "#e08d70")  ; terracotta: comments that should stand out
    (gutter           . "#7e8369")  ; line numbers / fold column

    ;; Accents.
    (accent           . "#dff094")  ; yellow-green slider: cursor, MatchParen
    (gold             . "#f2b950")  ; gold active "PageTab": numbers/constants
    (green            . "#bcd47a")  ; strings, additions
    (teal             . "#a6cabb")  ; keywords / cool accent / links
    (func             . "#e7d18d")  ; functions: soft gold-green
    (mauve            . "#c9a6b7")  ; rare: terminal magenta / specials

    ;; Diagnostics / git.
    (red              . "#dd7f6a")  ; errors, deletes
    (warn             . "#e6c065")  ; warnings, changes
    (info             . "#a6cabb")  ; info (== teal)
    (hint             . "#bcd198")  ; hints / types

    ;; Diff backgrounds (hard-coded in the Neovim theme, not variant-adjusted).
    (diff-add         . "#3a4a32")
    (diff-change      . "#454a36")
    (diff-delete      . "#4a3a34")
    (diff-text        . "#55603f")

    ;; The 16 terminal colours (vim.g.terminal_color_0..15).
    (term-black       . "#2f3527")
    (term-red         . "#cf7a66")
    (term-green       . "#aec46f")
    (term-yellow      . "#d9b05c")
    (term-blue        . "#93b8bf")
    (term-magenta     . "#c9a6b7")
    (term-cyan        . "#a6cabb")
    (term-white       . "#e8e8de")
    (term-br-black    . "#6b7459")
    (term-br-red      . "#e08f79")
    (term-br-green    . "#bcd47a")
    (term-br-yellow   . "#dff094")
    (term-br-blue     . "#b0d4d0")
    (term-br-magenta  . "#d8bccb")
    (term-br-cyan     . "#c2ddcf")
    (term-br-white    . "#fbfbfa"))
  "The Valve Olive colour palette, ported from valve-olive.nvim.
This is the standard-background set.  See
`valve-olive-themes-palette-darker' for the `darker' variant's overrides.")

(defconst valve-olive-themes-palette-darker
  '((bg-dark  . "#2a3024")
    (bg       . "#313829")
    (bg-light . "#3a4233")
    (bg-sel   . "#444d39")
    (bg-hl    . "#4c553e")
    (border   . "#4c553e")
    (black    . "#262b1d"))
  "Background overrides for the `valve-olive-darker' variant.
Same olive hue, shifted darker.  Foregrounds and accents are unchanged --
they contrast even more strongly against these.")


;;;; User options

(defgroup valve-olive nil
  "The Valve Olive themes."
  :group 'faces
  :prefix "valve-olive-")

(defvar valve-olive-themes--reloading nil
  "Non-nil while `valve-olive-themes-reload' is running.
`enable-theme' re-runs the `:set' function of every Customized variable it
knows about, which would call us straight back in; this flag breaks that
cycle.")

(defun valve-olive-themes-reload ()
  "Re-apply whichever Valve Olive theme is currently enabled.
Call this after changing a `valve-olive-' option with `setq'.  Changing an
option through Customize (or `customize-set-variable') does this for you."
  (interactive)
  (unless valve-olive-themes--reloading
    (let ((valve-olive-themes--reloading t))
      (dolist (theme '(valve-olive valve-olive-darker))
        (when (memq theme custom-enabled-themes)
          (disable-theme theme)
          (load-theme theme t))))))

(defcustom valve-olive-contrast-comments nil
  "Non-nil renders comments in a contrasting colour instead of blending in.

By default comments use the palette's `comment' (#8f9579), a green-gray that
sits quietly against the olive background.  With this enabled they switch to
`comment-contrast' (#e08d70), a warm terracotta that is deliberately hard to
miss.  Set `valve-olive-comment-color' to pick a different colour."
  :type 'boolean
  :set #'valve-olive-themes--set-option
  :initialize #'custom-initialize-default
  :group 'valve-olive)

(defcustom valve-olive-comment-color nil
  "Explicit colour for comments, or nil to use the palette default.

When nil, the comment colour is derived from
`valve-olive-contrast-comments'.  When set to a colour name or hex string it
always wins, whether or not contrasting comments are enabled -- this is the
supported way to recolour comments without editing the theme."
  :type '(choice (const :tag "Palette default" nil)
                 (color :tag "Colour"))
  :set #'valve-olive-themes--set-option
  :initialize #'custom-initialize-default
  :group 'valve-olive)

(defcustom valve-olive-italic-comments t
  "Non-nil italicizes comments.
Mirrors the `italic_comments' option of the Neovim theme, which is also on by
default.  Comments are the only thing this theme italicizes."
  :type 'boolean
  :set #'valve-olive-themes--set-option
  :initialize #'custom-initialize-default
  :group 'valve-olive)

(defcustom valve-olive-contrast-docstrings t
  "Non-nil gives docstrings the same colour as comments.

This matters only when comments have been recoloured, i.e. when
`valve-olive-contrast-comments' or `valve-olive-comment-color' is in effect.
Set it to nil to keep docstrings in the quiet default comment colour while
line comments stand out."
  :type 'boolean
  :set #'valve-olive-themes--set-option
  :initialize #'custom-initialize-default
  :group 'valve-olive)


;;;; Face construction

(defun valve-olive-themes-palette (variant)
  "Return the resolved palette alist for VARIANT (`default' or `darker')."
  (if (eq variant 'darker)
      (append valve-olive-themes-palette-darker valve-olive-themes-palette)
    valve-olive-themes-palette))

(defun valve-olive-themes-comment-color (&optional variant)
  "Return the colour comments resolve to for VARIANT.
Honours `valve-olive-comment-color' first, then
`valve-olive-contrast-comments', then the palette default."
  (let ((p (valve-olive-themes-palette (or variant 'default))))
    (or valve-olive-comment-color
        (cdr (assq (if valve-olive-contrast-comments 'comment-contrast 'comment)
                   p)))))

(defun valve-olive-themes--faces (variant)
  "Return the face spec list for VARIANT (`default' or `darker').
Each element is suitable as an argument to `custom-theme-set-faces'."
  (let* ((p (valve-olive-themes-palette variant))
         (g (lambda (k) (cdr (assq k p))))
         (bg-dark     (funcall g 'bg-dark))
         (bg          (funcall g 'bg))
         (bg-light    (funcall g 'bg-light))
         (bg-sel      (funcall g 'bg-sel))
         (bg-hl       (funcall g 'bg-hl))
         (border      (funcall g 'border))
         (black       (funcall g 'black))
         (fg          (funcall g 'fg))
         (fg-dim      (funcall g 'fg-dim))
         (comment     (funcall g 'comment))
         (gutter      (funcall g 'gutter))
         (accent      (funcall g 'accent))
         (gold        (funcall g 'gold))
         (green       (funcall g 'green))
         (teal        (funcall g 'teal))
         (func        (funcall g 'func))
         (mauve       (funcall g 'mauve))
         (red         (funcall g 'red))
         (warn        (funcall g 'warn))
         (info        (funcall g 'info))
         (hint        (funcall g 'hint))
         (diff-add    (funcall g 'diff-add))
         (diff-change (funcall g 'diff-change))
         (diff-delete (funcall g 'diff-delete))
         (diff-text   (funcall g 'diff-text))
         ;; Popups/floats use the darker background (`dark_floats = true').
         (float-bg    bg-dark)
         ;; Resolved comment styling.
         (comment-fg  (valve-olive-themes-comment-color variant))
         (italic      (if valve-olive-italic-comments '(:slant italic) '()))
         (tk-comment  `(:foreground ,comment-fg ,@italic))
         (tk-doc      (if valve-olive-contrast-docstrings
                          tk-comment
                        `(:foreground ,comment ,@italic)))
         ;; Per-token specs (1:1 with highlights.lua's legacy syntax groups).
         (tk-string   `(:foreground ,green))
         (tk-const    `(:foreground ,gold))
         (tk-func     `(:foreground ,func))
         (tk-keyword  `(:foreground ,teal))
         (tk-type     `(:foreground ,hint))
         (tk-special  `(:foreground ,func))
         (tk-ident    `(:foreground ,fg))
         (tk-op       `(:foreground ,fg))
         (tk-punct    `(:foreground ,fg-dim)))
    (list
     ;;; -------------------------------------------------------------------
     ;;; Core editor / UI chrome
     ;;; -------------------------------------------------------------------
     ;; NOTE: `default', `fixed-pitch' and `variable-pitch' deliberately carry
     ;; no :family and no :height -- that is the user's font configuration to
     ;; own, not the theme's.
     `(default              ((t (:foreground ,fg :background ,bg))))
     `(cursor               ((t (:background ,accent))))
     `(fringe               ((t (:foreground ,gutter :background ,bg))))
     ;; Visual is an inverted near-white block in the Neovim theme
     ;; (highlights.lua:48).  It is the theme's signature; kept verbatim.
     `(region               ((t (:foreground ,bg :background ,fg-dim :extend t))))
     `(secondary-selection  ((t (:background ,bg-sel :extend t))))
     `(highlight            ((t (:background ,bg-light))))
     `(hl-line              ((t (:background ,bg-light :extend t))))
     `(cursor-line          ((t (:background ,bg-light :extend t))))
     `(lazy-highlight       ((t (:foreground ,black :background ,accent))))
     `(isearch              ((t (:foreground ,black :background ,gold :weight bold))))
     `(isearch-fail         ((t (:foreground ,black :background ,red :weight bold))))
     `(isearch-group-1      ((t (:foreground ,black :background ,teal))))
     `(isearch-group-2      ((t (:foreground ,black :background ,mauve))))
     `(match                ((t (:foreground ,black :background ,accent))))
     `(query-replace        ((t (:foreground ,black :background ,warn :weight bold))))
     `(show-paren-match     ((t (:foreground ,accent :weight bold))))
     `(show-paren-match-expression ((t (:background ,bg-light))))
     `(show-paren-mismatch  ((t (:foreground ,black :background ,red :weight bold))))
     `(trailing-whitespace  ((t (:background ,diff-delete))))
     `(escape-glyph         ((t (:foreground ,func))))
     `(homoglyph            ((t (:foreground ,func))))
     `(nobreak-space        ((t (:foreground ,bg-hl :underline t))))
     `(nobreak-hyphen       ((t (:foreground ,bg-hl))))
     `(minibuffer-prompt    ((t (:foreground ,teal :weight bold))))
     `(shadow               ((t (:foreground ,gutter))))
     `(error                ((t (:foreground ,red :weight bold))))
     `(warning              ((t (:foreground ,warn :weight bold))))
     `(success              ((t (:foreground ,green :weight bold))))
     `(tooltip              ((t (:foreground ,fg :background ,float-bg))))
     `(link                 ((t (:foreground ,teal :underline t))))
     `(link-visited         ((t (:foreground ,mauve :underline t))))
     `(button               ((t (:inherit link))))
     `(help-key-binding     ((t (:foreground ,gold :background ,bg-light))))
     `(header-line          ((t (:foreground ,fg :background ,bg-dark))))
     `(header-line-highlight ((t (:foreground ,black :background ,accent))))
     `(highlight-quoted-symbol ((t (:foreground ,green))))

     ;; Line numbers / fill column / dividers
     `(line-number              ((t (:inherit default :foreground ,gutter :background ,bg))))
     `(line-number-current-line ((t (:inherit default :foreground ,accent :background ,bg :weight bold))))
     `(line-number-major-tick   ((t (:inherit default :foreground ,fg-dim :background ,bg))))
     `(line-number-minor-tick   ((t (:inherit default :foreground ,gutter :background ,bg))))
     `(fill-column-indicator    ((t (:inherit default :foreground ,bg-hl))))
     `(vertical-border          ((t (:foreground ,border :background ,bg))))
     `(window-divider           ((t (:foreground ,border))))
     `(window-divider-first-pixel ((t (:foreground ,border))))
     `(window-divider-last-pixel  ((t (:foreground ,border))))
     `(separator-line           ((t (:foreground ,border))))
     `(border                   ((t (:background ,border))))
     `(internal-border          ((t (:background ,bg))))
     `(child-frame-border       ((t (:background ,border))))

     ;; whitespace-mode / NonText
     `(whitespace-space       ((t (:foreground ,bg-hl))))
     `(whitespace-tab         ((t (:foreground ,bg-hl))))
     `(whitespace-newline     ((t (:foreground ,bg-hl))))
     `(whitespace-hspace      ((t (:foreground ,bg-hl))))
     `(whitespace-indentation ((t (:foreground ,bg-hl))))
     `(whitespace-empty       ((t (:background ,diff-change))))
     `(whitespace-line        ((t (:background ,bg-light))))
     `(whitespace-trailing    ((t (:background ,diff-delete))))
     `(whitespace-big-indent  ((t (:background ,diff-delete))))
     `(whitespace-missing-newline-at-eof ((t (:background ,diff-change))))
     `(glyphless-char         ((t (:foreground ,gutter))))

     ;; Mode line (StatusLine / StatusLineNC).  There is no modeline package
     ;; here -- these built-in faces are the whole modeline.
     `(mode-line            ((t (:foreground ,fg :background ,bg-light
                                             :box (:line-width 1 :color ,bg-light :style released-button)))))
     `(mode-line-active     ((t (:inherit mode-line))))
     `(mode-line-inactive   ((t (:foreground ,fg-dim :background ,bg-dark
                                             :box (:line-width 1 :color ,bg-dark)))))
     `(mode-line-emphasis   ((t (:foreground ,gold :weight bold))))
     `(mode-line-highlight  ((t (:foreground ,black :background ,accent))))
     `(mode-line-buffer-id  ((t (:foreground ,fg :weight bold))))

     ;; Tab bar / tab line (TabLine, TabLineSel, TabLineFill)
     ;; `(tab-bar				((t (:foreground ,fg-dim :background ,bg-dark))))
     ;; `(tab-bar-tab			((t (:foreground ,gold :background ,bg :weight bold :box (:line-width 2 :color ,bg)))))
     ;; `(tab-bar-tab-inactive		((t (:foreground ,fg-dim :background ,bg-dark :box (:line-width 2 :color ,bg-dark)))))
     ;; `(tab-bar-tab-group-current	((t (:foreground ,gold :weight bold))))
     ;; `(tab-bar-tab-group-inactive	((t (:foreground ,fg-dim))))
     ;; `(tab-bar-tab-ungrouped		((t (:foreground ,gutter))))
     ;; `(tab-line				((t (:foreground ,fg-dim :background ,bg-dark))))
     ;; `(tab-line-tab			((t (:foreground ,gold :background ,bg))))
     ;; `(tab-line-tab-current		((t (:foreground ,gold :background ,bg :weight bold))))
     ;; `(tab-line-tab-inactive		((t (:foreground ,fg-dim :background ,bg-dark))))
     ;; `(tab-line-highlight		((t (:foreground ,black :background ,accent))))

     ;; Misc UI / widgets / customize
     `(widget-field        ((t (:foreground ,fg :background ,bg-light
                                            :box (:line-width 1 :color ,border)))))
     `(widget-button       ((t (:inherit button))))
     `(widget-inactive     ((t (:inherit shadow))))
     `(widget-documentation ((t (:foreground ,comment))))
     `(custom-button       ((t (:foreground ,fg :background ,bg-light
                                            :box (:line-width 1 :color ,border)))))
     `(custom-button-mouse ((t (:foreground ,black :background ,accent
                                            :box (:line-width 1 :color ,border)))))
     `(custom-button-pressed ((t (:foreground ,black :background ,gold
                                              :box (:line-width 1 :color ,gold)))))
     `(custom-group-tag    ((t (:foreground ,gold :weight bold :height 1.2))))
     `(custom-group-tag-1  ((t (:foreground ,teal :weight bold :height 1.2))))
     `(custom-variable-tag ((t (:foreground ,hint :weight bold))))
     `(custom-variable-obsolete ((t (:foreground ,gutter :strike-through t))))
     `(custom-state        ((t (:foreground ,green))))
     `(custom-changed      ((t (:foreground ,warn))))
     `(custom-modified     ((t (:foreground ,warn))))
     `(custom-set          ((t (:foreground ,green))))
     `(custom-themed       ((t (:foreground ,teal))))
     `(custom-invalid      ((t (:foreground ,red :weight bold))))
     `(custom-comment      ((t (:foreground ,comment :background ,bg-light))))
     `(custom-comment-tag  ((t (:foreground ,fg-dim))))
     `(next-error          ((t (:background ,bg-sel :extend t))))
     `(next-error-message  ((t (:background ,bg-light :extend t))))
     `(pulse-highlight-start-face ((t (:background ,bg-sel))))

     ;; Messages / echo area (ErrorMsg, WarningMsg, ModeMsg, MoreMsg, Question)
     `(message-header-name    ((t (:foreground ,teal))))
     `(message-header-subject ((t (:foreground ,fg :weight bold))))
     `(message-header-to      ((t (:foreground ,fg :weight bold))))
     `(message-header-other   ((t (:foreground ,fg-dim))))
     `(message-cited-text-1   ((t (:foreground ,fg-dim :slant italic))))
     `(message-separator      ((t (:foreground ,border))))

     ;;; -------------------------------------------------------------------
     ;;; font-lock -- the syntax layer.
     ;;; Emacs 30+ tree-sitter modes reuse these faces, so they are covered.
     ;;; -------------------------------------------------------------------
     `(font-lock-comment-face           ((t ,tk-comment)))
     `(font-lock-comment-delimiter-face ((t ,tk-comment)))
     `(font-lock-doc-face               ((t ,tk-doc)))
     `(font-lock-doc-markup-face        ((t (:foreground ,fg-dim ,@italic))))
     `(font-lock-string-face            ((t ,tk-string)))
     `(font-lock-keyword-face           ((t ,tk-keyword)))
     `(font-lock-builtin-face           ((t ,tk-keyword)))
     `(font-lock-function-name-face     ((t ,tk-func)))
     `(font-lock-function-call-face     ((t ,tk-func)))
     `(font-lock-variable-name-face     ((t ,tk-ident)))
     `(font-lock-variable-use-face      ((t ,tk-ident)))
     `(font-lock-type-face              ((t ,tk-type)))
     `(font-lock-constant-face          ((t ,tk-const)))
     `(font-lock-number-face            ((t ,tk-const)))
     `(font-lock-property-name-face     ((t ,tk-ident)))
     `(font-lock-property-use-face      ((t ,tk-ident)))
     `(font-lock-preprocessor-face      ((t ,tk-keyword)))
     `(font-lock-macro-name-face        ((t ,tk-keyword)))
     `(font-lock-operator-face          ((t ,tk-op)))
     `(font-lock-delimiter-face         ((t ,tk-punct)))
     `(font-lock-punctuation-face       ((t ,tk-punct)))
     `(font-lock-bracket-face           ((t ,tk-punct)))
     `(font-lock-misc-punctuation-face  ((t ,tk-punct)))
     `(font-lock-escape-face            ((t ,tk-special)))
     `(font-lock-negation-char-face     ((t ,tk-op)))
     `(font-lock-regexp-face            ((t ,tk-special)))
     `(font-lock-regexp-grouping-backslash ((t ,tk-special)))
     `(font-lock-regexp-grouping-construct ((t ,tk-special)))
     `(font-lock-label-face             ((t ,tk-keyword)))
     `(font-lock-warning-face           ((t (:foreground ,red :weight bold))))
     `(elisp-shorthand-font-lock-face   ((t (:foreground ,mauve))))

     ;;; -------------------------------------------------------------------
     ;;; Diagnostics / LSP
     ;;; -------------------------------------------------------------------
     `(flymake-error        ((t (:underline (:style wave :color ,red)))))
     `(flymake-warning      ((t (:underline (:style wave :color ,warn)))))
     `(flymake-note         ((t (:underline (:style wave :color ,info)))))
     `(flymake-error-echo   ((t (:foreground ,red))))
     `(flymake-warning-echo ((t (:foreground ,warn))))
     `(flymake-note-echo    ((t (:foreground ,info))))
     `(flymake-end-of-line-diagnostics-face ((t (:inherit shadow :height 0.85))))
     `(flymake-error-echo-at-eol   ((t (:foreground ,red :height 0.85))))
     `(flymake-warning-echo-at-eol ((t (:foreground ,warn :height 0.85))))
     `(flymake-note-echo-at-eol    ((t (:foreground ,info :height 0.85))))
     `(flymake-error-fringe   ((t (:foreground ,red))))
     `(flymake-warning-fringe ((t (:foreground ,warn))))
     `(flymake-note-fringe    ((t (:foreground ,info))))
     `(flymake-eol-information-face ((t (:foreground ,fg-dim :height 0.85))))
     `(flycheck-error       ((t (:underline (:style wave :color ,red)))))
     `(flycheck-warning     ((t (:underline (:style wave :color ,warn)))))
     `(flycheck-info        ((t (:underline (:style wave :color ,info)))))
     `(flycheck-fringe-error   ((t (:foreground ,red))))
     `(flycheck-fringe-warning ((t (:foreground ,warn))))
     `(flycheck-fringe-info    ((t (:foreground ,info))))
     `(compilation-error    ((t (:foreground ,red :weight bold))))
     `(compilation-warning  ((t (:foreground ,warn :weight bold))))
     `(compilation-info     ((t (:foreground ,info))))
     `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))
     `(compilation-mode-line-run  ((t (:foreground ,warn))))
     `(compilation-mode-line-exit ((t (:foreground ,green :weight bold))))
     `(compilation-line-number    ((t (:foreground ,gutter))))
     `(compilation-column-number  ((t (:foreground ,gutter))))

     ;; Spelling (SpellBad / SpellCap / SpellRare / SpellLocal)
     `(flyspell-incorrect   ((t (:underline (:style wave :color ,red)))))
     `(flyspell-duplicate   ((t (:underline (:style wave :color ,warn)))))
     `(jinx-misspelled      ((t (:underline (:style wave :color ,red)))))
     `(jinx-highlight       ((t (:foreground ,black :background ,accent))))
     `(jinx-accept          ((t (:inherit shadow))))

     ;; eglot / lsp-mode
     `(eglot-highlight-symbol-face ((t (:background ,bg-sel))))
     `(eglot-mode-line             ((t (:foreground ,gold :weight bold))))
     `(eglot-inlay-hint-face       ((t (:foreground ,gutter :background ,bg-light :height 0.9))))
     `(eglot-parameter-hint-face   ((t (:inherit eglot-inlay-hint-face))))
     `(eglot-type-hint-face        ((t (:inherit eglot-inlay-hint-face))))
     `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,fg-dim))))
     `(eglot-diagnostic-tag-deprecated-face  ((t (:foreground ,fg-dim :strike-through t))))
     `(lsp-face-highlight-textual  ((t (:background ,bg-sel))))
     `(lsp-face-highlight-read     ((t (:background ,bg-sel))))
     `(lsp-face-highlight-write    ((t (:background ,bg-sel :underline t))))
     `(lsp-inlay-hint-face         ((t (:foreground ,gutter :background ,bg-light :height 0.9))))
     `(lsp-signature-active-parameter ((t (:foreground ,gold :weight bold))))

     ;; xref
     `(xref-file-header     ((t (:foreground ,teal :weight bold))))
     `(xref-line-number     ((t (:foreground ,gutter))))
     `(xref-match           ((t (:foreground ,black :background ,accent))))

     ;;; -------------------------------------------------------------------
     ;;; Completion (Pmenu / PmenuSel and friends)
     ;;; -------------------------------------------------------------------
     ;; Built-in *Completions* -- the primary completion UI in this config.
     `(completions-common-part      ((t (:foreground ,accent :weight bold))))
     `(completions-first-difference ((t (:foreground ,gold :weight bold))))
     `(completions-annotations      ((t (:foreground ,fg-dim))))
     `(completions-group-title      ((t (:foreground ,teal ,@italic))))
     `(completions-group-separator  ((t (:foreground ,border :strike-through t))))
     `(completions-highlight        ((t (:foreground ,accent :background ,bg-sel :weight bold))))
     `(file-name-shadow             ((t (:inherit shadow))))

     ;; vertico
     `(vertico-current         ((t (:foreground ,fg :background ,bg-sel :weight bold :extend t))))
     `(vertico-group-title     ((t (:foreground ,teal :weight bold ,@italic))))
     `(vertico-group-separator ((t (:foreground ,border :strike-through t))))
     `(vertico-multiline       ((t (:foreground ,gutter))))
     `(vertico-mouse           ((t (:background ,bg-sel))))
     `(vertico-quick1          ((t (:foreground ,black :background ,accent :weight bold))))
     `(vertico-quick2          ((t (:foreground ,black :background ,gold :weight bold))))
     `(vertico-indexed         ((t (:foreground ,gutter))))

     ;; marginalia
     `(marginalia-key           ((t (:foreground ,gold))))
     `(marginalia-documentation ((t (:inherit font-lock-comment-face))))
     `(marginalia-file-name     ((t (:foreground ,fg-dim))))
     `(marginalia-file-priv-dir   ((t (:foreground ,teal))))
     `(marginalia-file-priv-read  ((t (:foreground ,green))))
     `(marginalia-file-priv-write ((t (:foreground ,warn))))
     `(marginalia-file-priv-exec  ((t (:foreground ,red))))
     `(marginalia-file-priv-link  ((t (:foreground ,mauve))))
     `(marginalia-file-priv-no    ((t (:foreground ,gutter))))
     `(marginalia-file-priv-other ((t (:foreground ,mauve))))
     `(marginalia-file-priv-rare  ((t (:foreground ,mauve))))
     `(marginalia-file-owner      ((t (:foreground ,fg-dim))))
     `(marginalia-number        ((t (:foreground ,gold))))
     `(marginalia-size          ((t (:foreground ,fg-dim))))
     `(marginalia-date          ((t (:foreground ,fg-dim))))
     `(marginalia-mode          ((t (:foreground ,hint))))
     `(marginalia-modified      ((t (:foreground ,warn))))
     `(marginalia-type          ((t (:foreground ,hint))))
     `(marginalia-char          ((t (:foreground ,mauve))))
     `(marginalia-symbol        ((t (:foreground ,teal))))
     `(marginalia-value         ((t (:foreground ,fg))))
     `(marginalia-function      ((t (:foreground ,func))))
     `(marginalia-null          ((t (:foreground ,gutter))))
     `(marginalia-true          ((t (:foreground ,green))))
     `(marginalia-version       ((t (:foreground ,green))))
     `(marginalia-list          ((t (:foreground ,fg-dim))))
     `(marginalia-lighter       ((t (:foreground ,mauve))))
     `(marginalia-string        ((t (:foreground ,green))))
     `(marginalia-installed     ((t (:foreground ,green))))
     `(marginalia-archive       ((t (:foreground ,warn))))
     `(marginalia-on            ((t (:foreground ,green))))
     `(marginalia-off           ((t (:foreground ,red))))

     ;; corfu
     `(corfu-default     ((t (:foreground ,fg :background ,float-bg))))
     `(corfu-current     ((t (:foreground ,accent :background ,bg-sel :weight bold))))
     `(corfu-bar         ((t (:background ,border))))
     `(corfu-border      ((t (:background ,border))))
     `(corfu-popupinfo   ((t (:inherit corfu-default))))
     `(corfu-deprecated  ((t (:foreground ,fg-dim :strike-through t))))
     `(corfu-annotations ((t (:foreground ,fg-dim))))
     `(corfu-echo        ((t (:foreground ,fg-dim))))

     ;; company
     `(company-tooltip            ((t (:foreground ,fg :background ,float-bg))))
     `(company-tooltip-selection  ((t (:foreground ,accent :background ,bg-sel :weight bold))))
     `(company-tooltip-common     ((t (:foreground ,accent :weight bold))))
     `(company-tooltip-annotation ((t (:foreground ,teal))))
     `(company-scrollbar-bg       ((t (:background ,bg-light))))
     `(company-scrollbar-fg       ((t (:background ,border))))
     `(company-preview            ((t (:foreground ,fg-dim :background ,bg-light))))
     `(company-preview-common     ((t (:foreground ,accent :weight bold))))

     ;; orderless
     `(orderless-match-face-0 ((t (:foreground ,accent :weight bold))))
     `(orderless-match-face-1 ((t (:foreground ,gold :weight bold))))
     `(orderless-match-face-2 ((t (:foreground ,teal :weight bold))))
     `(orderless-match-face-3 ((t (:foreground ,mauve :weight bold))))

     ;; consult
     `(consult-line-number         ((t (:foreground ,gutter))))
     `(consult-line-number-prefix  ((t (:foreground ,gutter))))
     `(consult-line-number-wrapped ((t (:foreground ,warn))))
     `(consult-preview-line        ((t (:background ,bg-light :extend t))))
     `(consult-preview-match       ((t (:foreground ,black :background ,accent))))
     `(consult-preview-cursor      ((t (:foreground ,black :background ,gold))))
     `(consult-async-split         ((t (:foreground ,red))))
     `(consult-async-running       ((t (:foreground ,warn))))
     `(consult-async-finished      ((t (:foreground ,green))))
     `(consult-async-failed        ((t (:foreground ,red))))
     `(consult-key                 ((t (:foreground ,gold))))
     `(consult-imenu-prefix        ((t (:foreground ,fg-dim))))
     `(consult-bookmark            ((t (:foreground ,teal))))
     `(consult-file                ((t (:foreground ,fg))))
     `(consult-separator           ((t (:foreground ,border))))
     `(consult-help                ((t (:foreground ,fg-dim))))

     ;; embark
     `(embark-keybinding   ((t (:foreground ,gold))))
     `(embark-target       ((t (:background ,bg-sel))))
     `(embark-collect-group-title ((t (:foreground ,teal :weight bold ,@italic))))
     `(embark-collect-marked      ((t (:foreground ,mauve))))
     `(embark-verbose-indicator-documentation ((t (:inherit font-lock-comment-face))))

     ;; which-key (built-in since Emacs 30)
     `(which-key-key-face                 ((t (:foreground ,gold :weight bold))))
     `(which-key-group-description-face   ((t (:foreground ,teal))))
     `(which-key-command-description-face ((t (:foreground ,fg))))
     `(which-key-separator-face           ((t (:foreground ,gutter))))
     `(which-key-note-face                ((t (:foreground ,gutter))))
     `(which-key-local-map-description-face ((t (:foreground ,hint))))
     `(which-key-highlighted-command-face ((t (:foreground ,accent :underline t))))
     `(which-key-special-key-face         ((t (:foreground ,black :background ,accent :weight bold))))
     `(which-key-docstring-face           ((t (:inherit font-lock-comment-face))))

     ;;; -------------------------------------------------------------------
     ;;; Git (magit / diff-mode / diff-hl / git-commit / smerge / ediff)
     ;;; -------------------------------------------------------------------
     `(magit-section-heading        ((t (:foreground ,gold :weight bold))))
     `(magit-section-heading-selection ((t (:foreground ,accent :weight bold))))
     `(magit-section-highlight      ((t (:background ,bg-light :extend t))))
     `(magit-section-secondary-heading ((t (:foreground ,teal :weight bold))))
     `(magit-header-line            ((t (:foreground ,fg :background ,bg-dark :weight bold))))
     `(magit-diff-added             ((t (:foreground ,green :background ,diff-add :extend t))))
     `(magit-diff-added-highlight   ((t (:foreground ,green :background ,diff-add :weight bold :extend t))))
     `(magit-diff-removed           ((t (:foreground ,red :background ,diff-delete :extend t))))
     `(magit-diff-removed-highlight ((t (:foreground ,red :background ,diff-delete :weight bold :extend t))))
     `(magit-diff-context           ((t (:foreground ,fg-dim :extend t))))
     `(magit-diff-context-highlight ((t (:foreground ,fg-dim :background ,bg-light :extend t))))
     `(magit-diff-hunk-heading      ((t (:foreground ,fg-dim :background ,bg-light :extend t))))
     `(magit-diff-hunk-heading-highlight ((t (:foreground ,fg :background ,bg-sel :weight bold :extend t))))
     `(magit-diff-hunk-heading-selection ((t (:foreground ,accent :background ,bg-sel :weight bold :extend t))))
     `(magit-diff-lines-heading     ((t (:foreground ,black :background ,gold :extend t))))
     `(magit-diff-file-heading      ((t (:foreground ,teal :weight bold))))
     `(magit-diff-file-heading-highlight ((t (:foreground ,teal :background ,bg-light :weight bold))))
     `(magit-diff-file-heading-selection ((t (:foreground ,accent :background ,bg-light :weight bold))))
     `(magit-diffstat-added         ((t (:foreground ,green))))
     `(magit-diffstat-removed       ((t (:foreground ,red))))
     `(magit-branch-local           ((t (:foreground ,teal :weight bold))))
     `(magit-branch-remote          ((t (:foreground ,green :weight bold))))
     `(magit-branch-current         ((t (:foreground ,accent :weight bold :box (:line-width 1 :color ,accent)))))
     `(magit-head                   ((t (:foreground ,accent :weight bold))))
     `(magit-tag                    ((t (:foreground ,gold))))
     `(magit-hash                   ((t (:foreground ,gutter))))
     `(magit-log-author             ((t (:foreground ,func))))
     `(magit-log-date               ((t (:foreground ,fg-dim))))
     `(magit-log-graph              ((t (:foreground ,gutter))))
     `(magit-dimmed                 ((t (:foreground ,gutter))))
     `(magit-filename               ((t (:foreground ,fg))))
     `(magit-process-ok             ((t (:foreground ,green :weight bold))))
     `(magit-process-ng             ((t (:foreground ,red :weight bold))))
     `(magit-bisect-good            ((t (:foreground ,green))))
     `(magit-bisect-bad             ((t (:foreground ,red))))
     `(magit-bisect-skip            ((t (:foreground ,warn))))
     `(magit-cherry-equivalent      ((t (:foreground ,mauve))))
     `(magit-cherry-unmatched       ((t (:foreground ,teal))))
     `(magit-signature-good         ((t (:foreground ,green))))
     `(magit-signature-bad          ((t (:foreground ,red :weight bold))))
     `(magit-signature-untrusted    ((t (:foreground ,warn))))
     `(magit-signature-expired      ((t (:foreground ,warn))))
     `(magit-signature-revoked      ((t (:foreground ,mauve))))
     `(magit-reflog-commit          ((t (:foreground ,green))))
     `(magit-reflog-amend           ((t (:foreground ,mauve))))
     `(magit-reflog-merge           ((t (:foreground ,green))))
     `(magit-reflog-checkout        ((t (:foreground ,teal))))
     `(magit-reflog-reset           ((t (:foreground ,red))))
     `(magit-reflog-rebase          ((t (:foreground ,mauve))))
     `(magit-reflog-cherry-pick     ((t (:foreground ,green))))
     `(magit-reflog-remote          ((t (:foreground ,teal))))
     `(magit-reflog-other           ((t (:foreground ,fg-dim))))
     `(magit-blame-heading          ((t (:foreground ,fg :background ,bg-light :weight bold))))
     `(magit-blame-date             ((t (:foreground ,teal :background ,bg-light))))
     `(magit-blame-name             ((t (:foreground ,func :background ,bg-light))))
     `(magit-blame-hash             ((t (:foreground ,gutter :background ,bg-light))))
     `(magit-blame-summary          ((t (:foreground ,fg :background ,bg-light))))

     ;; diff-mode
     `(diff-added          ((t (:foreground ,green :background ,diff-add :extend t))))
     `(diff-removed        ((t (:foreground ,red :background ,diff-delete :extend t))))
     `(diff-changed        ((t (:foreground ,warn :background ,diff-change :extend t))))
     `(diff-changed-unspecified ((t (:foreground ,warn :background ,diff-change :extend t))))
     `(diff-indicator-added   ((t (:foreground ,green :background ,diff-add))))
     `(diff-indicator-removed ((t (:foreground ,red :background ,diff-delete))))
     `(diff-indicator-changed ((t (:foreground ,warn :background ,diff-change))))
     `(diff-refine-added   ((t (:foreground ,green :background ,diff-text :weight bold))))
     `(diff-refine-removed ((t (:foreground ,red :background ,diff-text :weight bold))))
     `(diff-refine-changed ((t (:foreground ,warn :background ,diff-text :weight bold))))
     `(diff-header         ((t (:foreground ,fg-dim :background ,bg-light :extend t))))
     `(diff-file-header    ((t (:foreground ,teal :weight bold :extend t))))
     `(diff-hunk-header    ((t (:foreground ,fg-dim :background ,bg-light :extend t))))
     `(diff-index          ((t (:foreground ,gold))))
     `(diff-function       ((t (:foreground ,gutter))))
     `(diff-context        ((t (:foreground ,fg-dim))))
     `(diff-nonexistent    ((t (:foreground ,red :weight bold))))
     `(diff-error          ((t (:foreground ,black :background ,red :weight bold))))

     ;; diff-hl (fringe / margin)
     `(diff-hl-insert       ((t (:foreground ,green :background ,green))))
     `(diff-hl-change       ((t (:foreground ,warn :background ,warn))))
     `(diff-hl-delete       ((t (:foreground ,red :background ,red))))
     `(diff-hl-reverted-hunk-highlight ((t (:foreground ,black :background ,red))))
     `(diff-hl-dired-insert ((t (:foreground ,green))))
     `(diff-hl-dired-change ((t (:foreground ,warn))))
     `(diff-hl-dired-delete ((t (:foreground ,red))))
     `(git-gutter:added     ((t (:foreground ,green))))
     `(git-gutter:modified  ((t (:foreground ,warn))))
     `(git-gutter:deleted   ((t (:foreground ,red))))

     ;; git-commit / log-edit / vc
     `(git-commit-summary              ((t (:foreground ,fg :weight bold))))
     `(git-commit-overflow             ((t (:inherit error))))
     `(git-commit-nonempty-second-line ((t (:foreground ,warn))))
     `(git-commit-comment-heading      ((t (:foreground ,teal :weight bold))))
     `(git-commit-comment-file         ((t (:foreground ,func))))
     `(git-commit-comment-branch-local  ((t (:foreground ,teal :weight bold))))
     `(git-commit-comment-branch-remote ((t (:foreground ,green :weight bold))))
     `(git-commit-comment-action       ((t (:foreground ,fg-dim))))
     `(log-edit-summary                ((t (:foreground ,fg :weight bold))))
     `(log-edit-header                 ((t (:foreground ,teal :weight bold))))
     `(log-view-message                ((t (:foreground ,gutter))))
     `(log-view-commit-body            ((t (:foreground ,fg))))
     `(vc-up-to-date-state             ((t (:foreground ,green))))
     `(vc-edited-state                 ((t (:foreground ,warn))))
     `(vc-missing-state                ((t (:foreground ,red))))
     `(vc-conflict-state               ((t (:foreground ,red :weight bold))))
     `(vc-locally-added-state          ((t (:foreground ,green))))
     `(vc-removed-state                ((t (:foreground ,red))))

     ;; smerge / ediff
     `(smerge-upper           ((t (:background ,diff-delete :extend t))))
     `(smerge-lower           ((t (:background ,diff-add :extend t))))
     `(smerge-base            ((t (:background ,diff-change :extend t))))
     `(smerge-markers         ((t (:foreground ,fg-dim :background ,bg-light :weight bold :extend t))))
     `(smerge-refined-added   ((t (:foreground ,green :background ,diff-text))))
     `(smerge-refined-removed ((t (:foreground ,red :background ,diff-text))))
     `(ediff-current-diff-A        ((t (:background ,diff-delete :extend t))))
     `(ediff-current-diff-B        ((t (:background ,diff-add :extend t))))
     `(ediff-current-diff-C        ((t (:background ,diff-change :extend t))))
     `(ediff-current-diff-Ancestor ((t (:background ,bg-light :extend t))))
     `(ediff-fine-diff-A       ((t (:foreground ,red :background ,diff-text :weight bold))))
     `(ediff-fine-diff-B       ((t (:foreground ,green :background ,diff-text :weight bold))))
     `(ediff-fine-diff-C       ((t (:foreground ,warn :background ,diff-text :weight bold))))
     `(ediff-fine-diff-Ancestor ((t (:foreground ,fg :background ,diff-text :weight bold))))
     `(ediff-even-diff-A       ((t (:background ,bg-light :extend t))))
     `(ediff-even-diff-B       ((t (:background ,bg-light :extend t))))
     `(ediff-even-diff-C       ((t (:background ,bg-light :extend t))))
     `(ediff-even-diff-Ancestor ((t (:background ,bg-light :extend t))))
     `(ediff-odd-diff-A        ((t (:background ,bg-dark :extend t))))
     `(ediff-odd-diff-B        ((t (:background ,bg-dark :extend t))))
     `(ediff-odd-diff-C        ((t (:background ,bg-dark :extend t))))
     `(ediff-odd-diff-Ancestor ((t (:background ,bg-dark :extend t))))

     ;;; -------------------------------------------------------------------
     ;;; File trees, tabs, buffer lists, side panels
     ;;; -------------------------------------------------------------------
     ;; dired (Directory = teal)
     `(dired-directory   ((t (:foreground ,teal :weight bold))))
     `(dired-header      ((t (:foreground ,gold :weight bold))))
     `(dired-symlink     ((t (:foreground ,mauve))))
     `(dired-broken-symlink ((t (:foreground ,black :background ,red))))
     `(dired-mark        ((t (:foreground ,accent :weight bold))))
     `(dired-marked      ((t (:foreground ,accent :weight bold))))
     `(dired-flagged     ((t (:foreground ,red :weight bold))))
     `(dired-perm-write  ((t (:foreground ,warn))))
     `(dired-set-id      ((t (:foreground ,red))))
     `(dired-special     ((t (:foreground ,mauve))))
     `(dired-ignored     ((t (:foreground ,gutter))))
     `(dired-warning     ((t (:foreground ,warn :weight bold))))

     ;; treemacs
     `(treemacs-root-face            ((t (:foreground ,gold :weight bold :height 1.1))))
     `(treemacs-root-unreadable-face ((t (:foreground ,red :weight bold))))
     `(treemacs-root-remote-face     ((t (:foreground ,teal :weight bold))))
     `(treemacs-directory-face       ((t (:foreground ,teal))))
     `(treemacs-directory-collapsed-face ((t (:foreground ,teal))))
     `(treemacs-file-face            ((t (:foreground ,fg))))
     `(treemacs-tags-face            ((t (:foreground ,fg))))
     `(treemacs-term-node-face       ((t (:foreground ,teal :weight bold))))
     `(treemacs-fringe-indicator-face ((t (:foreground ,accent))))
     `(treemacs-on-success-pulse-face ((t (:foreground ,black :background ,green))))
     `(treemacs-on-failure-pulse-face ((t (:foreground ,black :background ,red))))
     `(treemacs-git-added-face       ((t (:foreground ,green))))
     `(treemacs-git-modified-face    ((t (:foreground ,warn))))
     `(treemacs-git-renamed-face     ((t (:foreground ,teal))))
     `(treemacs-git-conflict-face    ((t (:foreground ,red :weight bold))))
     `(treemacs-git-ignored-face     ((t (:foreground ,gutter))))
     `(treemacs-git-untracked-face   ((t (:foreground ,fg-dim))))
     `(treemacs-git-unmodified-face  ((t (:foreground ,fg))))
     `(treemacs-git-commit-diff-face ((t (:foreground ,warn))))
     `(treemacs-window-background-face ((t (:background ,bg-dark))))
     `(treemacs-hl-line-face         ((t (:background ,bg-sel :extend t))))
     `(treemacs-peek-mode-indicator-face ((t (:foreground ,black :background ,accent))))

     ;; centaur-tabs (bufferline)
     `(centaur-tabs-default            ((t (:foreground ,fg-dim :background ,bg-dark))))
     `(centaur-tabs-unselected         ((t (:foreground ,fg-dim :background ,bg-dark))))
     `(centaur-tabs-selected           ((t (:foreground ,gold :background ,bg :weight bold))))
     `(centaur-tabs-unselected-modified ((t (:foreground ,warn :background ,bg-dark))))
     `(centaur-tabs-selected-modified  ((t (:foreground ,warn :background ,bg :weight bold))))
     `(centaur-tabs-active-bar-face    ((t (:background ,accent))))
     `(centaur-tabs-modified-marker-selected   ((t (:foreground ,warn :background ,bg))))
     `(centaur-tabs-modified-marker-unselected ((t (:foreground ,warn :background ,bg-dark))))
     `(centaur-tabs-close-selected     ((t (:foreground ,red :background ,bg))))
     `(centaur-tabs-close-unselected   ((t (:foreground ,fg-dim :background ,bg-dark))))
     `(centaur-tabs-close-mouse-face   ((t (:foreground ,red))))
     `(centaur-tabs-name-mouse-face    ((t (:foreground ,accent :weight bold))))

     ;; ibuffer / bufler / speedbar
     `(ibuffer-locked-buffer ((t (:foreground ,red))))
     `(bufler-group          ((t (:foreground ,gold :weight bold))))
     `(bufler-path           ((t (:foreground ,fg-dim))))
     `(bufler-dim            ((t (:foreground ,gutter))))
     `(bufler-buffer         ((t (:foreground ,fg))))
     `(bufler-buffer-special ((t (:foreground ,teal))))
     `(bufler-mode           ((t (:foreground ,hint))))
     `(bufler-size           ((t (:foreground ,fg-dim))))
     `(bufler-vc             ((t (:foreground ,green))))
     `(speedbar-directory-face ((t (:inherit 'variable-pitch :foreground ,teal :weight bold))))
     `(speedbar-file-face      ((t (:inherit 'variable-pitch :foreground ,fg))))
     `(speedbar-selected-face  ((t (:inherit 'variable-pitch :foreground ,accent :underline t))))
     `(speedbar-highlight-face ((t (:inherit 'variable-pitch :background ,bg-sel))))
     `(speedbar-button-face    ((t (:inherit 'variable-pitch :foreground ,gutter))))
     `(speedbar-tag-face       ((t (:inherit 'variable-pitch :foreground ,green))))
     `(speedbar-separator-face ((t (:inherit 'variable-pitch :foreground ,black :background ,accent))))

     ;; solaire-mode (side / "non-real" buffers get the darker background)
     `(solaire-default-face     ((t (:foreground ,fg :background ,bg-dark))))
     `(solaire-fringe-face      ((t (:background ,bg-dark))))
     `(solaire-line-number-face ((t (:foreground ,gutter :background ,bg-dark))))
     `(solaire-hl-line-face     ((t (:background ,bg-light :extend t))))
     `(solaire-region-face      ((t (:inherit region))))
     `(solaire-mode-line-face          ((t (:inherit mode-line))))
     `(solaire-mode-line-inactive-face ((t (:inherit mode-line-inactive))))
     `(solaire-header-line-face ((t (:inherit header-line))))

     ;;; -------------------------------------------------------------------
     ;;; doom-modeline (mirrors the lualine extension's mode accents)
     ;;; -------------------------------------------------------------------
     `(doom-modeline-bar               ((t (:background ,gold))))
     `(doom-modeline-bar-inactive      ((t (:background ,bg-dark))))
     `(doom-modeline-buffer-file       ((t (:foreground ,fg :weight bold))))
     `(doom-modeline-buffer-modified   ((t (:foreground ,warn :weight bold))))
     `(doom-modeline-buffer-path       ((t (:foreground ,fg-dim))))
     `(doom-modeline-project-dir       ((t (:foreground ,teal :weight bold))))
     `(doom-modeline-project-root-dir  ((t (:foreground ,fg-dim))))
     `(doom-modeline-buffer-major-mode ((t (:foreground ,gold :weight bold))))
     `(doom-modeline-buffer-minor-mode ((t (:foreground ,fg-dim))))
     `(doom-modeline-info              ((t (:foreground ,green))))
     `(doom-modeline-warning           ((t (:foreground ,warn))))
     `(doom-modeline-urgent            ((t (:foreground ,red :weight bold))))
     `(doom-modeline-debug             ((t (:foreground ,fg-dim))))
     `(doom-modeline-highlight         ((t (:foreground ,black :background ,accent))))
     `(doom-modeline-panel             ((t (:foreground ,black :background ,gold :weight bold))))
     `(doom-modeline-time              ((t (:foreground ,fg-dim))))
     `(doom-modeline-host              ((t (:foreground ,fg-dim ,@italic))))
     `(doom-modeline-input-method      ((t (:foreground ,mauve :weight bold))))
     `(doom-modeline-spc-face          ((t (:foreground ,fg-dim))))
     `(doom-modeline-lsp-success       ((t (:foreground ,green))))
     `(doom-modeline-lsp-warning       ((t (:foreground ,warn))))
     `(doom-modeline-lsp-error         ((t (:foreground ,red))))
     `(doom-modeline-lsp-running       ((t (:foreground ,warn))))
     `(doom-modeline-notification      ((t (:foreground ,red))))
     ;; Mode accents lifted from lua/lualine/themes/valve-olive.lua.
     `(doom-modeline-evil-normal-state   ((t (:foreground ,gold :weight bold))))
     `(doom-modeline-evil-insert-state   ((t (:foreground ,green :weight bold))))
     `(doom-modeline-evil-visual-state   ((t (:foreground ,accent :weight bold))))
     `(doom-modeline-evil-replace-state  ((t (:foreground ,red :weight bold))))
     `(doom-modeline-evil-operator-state ((t (:foreground ,teal :weight bold))))
     `(doom-modeline-evil-motion-state   ((t (:foreground ,teal :weight bold))))
     `(doom-modeline-evil-emacs-state    ((t (:foreground ,func :weight bold))))

     ;;; -------------------------------------------------------------------
     ;;; Editing aids, navigation, languages
     ;;; -------------------------------------------------------------------
     ;; rainbow-delimiters -- order taken from highlights.lua:652-658
     ;; (Red, Yellow/gold, Blue/teal, Orange/func, Green, Violet/mauve, Cyan/info).
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,red))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,gold))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,teal))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,func))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,green))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,mauve))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,info))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,hint))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg-dim))))
     `(rainbow-delimiters-base-face       ((t (:foreground ,fg-dim))))
     `(rainbow-delimiters-base-error-face ((t (:foreground ,black :background ,red :weight bold))))
     `(rainbow-delimiters-unmatched-face  ((t (:foreground ,black :background ,red :weight bold))))
     `(rainbow-delimiters-mismatched-face ((t (:foreground ,black :background ,red :weight bold))))

     ;; avy (flash / leap / hop -- the badge idiom)
     `(avy-lead-face        ((t (:foreground ,black :background ,accent :weight bold))))
     `(avy-lead-face-0      ((t (:foreground ,black :background ,gold :weight bold))))
     `(avy-lead-face-1      ((t (:foreground ,black :background ,teal :weight bold))))
     `(avy-lead-face-2      ((t (:foreground ,black :background ,mauve :weight bold))))
     `(avy-background-face  ((t (:foreground ,gutter))))
     `(avy-goto-char-timer-face ((t (:foreground ,black :background ,warn :weight bold))))

     ;; hl-todo / TODO keywords (the `Todo' badge)
     `(hl-todo ((t (:foreground ,bg :background ,gold :weight bold))))

     ;; symbol/word highlight (vim-illuminate, symbol-overlay)
     `(symbol-overlay-default-face ((t (:background ,bg-sel))))
     `(highlight-symbol-face       ((t (:background ,bg-sel))))

     ;; indent guides (indent-blankline / mini.indentscope)
     `(highlight-indent-guides-character-face ((t (:foreground ,bg-hl))))
     `(highlight-indent-guides-even-face      ((t (:background ,bg-light))))
     `(highlight-indent-guides-odd-face       ((t (:background ,bg-light))))
     `(highlight-indent-guides-top-character-face ((t (:foreground ,gutter))))
     `(indent-bars-face ((t (:foreground ,bg-hl))))

     ;; outline -- also drives markdown/org headings below.
     ;; Heading hues follow @markup.heading.1-6: gold, gold, hint, hint, teal, teal.
     `(outline-1 ((t (:foreground ,gold :weight bold))))
     `(outline-2 ((t (:foreground ,gold :weight bold))))
     `(outline-3 ((t (:foreground ,hint :weight bold))))
     `(outline-4 ((t (:foreground ,hint :weight bold))))
     `(outline-5 ((t (:foreground ,teal :weight bold))))
     `(outline-6 ((t (:foreground ,teal :weight bold))))
     `(outline-7 ((t (:foreground ,fg-dim :weight bold))))
     `(outline-8 ((t (:foreground ,fg-dim :weight bold))))
     `(outline-minor-0 ((t (:background ,bg-light))))

     ;; markdown-mode
     `(markdown-header-face            ((t (:foreground ,gold :weight bold))))
     `(markdown-header-face-1          ((t (:inherit outline-1 :height 1.3))))
     `(markdown-header-face-2          ((t (:inherit outline-2 :height 1.2))))
     `(markdown-header-face-3          ((t (:inherit outline-3 :height 1.1))))
     `(markdown-header-face-4          ((t (:inherit outline-4))))
     `(markdown-header-face-5          ((t (:inherit outline-5))))
     `(markdown-header-face-6          ((t (:inherit outline-6))))
     `(markdown-header-delimiter-face  ((t (:foreground ,gutter))))
     `(markdown-header-rule-face       ((t (:foreground ,gutter))))
     `(markdown-bold-face              ((t (:foreground ,fg :weight bold))))
     `(markdown-italic-face            ((t (:foreground ,fg :slant italic))))
     `(markdown-strike-through-face    ((t (:foreground ,fg-dim :strike-through t))))
     `(markdown-code-face              ((t (:foreground ,green :background ,bg-dark :extend t))))
     `(markdown-pre-face               ((t (:foreground ,green :background ,bg-dark :extend t))))
     `(markdown-inline-code-face       ((t (:foreground ,green :background ,bg-dark))))
     `(markdown-language-keyword-face  ((t (:foreground ,teal))))
     `(markdown-link-face              ((t (:foreground ,teal))))
     `(markdown-url-face               ((t (:foreground ,teal :underline t))))
     `(markdown-plain-url-face         ((t (:foreground ,teal :underline t))))
     `(markdown-link-title-face        ((t (:foreground ,teal))))
     `(markdown-list-face              ((t (:foreground ,func))))
     `(markdown-blockquote-face        ((t (:foreground ,fg-dim :slant italic))))
     `(markdown-markup-face            ((t (:foreground ,gutter))))
     `(markdown-gfm-checkbox-face      ((t (:foreground ,green))))
     `(markdown-table-face             ((t (:foreground ,fg-dim))))
     `(markdown-metadata-key-face      ((t (:foreground ,gutter))))
     `(markdown-metadata-value-face    ((t (:foreground ,fg))))
     `(markdown-html-tag-name-face     ((t (:foreground ,teal))))
     `(markdown-html-tag-delimiter-face ((t (:foreground ,fg-dim))))
     `(markdown-html-attr-name-face    ((t (:foreground ,func))))
     `(markdown-html-attr-value-face   ((t (:foreground ,green))))
     `(markdown-highlight-face         ((t (:foreground ,black :background ,accent))))
     `(markdown-highlighting-face      ((t (:foreground ,black :background ,accent))))
     `(markdown-comment-face           ((t ,tk-comment)))
     `(markdown-math-face              ((t (:foreground ,func))))
     `(markdown-hr-face                ((t (:foreground ,border))))
     `(markdown-line-break-face        ((t (:foreground ,fg-dim :underline t))))
     `(markdown-html-entity-face       ((t (:foreground ,func))))
     `(markdown-language-info-face     ((t (:foreground ,fg-dim))))
     `(markdown-reference-face         ((t (:foreground ,teal))))
     `(markdown-footnote-marker-face   ((t (:foreground ,teal))))
     `(markdown-footnote-text-face     ((t (:foreground ,fg-dim))))
     `(markdown-missing-link-face      ((t (:foreground ,red :weight bold))))

     ;; org-mode
     `(org-level-1 ((t (:inherit outline-1 :height 1.3))))
     `(org-level-2 ((t (:inherit outline-2 :height 1.2))))
     `(org-level-3 ((t (:inherit outline-3 :height 1.1))))
     `(org-level-4 ((t (:inherit outline-4))))
     `(org-level-5 ((t (:inherit outline-5))))
     `(org-level-6 ((t (:inherit outline-6))))
     `(org-level-7 ((t (:inherit outline-7))))
     `(org-level-8 ((t (:inherit outline-8))))
     `(org-document-title        ((t (:foreground ,gold :weight bold :height 1.4))))
     `(org-document-info         ((t (:foreground ,fg-dim))))
     `(org-document-info-keyword ((t (:foreground ,gutter))))
     `(org-block             ((t (:background ,bg-dark :extend t))))
     `(org-block-begin-line  ((t (:foreground ,gutter :background ,bg-dark :extend t))))
     `(org-block-end-line    ((t (:foreground ,gutter :background ,bg-dark :extend t))))
     `(org-code              ((t (:foreground ,green :background ,bg-dark))))
     `(org-verbatim          ((t (:foreground ,green :background ,bg-dark))))
     `(org-table             ((t (:foreground ,fg-dim))))
     `(org-formula           ((t (:foreground ,func))))
     `(org-link              ((t (:foreground ,teal :underline t))))
     `(org-footnote          ((t (:foreground ,teal :underline t))))
     `(org-todo              ((t (:foreground ,red :weight bold))))
     `(org-done              ((t (:foreground ,green :weight bold))))
     `(org-headline-todo     ((t (:foreground ,fg))))
     `(org-headline-done     ((t (:foreground ,gutter :strike-through t))))
     `(org-date              ((t (:foreground ,mauve :underline t))))
     `(org-special-keyword   ((t (:foreground ,gutter))))
     `(org-drawer            ((t (:foreground ,gutter))))
     `(org-property-value    ((t (:foreground ,fg))))
     `(org-tag               ((t (:foreground ,fg-dim :weight bold))))
     `(org-priority          ((t (:foreground ,warn :weight bold))))
     `(org-checkbox          ((t (:foreground ,green :weight bold))))
     `(org-ellipsis          ((t (:foreground ,gutter :underline nil))))
     `(org-warning           ((t (:foreground ,red :weight bold))))
     `(org-agenda-structure  ((t (:foreground ,gold :weight bold))))
     `(org-agenda-date       ((t (:foreground ,fg :weight bold))))
     `(org-agenda-date-today ((t (:foreground ,accent :weight bold :underline t))))
     `(org-agenda-date-weekend ((t (:foreground ,fg-dim :weight bold))))
     `(org-agenda-done       ((t (:foreground ,green))))
     `(org-scheduled         ((t (:foreground ,green))))
     `(org-scheduled-today   ((t (:foreground ,green :weight bold))))
     `(org-scheduled-previously ((t (:foreground ,warn))))
     `(org-upcoming-deadline ((t (:foreground ,warn))))
     `(org-time-grid         ((t (:foreground ,gutter))))
     `(org-hide              ((t (:foreground ,bg :background ,bg))))
     `(org-quote             ((t (:foreground ,fg-dim :slant italic :extend t))))
     `(org-verse             ((t (:foreground ,fg-dim :slant italic :extend t))))

     ;; AUCTeX / font-latex.
     ;; NOTE: if `custom-set-faces' in your custom-file sets any of these, that
     ;; wins over the theme -- see the README.
     `(font-latex-math-face         ((t (:foreground ,func))))
     `(font-latex-sedate-face       ((t (:foreground ,teal))))
     `(font-latex-script-char-face  ((t (:foreground ,fg-dim))))
     `(font-latex-string-face       ((t ,tk-string)))
     `(font-latex-warning-face      ((t (:foreground ,red :weight bold))))
     `(font-latex-bold-face         ((t (:foreground ,fg :weight bold))))
     `(font-latex-italic-face       ((t (:foreground ,fg :slant italic))))
     `(font-latex-underline-face    ((t (:foreground ,fg :underline t))))
     `(font-latex-verbatim-face     ((t (:foreground ,green :background ,bg-dark))))
     `(font-latex-doctex-documentation-face ((t (:inherit font-lock-comment-face))))
     `(font-latex-doctex-preprocessor-face  ((t (:foreground ,teal))))
     `(font-latex-sectioning-0-face ((t (:foreground ,gold :weight bold :height 1.4))))
     `(font-latex-sectioning-1-face ((t (:foreground ,gold :weight bold :height 1.3))))
     `(font-latex-sectioning-2-face ((t (:foreground ,gold :weight bold :height 1.2))))
     `(font-latex-sectioning-3-face ((t (:foreground ,hint :weight bold :height 1.1))))
     `(font-latex-sectioning-4-face ((t (:foreground ,hint :weight bold))))
     `(font-latex-sectioning-5-face ((t (:foreground ,teal :weight bold))))
     `(font-latex-slide-title-face  ((t (:foreground ,gold :weight bold :height 1.3))))
     `(TeX-error-description-error   ((t (:foreground ,red :weight bold))))
     `(TeX-error-description-warning ((t (:foreground ,warn :weight bold))))
     `(font-latex-subscript-face    ((t (:height 0.9))))
     `(font-latex-superscript-face  ((t (:height 0.9))))
     `(TeX-error-description-tex-said ((t (:foreground ,fg-dim))))
     `(TeX-error-description-help    ((t (:foreground ,teal))))
     `(preview-face                 ((t (:background ,bg-light))))
     `(preview-reference-face       ((t (:foreground ,fg :background ,bg-light))))

     ;; highlight-defined (elisp)
     `(highlight-defined-function-name-face         ((t ,tk-func)))
     `(highlight-defined-builtin-function-name-face ((t ,tk-keyword)))
     `(highlight-defined-special-form-name-face     ((t (:foreground ,teal))))
     `(highlight-defined-macro-name-face            ((t (:foreground ,teal))))
     `(highlight-defined-variable-name-face         ((t (:foreground ,fg))))
     `(highlight-defined-face-name-face             ((t (:foreground ,hint))))

     ;; misc editing aids
     `(goggles-added   ((t (:background ,diff-add))))
     `(goggles-changed ((t (:background ,diff-change))))
     `(goggles-removed ((t (:background ,diff-delete))))
     `(yas-field-highlight-face ((t (:background ,bg-sel))))
     `(page-break-lines ((t (:foreground ,border))))
     `(show-inactive-region-face ((t (:background ,bg-light :extend t))))
     `(which-func ((t (:foreground ,gold))))
     `(bookmark-face ((t (:foreground ,accent :background ,bg-light))))

     ;; pdf-tools
     `(pdf-isearch-match    ((t (:foreground ,black :background ,accent))))
     `(pdf-isearch-lazy     ((t (:foreground ,black :background ,gold))))
     `(pdf-isearch-batch    ((t (:foreground ,black :background ,warn))))
     `(pdf-links-read-link  ((t (:foreground ,black :background ,red))))

     ;; info / help
     `(info-title-1     ((t (:foreground ,gold :weight bold :height 1.3))))
     `(info-title-2     ((t (:foreground ,gold :weight bold :height 1.2))))
     `(info-title-3     ((t (:foreground ,hint :weight bold :height 1.1))))
     `(info-title-4     ((t (:foreground ,hint :weight bold))))
     `(info-menu-header ((t (:foreground ,gold :weight bold))))
     `(info-menu-star   ((t (:foreground ,accent))))
     `(info-node        ((t (:foreground ,gold :weight bold))))
     `(info-xref        ((t (:inherit link))))
     `(info-xref-visited ((t (:inherit link-visited))))
     `(help-argument-name ((t (:foreground ,hint :slant italic))))
     `(help-for-help-header ((t (:foreground ,gold :weight bold :height 1.2))))
     `(Info-quoted      ((t (:foreground ,green))))

     ;; eldoc / describe
     `(eldoc-highlight-function-argument ((t (:foreground ,gold :weight bold))))

     ;;; -------------------------------------------------------------------
     ;;; ANSI / term colours (vterm, eshell, comint, shell-command output)
     ;;; Mirrors vim.g.terminal_color_0..15 from lua/valve-olive/init.lua.
     ;;; -------------------------------------------------------------------
     `(ansi-color-black          ((t (:foreground ,(funcall g 'term-black)      :background ,(funcall g 'term-black)))))
     `(ansi-color-red            ((t (:foreground ,(funcall g 'term-red)        :background ,(funcall g 'term-red)))))
     `(ansi-color-green          ((t (:foreground ,(funcall g 'term-green)      :background ,(funcall g 'term-green)))))
     `(ansi-color-yellow         ((t (:foreground ,(funcall g 'term-yellow)     :background ,(funcall g 'term-yellow)))))
     `(ansi-color-blue           ((t (:foreground ,(funcall g 'term-blue)       :background ,(funcall g 'term-blue)))))
     `(ansi-color-magenta        ((t (:foreground ,(funcall g 'term-magenta)    :background ,(funcall g 'term-magenta)))))
     `(ansi-color-cyan           ((t (:foreground ,(funcall g 'term-cyan)       :background ,(funcall g 'term-cyan)))))
     `(ansi-color-white          ((t (:foreground ,(funcall g 'term-white)      :background ,(funcall g 'term-white)))))
     `(ansi-color-bright-black   ((t (:foreground ,(funcall g 'term-br-black)   :background ,(funcall g 'term-br-black)))))
     `(ansi-color-bright-red     ((t (:foreground ,(funcall g 'term-br-red)     :background ,(funcall g 'term-br-red)))))
     `(ansi-color-bright-green   ((t (:foreground ,(funcall g 'term-br-green)   :background ,(funcall g 'term-br-green)))))
     `(ansi-color-bright-yellow  ((t (:foreground ,(funcall g 'term-br-yellow)  :background ,(funcall g 'term-br-yellow)))))
     `(ansi-color-bright-blue    ((t (:foreground ,(funcall g 'term-br-blue)    :background ,(funcall g 'term-br-blue)))))
     `(ansi-color-bright-magenta ((t (:foreground ,(funcall g 'term-br-magenta) :background ,(funcall g 'term-br-magenta)))))
     `(ansi-color-bright-cyan    ((t (:foreground ,(funcall g 'term-br-cyan)    :background ,(funcall g 'term-br-cyan)))))
     `(ansi-color-bright-white   ((t (:foreground ,(funcall g 'term-br-white)   :background ,(funcall g 'term-br-white)))))

     `(term-color-black   ((t (:foreground ,(funcall g 'term-black)   :background ,(funcall g 'term-black)))))
     `(term-color-red     ((t (:foreground ,(funcall g 'term-red)     :background ,(funcall g 'term-red)))))
     `(term-color-green   ((t (:foreground ,(funcall g 'term-green)   :background ,(funcall g 'term-green)))))
     `(term-color-yellow  ((t (:foreground ,(funcall g 'term-yellow)  :background ,(funcall g 'term-yellow)))))
     `(term-color-blue    ((t (:foreground ,(funcall g 'term-blue)    :background ,(funcall g 'term-blue)))))
     `(term-color-magenta ((t (:foreground ,(funcall g 'term-magenta) :background ,(funcall g 'term-magenta)))))
     `(term-color-cyan    ((t (:foreground ,(funcall g 'term-cyan)    :background ,(funcall g 'term-cyan)))))
     `(term-color-white   ((t (:foreground ,(funcall g 'term-white)   :background ,(funcall g 'term-white)))))
     `(term-color-bright-black   ((t (:foreground ,(funcall g 'term-br-black)   :background ,(funcall g 'term-br-black)))))
     `(term-color-bright-red     ((t (:foreground ,(funcall g 'term-br-red)     :background ,(funcall g 'term-br-red)))))
     `(term-color-bright-green   ((t (:foreground ,(funcall g 'term-br-green)   :background ,(funcall g 'term-br-green)))))
     `(term-color-bright-yellow  ((t (:foreground ,(funcall g 'term-br-yellow)  :background ,(funcall g 'term-br-yellow)))))
     `(term-color-bright-blue    ((t (:foreground ,(funcall g 'term-br-blue)    :background ,(funcall g 'term-br-blue)))))
     `(term-color-bright-magenta ((t (:foreground ,(funcall g 'term-br-magenta) :background ,(funcall g 'term-br-magenta)))))
     `(term-color-bright-cyan    ((t (:foreground ,(funcall g 'term-br-cyan)    :background ,(funcall g 'term-br-cyan)))))
     `(term-color-bright-white   ((t (:foreground ,(funcall g 'term-br-white)   :background ,(funcall g 'term-br-white)))))
     `(term               ((t (:foreground ,fg :background ,bg))))

     ;; comint / eshell
     `(comint-highlight-prompt ((t (:foreground ,teal :weight bold))))
     `(comint-highlight-input  ((t (:foreground ,fg :weight bold))))
     `(eshell-prompt           ((t (:foreground ,teal :weight bold))))
     `(eshell-ls-directory     ((t (:foreground ,teal :weight bold))))
     `(eshell-ls-executable    ((t (:foreground ,green))))
     `(eshell-ls-symlink       ((t (:foreground ,mauve))))
     `(eshell-ls-archive       ((t (:foreground ,warn))))
     `(eshell-ls-backup        ((t (:foreground ,gutter))))
     `(eshell-ls-missing       ((t (:foreground ,red))))
     `(eshell-ls-special       ((t (:foreground ,mauve))))
     `(eshell-ls-unreadable    ((t (:foreground ,gutter))))
     `(eshell-ls-readonly      ((t (:foreground ,fg-dim)))))))

(defun valve-olive-themes--apply (theme variant)
  "Apply the Valve Olive faces for VARIANT to THEME, and set frame colours."
  (apply #'custom-theme-set-faces theme (valve-olive-themes--faces variant))
  (let* ((p (valve-olive-themes-palette variant))
         (g (lambda (k) (cdr (assq k p)))))
    (custom-theme-set-variables
     theme
     '(frame-background-mode 'dark)
     ;; ANSI colour vector for term/ansi-color consumers.
     `(ansi-color-names-vector
       [,(funcall g 'term-black)   ,(funcall g 'term-red)
        ,(funcall g 'term-green)   ,(funcall g 'term-yellow)
        ,(funcall g 'term-blue)    ,(funcall g 'term-magenta)
        ,(funcall g 'term-cyan)    ,(funcall g 'term-white)]))))

(defmacro valve-olive-themes-deftheme (name docstring variant)
  "Define a Valve Olive theme NAME with DOCSTRING for VARIANT.
VARIANT is either `default' or `darker'."
  (declare (indent defun))
  `(progn
     (deftheme ,name ,docstring)
     (valve-olive-themes--apply ',name ',variant)
     (provide-theme ',name)
     (when (and (boundp 'custom-theme-load-path) load-file-name)
       (add-to-list 'custom-theme-load-path
                    (file-name-directory load-file-name)))))

(provide 'valve-olive-themes)
;;; valve-olive-themes.el ends here
