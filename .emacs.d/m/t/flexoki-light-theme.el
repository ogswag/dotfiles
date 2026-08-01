;;; flexoki-light-theme.el --- Flexoki Light -*- no-byte-compile: t; lexical-binding: t; -*-

;; Ported from farigab/nocturne-and-daylight flexoki-light.json
;; Author: github.com/ogswag
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A single-file Emacs port of the "Flexoki Light" theme distributed as a
;; VSCode/TextMate theme in farigab/nocturne-and-daylight.  The Flexoki color
;; palette is by Steph Ango (https://stephango.com/flexoki).

;;; Code:

;;;###theme-autoload
(deftheme flexoki-light
  "Light, warm, low-saturation Flexoki theme.
Ported from the Flexoki Light VSCode theme (palette by Steph Ango)."
  :background-mode 'light
  :kind 'color-scheme)

(let* ((class '((class color) (min-colors 89)))

       ;; Backgrounds & neutrals
       (bg        "#fffcf0") ; paper / editor background
       (bg-alt    "#f2f0e5") ; sidebar, panels, inputs
       (bg-hl     "#f2f0e5") ; current-line highlight
       (fg        "#100f0f") ; black / main foreground
       (ui-border "#e6e4d9") ; whitespace, indent guides, borders
       (muted     "#b7b5ac") ; line numbers, ui-3, bright white
       (muted-2   "#878580") ; comments, inactive fg, ansi white
       (punct     "#575653") ; punctuation, ansi bright black
       (sel       "#cdd9e6") ; opaque tint of selectionBackground
       (lazy-bg   "#ece4ce") ; light yellow tint for lazy-highlight

       ;; Accents (Flexoki)
       (red       "#af3029") (red-br    "#d14d41")
       (green     "#66800b") (green-br  "#879a39")
       (yellow    "#ad8301") (yellow-br "#d0a215") (yellow-600 "#8b6f20")
       (blue      "#205ea6") (blue-br   "#4385be")
       (purple    "#5e409d") (purple-br "#8b7ec8")
       (cyan      "#24837b") (cyan-br   "#3aa99f"))

  (custom-theme-set-faces
   'flexoki-light

   ;; Basic faces
   `(default			((,class (:background ,bg :foreground ,fg))))
   `(cursor			((,class (:background ,fg))))
   `(fringe			((,class (:background ,bg))))
   `(hl-line			((,class (:background ,bg-hl))))
   `(highlight			((,class (:background ,bg-hl))))
   `(region			((,class (:background ,sel :extend t))))
   `(secondary-selection	((,class (:background ,bg-alt :extend t))))
   `(shadow			((,class (:foreground ,muted-2))))
   `(tooltip			((,class (:inherit variable-pitch :background ,bg-alt :foreground ,fg))))
   `(vertical-border		((,class (:foreground ,ui-border))))
   `(window-divider		((,class (:foreground ,ui-border))))
   `(window-divider-first-pixel	((,class (:foreground ,ui-border))))
   `(window-divider-last-pixel	((,class (:foreground ,ui-border))))
   `(minibuffer-prompt		((,class (:foreground ,blue-br :weight bold))))
   `(escape-glyph		((,class (:foreground ,cyan :weight bold))))
   `(homoglyph			((,class (:foreground ,cyan))))
   `(error			((,class (:foreground ,red-br :weight bold))))
   `(warning			((,class (:foreground ,yellow :weight bold))))
   `(success			((,class (:foreground ,green :weight bold))))

   ;; Line numbers
   `(line-number		((,class (:inherit default :background ,bg :foreground ,muted))))
   `(line-number-current-line	((,class (:inherit default :background ,bg-hl :foreground ,fg))))
   `(linum			((,class (:inherit default :background ,bg :foreground ,muted))))

   ;; Mode line (statusBar: white on blue)
   `(mode-line			((,class (:background ,blue-br :foreground ,bg :box nil))))
   `(mode-line-active		((,class (:background ,blue-br :foreground ,bg :box nil))))
   `(mode-line-inactive		((,class (:background ,bg-alt :foreground ,muted-2 :box nil))))
   `(mode-line-highlight	((,class (:background ,blue :foreground ,bg))))
   `(mode-line-buffer-id	((,class (:weight bold))))
   `(header-line		((,class (:background ,bg-alt :foreground ,fg))))

   ;; Search
   `(isearch			((,class (:background ,yellow-600 :foreground ,bg))))
   `(isearch-fail		((,class (:background ,red-br :foreground ,bg))))
   `(lazy-highlight		((,class (:background ,lazy-bg :foreground ,fg))))
   `(match			((,class (:background ,lazy-bg :foreground ,fg))))

   ;; Parens
   `(show-paren-match		((,class (:background ,sel :weight bold))))
   `(show-paren-match-expression ((,class (:background ,sel))))
   `(show-paren-mismatch	((,class (:background ,red-br :foreground ,bg :weight bold))))

   ;; Links & buttons
   `(link			((,class (:foreground ,blue :underline t))))
   `(link-visited		((,class (:foreground ,purple :underline t))))
   `(button			((,class (:foreground ,blue :underline t))))
   `(help-key-binding		((,class (:background ,bg-alt :foreground ,fg :box (:line-width 1 :color ,ui-border)))))

   ;; Font lock
   `(font-lock-comment-face		((,class (:foreground ,muted-2 :slant italic))))
   `(font-lock-comment-delimiter-face	((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face			((,class (:foreground ,muted-2 :slant italic))))
   `(font-lock-doc-markup-face		((,class (:inherit font-lock-doc-face))))
   `(font-lock-keyword-face		((,class (:foreground ,red))))
   `(font-lock-operator-face		((,class (:foreground ,fg))))
   `(font-lock-string-face		((,class (:foreground ,green))))
   `(font-lock-regexp-face		((,class (:foreground ,cyan))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyan))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,cyan))))
   `(font-lock-number-face		((,class (:foreground ,purple))))
   `(font-lock-constant-face		((,class (:foreground ,purple))))
   `(font-lock-escape-face		((,class (:foreground ,cyan))))
   `(font-lock-variable-name-face	((,class (:foreground ,fg))))
   `(font-lock-variable-use-face	((,class (:foreground ,fg))))
   `(font-lock-builtin-face		((,class (:foreground ,purple :slant italic))))
   `(font-lock-function-name-face	((,class (:foreground ,blue-br))))
   `(font-lock-function-call-face	((,class (:foreground ,blue-br))))
   `(font-lock-type-face		((,class (:foreground ,yellow-600))))
   `(font-lock-property-name-face	((,class (:foreground ,yellow-600))))
   `(font-lock-property-use-face	((,class (:foreground ,yellow-600))))
   `(font-lock-preprocessor-face	((,class (:foreground ,blue-br))))
   `(font-lock-punctuation-face		((,class (:foreground ,punct))))
   `(font-lock-delimiter-face		((,class (:foreground ,punct))))
   `(font-lock-bracket-face		((,class (:foreground ,punct))))
   `(font-lock-misc-punctuation-face	((,class (:foreground ,punct))))
   `(font-lock-negation-char-face	((,class (:foreground ,red))))
   `(font-lock-warning-face		((,class (:foreground ,red-br :weight bold))))
   ;; Completion / minibuffer
   `(completions-common-part	((,class (:foreground ,blue-br :weight bold))))
   `(completions-first-difference ((,class (:foreground ,fg))))
   `(completions-annotations	((,class (:foreground ,muted-2 :slant italic))))

   ;; Compilation / flymake
   `(flymake-error		((,class (:underline (:style wave :color ,red-br)))))
   `(flymake-warning		((,class (:underline (:style wave :color ,yellow)))))
   `(flymake-note		((,class (:underline (:style wave :color ,blue-br)))))
   `(compilation-error		((,class (:foreground ,red-br))))
   `(compilation-warning	((,class (:foreground ,yellow))))
   `(compilation-info		((,class (:foreground ,blue-br))))

   ;; Diff / VC gutter (editorGutter.*)
   `(diff-added			((,class (:foreground ,green-br))))
   `(diff-removed		((,class (:foreground ,red-br))))
   `(diff-changed		((,class (:foreground ,blue-br))))
   `(diff-hl-insert		((,class (:foreground ,green-br :background ,green-br))))
   `(diff-hl-change		((,class (:foreground ,blue-br :background ,blue-br))))
   `(diff-hl-delete		((,class (:foreground ,red-br :background ,red-br))))
   `(git-gutter:added		((,class (:foreground ,green-br))))
   `(git-gutter:modified	((,class (:foreground ,blue-br))))
   `(git-gutter:deleted		((,class (:foreground ,red-br))))

   ;; Whitespace / indent guides
   `(whitespace-space		((,class (:foreground ,ui-border))))
   `(whitespace-tab		((,class (:foreground ,ui-border))))
   `(whitespace-newline		((,class (:foreground ,ui-border))))
   `(whitespace-indentation	((,class (:foreground ,ui-border))))
   `(whitespace-trailing	((,class (:background ,lazy-bg))))
   `(fill-column-indicator	((,class (:foreground ,ui-border))))

   ;; ANSI terminal palette (terminal.ansi*)
   `(ansi-color-black		((,class (:background ,fg :foreground ,fg))))
   `(ansi-color-red		((,class (:background ,red :foreground ,red))))
   `(ansi-color-green		((,class (:background ,green :foreground ,green))))
   `(ansi-color-yellow		((,class (:background ,yellow :foreground ,yellow))))
   `(ansi-color-blue		((,class (:background ,blue :foreground ,blue))))
   `(ansi-color-magenta		((,class (:background ,purple :foreground ,purple))))
   `(ansi-color-cyan		((,class (:background ,cyan :foreground ,cyan))))
   `(ansi-color-white		((,class (:background ,muted-2 :foreground ,muted-2))))
   `(ansi-color-bright-black	((,class (:background ,punct :foreground ,punct))))
   `(ansi-color-bright-red	((,class (:background ,red-br :foreground ,red-br))))
   `(ansi-color-bright-green	((,class (:background ,green-br :foreground ,green-br))))
   `(ansi-color-bright-yellow	((,class (:background ,yellow-br :foreground ,yellow-br))))
   `(ansi-color-bright-blue	((,class (:background ,blue-br :foreground ,blue-br))))
   `(ansi-color-bright-magenta	((,class (:background ,purple-br :foreground ,purple-br))))
   `(ansi-color-bright-cyan	((,class (:background ,cyan-br :foreground ,cyan-br))))
   `(ansi-color-bright-white	((,class (:background ,muted :foreground ,muted))))

   ;; Org headings (cycle the accent palette)
   `(org-level-1		((,class (:foreground ,blue-br :weight bold :height 1.3))))
   `(org-level-2		((,class (:foreground ,green :weight bold :height 1.1))))
   `(org-level-3		((,class (:foreground ,purple :weight bold))))
   `(org-level-4		((,class (:foreground ,yellow-600 :weight bold))))
   `(org-level-5		((,class (:foreground ,cyan :weight bold))))
   `(org-level-6		((,class (:foreground ,red :weight bold))))
   `(org-level-7		((,class (:foreground ,blue :weight bold))))
   `(org-level-8		((,class (:foreground ,muted-2 :weight bold))))
   `(org-document-title	((,class (:foreground ,fg :weight bold :height 1.4))))
   `(org-block		((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line	((,class (:foreground ,muted-2 :background ,bg-alt :extend t))))
   `(org-code		((,class (:foreground ,cyan))))
   `(org-verbatim		((,class (:foreground ,green))))
   `(org-link		((,class (:foreground ,blue :underline t))))
   `(org-todo		((,class (:foreground ,red :weight bold))))
   `(org-done		((,class (:foreground ,green :weight bold)))))

  (custom-theme-set-variables
   'flexoki-light
   `(ansi-color-names-vector
     [,fg ,red ,green ,yellow ,blue ,purple ,cyan ,muted-2])))

(provide-theme 'flexoki-light)

(provide 'flexoki-light-theme)

;;; flexoki-light-theme.el ends here
