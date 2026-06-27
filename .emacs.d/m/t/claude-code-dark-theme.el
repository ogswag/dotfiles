;;; claude-code-dark-theme.el --- Claude Code Dark -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: ported by Claude from ashwingopalsamy/claude-code-theme
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A single-file Emacs port of "Claude Code Dark" from the VSCode theme
;; collection ashwingopalsamy/claude-code-theme.  The palette is Anthropic's
;; warm "Claude" brand: a terracotta accent over paper/ink neutrals.
;;
;; All colors are collected in the `let*' palette below; edit a color there
;; once and every face that uses it updates.  Sibling variants
;; (high-contrast, brand, and the light editions) differ only in this block.

;;; Code:

;;;###theme-autoload
(deftheme claude-code-dark
  "Dark, warm Claude Code theme (terracotta accent on ink neutrals).
Ported from the Claude Code Dark VSCode theme."
  :background-mode 'dark
  :kind 'color-scheme)

(let* ((class '((class color) (min-colors 89)))

       ;; Backgrounds & neutrals
       (bg        "#141413") ; editor background
       (bg-alt    "#1f1d1a") ; surface: sidebar, panels, inputs
       (bg-hl     "#2b2420") ; current-line (lineHighlight tint over bg)
       (fg        "#eae7df") ; main foreground
       (ui-border "#4a473f") ; borders, indent guides
       (line-num  "#6b665f") ; line numbers / subtle
       (muted     "#a9a39a") ; dim text, ansi white
       (comment   "#b8afa3") ; comments
       (punct     "#c6bdb2") ; punctuation
       (op        "#e2d8cc") ; operators
       (sel       "#46362f") ; opaque tint of selectionBackground
       (lazy-bg   "#3a3530") ; warm tint for lazy-highlight

       ;; Accent (interactive / mode-line)
       (accent    "#d4967e")
       (accent-hi "#e0ab96")

       ;; Syntax accents
       (kw        "#e2a48b") ; keyword / storage
       (str       "#b5e6a0") ; string
       (num       "#f4dc90") ; number / constant
       (param     "#f0cdba") ; parameter
       (func      "#ffc1a6") ; function
       (type      "#afccf8") ; type / class
       (tag       "#d9645b") ; tag
       (attr      "#f6dfc7") ; attribute

       ;; Semantic
       (err       "#d47563")
       (warn      "#e8c96b")
       (ok        "#9aca86")
       (info      "#61aaf2")
       (violet    "#9b87f5"))

  (custom-theme-set-faces
   'claude-code-dark

   ;; Basic faces
   `(default			((,class (:background ,bg :foreground ,fg))))
   `(cursor			((,class (:background ,accent))))
   `(fringe			((,class (:background ,bg))))
   `(hl-line			((,class (:background ,bg-hl))))
   `(highlight			((,class (:background ,bg-hl))))
   `(region			((,class (:background ,sel :extend t))))
   `(secondary-selection	((,class (:background ,bg-alt :extend t))))
   `(shadow			((,class (:foreground ,line-num))))
   `(tooltip			((,class (:inherit variable-pitch :background ,bg-alt :foreground ,fg))))
   `(vertical-border		((,class (:foreground ,ui-border))))
   `(window-divider		((,class (:foreground ,ui-border))))
   `(window-divider-first-pixel	((,class (:foreground ,ui-border))))
   `(window-divider-last-pixel	((,class (:foreground ,ui-border))))
   `(minibuffer-prompt		((,class (:foreground ,accent :weight bold))))
   `(escape-glyph		((,class (:foreground ,info :weight bold))))
   `(homoglyph			((,class (:foreground ,info))))
   `(error			((,class (:foreground ,err :weight bold))))
   `(warning			((,class (:foreground ,warn :weight bold))))
   `(success			((,class (:foreground ,ok :weight bold))))

   ;; Line numbers
   `(line-number		((,class (:background ,bg :foreground ,line-num))))
   `(line-number-current-line	((,class (:background ,bg-hl :foreground ,fg))))
   `(linum			((,class (:background ,bg :foreground ,line-num))))

   ;; Mode line (accent on surface)
   `(mode-line			((,class (:background ,accent :foreground ,bg :box nil))))
   `(mode-line-active		((,class (:background ,accent :foreground ,bg :box nil))))
   `(mode-line-inactive		((,class (:background ,bg-alt :foreground ,muted :box nil))))
   `(mode-line-highlight	((,class (:background ,accent-hi :foreground ,bg))))
   `(mode-line-buffer-id	((,class (:weight bold))))
   `(header-line		((,class (:background ,bg-alt :foreground ,fg))))

   ;; Search
   `(isearch			((,class (:background ,accent :foreground ,bg))))
   `(isearch-fail		((,class (:background ,err :foreground ,bg))))
   `(lazy-highlight		((,class (:background ,lazy-bg :foreground ,fg))))
   `(match			((,class (:background ,lazy-bg :foreground ,fg))))

   ;; Parens
   `(show-paren-match		((,class (:background ,sel :weight bold))))
   `(show-paren-match-expression ((,class (:background ,sel))))
   `(show-paren-mismatch	((,class (:background ,err :foreground ,bg :weight bold))))

   ;; Links & buttons
   `(link			((,class (:foreground ,info :underline t))))
   `(link-visited		((,class (:foreground ,violet :underline t))))
   `(button			((,class (:foreground ,info :underline t))))
   `(help-key-binding		((,class (:background ,bg-alt :foreground ,fg :box (:line-width 1 :color ,ui-border)))))

   ;; Font lock
   `(font-lock-comment-face		((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face	((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face			((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-markup-face		((,class (:inherit font-lock-doc-face))))
   `(font-lock-keyword-face		((,class (:foreground ,kw :weight bold))))
   `(font-lock-operator-face		((,class (:foreground ,op))))
   `(font-lock-string-face		((,class (:foreground ,str))))
   `(font-lock-regexp-face		((,class (:foreground ,str))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,tag))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,tag))))
   `(font-lock-number-face		((,class (:foreground ,num))))
   `(font-lock-constant-face		((,class (:foreground ,num))))
   `(font-lock-escape-face		((,class (:foreground ,tag))))
   `(font-lock-variable-name-face	((,class (:foreground ,fg))))
   `(font-lock-variable-use-face	((,class (:foreground ,param))))
   `(font-lock-builtin-face		((,class (:foreground ,kw))))
   `(font-lock-function-name-face	((,class (:foreground ,func :weight bold))))
   `(font-lock-function-call-face	((,class (:foreground ,func))))
   `(font-lock-type-face		((,class (:foreground ,type))))
   `(font-lock-property-name-face	((,class (:foreground ,attr))))
   `(font-lock-property-use-face	((,class (:foreground ,attr))))
   `(font-lock-preprocessor-face	((,class (:foreground ,tag))))
   `(font-lock-punctuation-face		((,class (:foreground ,punct))))
   `(font-lock-delimiter-face		((,class (:foreground ,punct))))
   `(font-lock-bracket-face		((,class (:foreground ,punct))))
   `(font-lock-misc-punctuation-face	((,class (:foreground ,punct))))
   `(font-lock-negation-char-face	((,class (:foreground ,tag))))
   `(font-lock-warning-face		((,class (:foreground ,err :weight bold))))

   ;; Completion / minibuffer
   `(completions-common-part	((,class (:foreground ,accent :weight bold))))
   `(completions-first-difference ((,class (:foreground ,fg))))
   `(completions-annotations	((,class (:foreground ,muted :slant italic))))

   ;; Vertico
   `(vertico-current		((,class (:background ,bg-hl :extend t))))
   `(vertico-group-title	((,class (:foreground ,accent :slant italic))))
   `(vertico-group-separator	((,class (:foreground ,ui-border :strike-through t))))

   ;; Marginalia
   `(marginalia-key		((,class (:foreground ,accent))))
   `(marginalia-documentation	((,class (:foreground ,comment :slant italic))))
   `(marginalia-file-name	((,class (:foreground ,muted))))
   `(marginalia-size		((,class (:foreground ,num))))
   `(marginalia-number		((,class (:foreground ,num))))
   `(marginalia-modified	((,class (:foreground ,warn))))

   ;; which-key
   `(which-key-key-face			((,class (:foreground ,accent :weight bold))))
   `(which-key-group-description-face	((,class (:foreground ,type))))
   `(which-key-command-description-face	((,class (:foreground ,fg))))
   `(which-key-separator-face		((,class (:foreground ,comment))))

   ;; rainbow-delimiters (cycle the accent palette)
   `(rainbow-delimiters-depth-1-face	((,class (:foreground ,accent))))
   `(rainbow-delimiters-depth-2-face	((,class (:foreground ,type))))
   `(rainbow-delimiters-depth-3-face	((,class (:foreground ,ok))))
   `(rainbow-delimiters-depth-4-face	((,class (:foreground ,num))))
   `(rainbow-delimiters-depth-5-face	((,class (:foreground ,violet))))
   `(rainbow-delimiters-depth-6-face	((,class (:foreground ,tag))))
   `(rainbow-delimiters-depth-7-face	((,class (:foreground ,str))))
   `(rainbow-delimiters-depth-8-face	((,class (:foreground ,attr))))
   `(rainbow-delimiters-depth-9-face	((,class (:foreground ,info))))
   `(rainbow-delimiters-unmatched-face	((,class (:foreground ,err :weight bold))))
   `(rainbow-delimiters-mismatched-face	((,class (:foreground ,err :weight bold))))

   ;; Compilation / flymake
   `(flymake-error		((,class (:underline (:style wave :color ,err)))))
   `(flymake-warning		((,class (:underline (:style wave :color ,warn)))))
   `(flymake-note		((,class (:underline (:style wave :color ,info)))))
   `(compilation-error		((,class (:foreground ,err))))
   `(compilation-warning	((,class (:foreground ,warn))))
   `(compilation-info		((,class (:foreground ,info))))

   ;; Diff / VC gutter
   `(diff-added			((,class (:foreground ,ok))))
   `(diff-removed		((,class (:foreground ,err))))
   `(diff-changed		((,class (:foreground ,info))))
   `(diff-hl-insert		((,class (:foreground ,ok :background ,ok))))
   `(diff-hl-change		((,class (:foreground ,info :background ,info))))
   `(diff-hl-delete		((,class (:foreground ,err :background ,err))))
   `(git-gutter:added		((,class (:foreground ,ok))))
   `(git-gutter:modified	((,class (:foreground ,info))))
   `(git-gutter:deleted		((,class (:foreground ,err))))

   ;; Whitespace / indent guides
   `(whitespace-space		((,class (:foreground ,ui-border))))
   `(whitespace-tab		((,class (:foreground ,ui-border))))
   `(whitespace-newline		((,class (:foreground ,ui-border))))
   `(whitespace-indentation	((,class (:foreground ,ui-border))))
   `(whitespace-trailing	((,class (:background ,lazy-bg))))
   `(fill-column-indicator	((,class (:foreground ,ui-border))))

   ;; ANSI terminal palette
   `(ansi-color-black		((,class (:background ,bg :foreground ,bg))))
   `(ansi-color-red		((,class (:background ,tag :foreground ,tag))))
   `(ansi-color-green		((,class (:background ,ok :foreground ,ok))))
   `(ansi-color-yellow		((,class (:background ,warn :foreground ,warn))))
   `(ansi-color-blue		((,class (:background ,info :foreground ,info))))
   `(ansi-color-magenta		((,class (:background ,violet :foreground ,violet))))
   `(ansi-color-cyan		((,class (:background ,type :foreground ,type))))
   `(ansi-color-white		((,class (:background ,muted :foreground ,muted))))
   `(ansi-color-bright-black	((,class (:background ,line-num :foreground ,line-num))))
   `(ansi-color-bright-red	((,class (:background ,err :foreground ,err))))
   `(ansi-color-bright-green	((,class (:background ,str :foreground ,str))))
   `(ansi-color-bright-yellow	((,class (:background ,num :foreground ,num))))
   `(ansi-color-bright-blue	((,class (:background ,type :foreground ,type))))
   `(ansi-color-bright-magenta	((,class (:background ,violet :foreground ,violet))))
   `(ansi-color-bright-cyan	((,class (:background ,type :foreground ,type))))
   `(ansi-color-bright-white	((,class (:background ,fg :foreground ,fg))))

   ;; Markdown
   `(markdown-header-face-1	((,class (:foreground ,func :weight bold :height 1.3))))
   `(markdown-header-face-2	((,class (:foreground ,ok :weight bold :height 1.1))))
   `(markdown-header-face-3	((,class (:foreground ,type :weight bold))))
   `(markdown-header-face-4	((,class (:foreground ,num :weight bold))))
   `(markdown-header-face-5	((,class (:foreground ,violet :weight bold))))
   `(markdown-header-face-6	((,class (:foreground ,tag :weight bold))))
   `(markdown-bold-face		((,class (:foreground ,fg :weight bold))))
   `(markdown-italic-face	((,class (:foreground ,fg :slant italic))))
   `(markdown-code-face		((,class (:foreground ,str :background ,bg-alt :extend t))))
   `(markdown-inline-code-face	((,class (:foreground ,str :background ,bg-alt))))
   `(markdown-pre-face		((,class (:foreground ,op :background ,bg-alt :extend t))))
   `(markdown-link-face		((,class (:foreground ,info))))
   `(markdown-url-face		((,class (:foreground ,comment :underline t))))
   `(markdown-markup-face	((,class (:foreground ,punct))))
   `(markdown-blockquote-face	((,class (:foreground ,muted :slant italic))))
   `(markdown-list-face		((,class (:foreground ,accent))))

   ;; Org headings (cycle the accent palette)
   `(org-level-1		((,class (:foreground ,func :weight bold :height 1.3))))
   `(org-level-2		((,class (:foreground ,ok :weight bold :height 1.1))))
   `(org-level-3		((,class (:foreground ,type :weight bold))))
   `(org-level-4		((,class (:foreground ,num :weight bold))))
   `(org-level-5		((,class (:foreground ,violet :weight bold))))
   `(org-level-6		((,class (:foreground ,tag :weight bold))))
   `(org-level-7		((,class (:foreground ,info :weight bold))))
   `(org-level-8		((,class (:foreground ,muted :weight bold))))
   `(org-document-title	((,class (:foreground ,fg :weight bold :height 1.4))))
   `(org-block		((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line	((,class (:foreground ,comment :background ,bg-alt :extend t))))
   `(org-code		((,class (:foreground ,str))))
   `(org-verbatim		((,class (:foreground ,attr))))
   `(org-link		((,class (:foreground ,info :underline t))))
   `(org-todo		((,class (:foreground ,tag :weight bold))))
   `(org-done		((,class (:foreground ,ok :weight bold)))))

  (custom-theme-set-variables
   'claude-code-dark
   `(ansi-color-names-vector
     [,bg ,tag ,ok ,warn ,info ,violet ,type ,muted])))

(provide-theme 'claude-code-dark)

(provide 'claude-code-dark-theme)

;;; claude-code-dark-theme.el ends here
