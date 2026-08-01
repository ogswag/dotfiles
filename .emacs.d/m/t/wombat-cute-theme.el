;;; wombat-cute-theme.el --- Custom face theme for Emacs -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2011-2026 Free Software Foundation, Inc.

;; Author: Kristoffer Grönlund <krig@koru.se>
;; Made it "cute": Alexander Zakharov <irlpilled@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; All colors used by the theme are collected in the `let*' palette at
;; the top of `custom-theme-set-faces' below.  To retheme, edit a color
;; there once and every face that uses it updates.

;;; Code:

;;;###theme-autoload
(deftheme wombat-cute
  "Medium-contrast faces with a dark gray background.
Adapted, with permission, from a Vim color scheme by Lars H. Nielsen.
Basic, Font Lock, Isearch, Gnus, Message, and Ansi-Color faces
are included."
  :background-mode 'dark
  :kind 'color-scheme)

(let* ((class '((class color) (min-colors 89)))

       ;; Neutrals & backgrounds
       (bg                   "#212121") ; main background
       (fg                   "#E9E9E9") ; main foreground
       (white                "#ffffff")
       (cream                "#f6f3e8") ; tooltip text
       (bg-highlight         "#454545") ; highlight overlay
       (bg-button            "#333333") ; buttons, help keys
       (bg-header            "#303030") ; header line
       (header-fg            "#e7f6da")
       (bg-modeline          "#343434") ; mode line & isearch
       (modeline-fg          "#F7E5C6")
       (bg-modeline-inactive "#3E3B3A")
       (modeline-inactive-fg "#968975")
       (bg-region            "#123499") ; active region
       (bg-secondary         "#333366") ; secondary selection
       (secondary-fg         "#E9E9E9")
       (bg-lazy              "#384048") ; lazy-highlight
       (lazy-fg              "#a0a8b0")
       (isearch-fg           "#B8A998")
       (gray-bright-black    "#444444") ; ansi bright black
       (bg-verbatim          "#444")    ; latex verbatim

       ;; Accents
       (coral         "#e5786d") ; prompt, visited link, separator, bright red
       (apricot       "#ddaa6f") ; escape glyph
       (comment-gray  "#99968b")
       (grass         "#A6CE92") ; functions, types
       (ash-gray      "#a8b5ae") ; preprocessor
       (sky           "#81C6D6") ; keywords
       (pink          "Pink1")   ; negation
       (peach         "#FABD9E") ; strings
       (light-moss    "#c5cfc4") ; regexp
       (light-apricot "#ffe5ba") ; regexp grouping
       (wisteria      "#D4A4DE") ; builtins
       (link-blue     "#8ac6f2") ; links, gnus headers, bright blue
       (orange-wave   "Orange1") ; flymake error underline
       (paren-bg      "steelblue3")
       (error-red     "#d00")    ; paren mismatch

       ;; gnus / message greens
       (green         "#95e454")
       (yellow-green  "#cae682")
       (tan           "#ccaa8f")

       ;; ANSI palette
       (ansi-red            "#b85149")
       (ansi-green          "#92a65e")
       (ansi-blue           "#5b98c2")
       (ansi-magenta        "#64619a")
       (ansi-cyan           "#3f9f9e")
       (ansi-bright-yellow  "#edc4a3")
       (ansi-bright-magenta "#a6a1de")
       (ansi-bright-cyan    "#70cecc")

       ;; Headings (org levels & LaTeX sectioning)
       (heading1-fg "#DACFE6") (heading1-bg "#322D37")
       (heading2-fg "#EFCAB2") (heading2-bg "#3D2A2D")
       (heading3-bg "#004020") ; shared by org level 3 and LaTeX sectioning 3
       (org3-fg     "#deeede")
       (latex3-fg   "#CDDFD9")
       (org4-fg     "#99ddff") (org4-bg   "#152C43")
       (latex4-fg   "#D3E0E7") (latex4-bg "#0E3257")
       (org5-fg     "#21da7a")
       (org6-fg     "#ff883d")
       (org7-fg     "#d451d9")
       (org8-fg     "#077ffa")
       (latex0-fg   "cornflower blue")
       (latex5-fg   "#eeffbb") ; also verbatim text

       ;; LaTeX misc
       (latex-string  "#D8CF8E")
       (latex-math    "#F2C4EC")
       (latex-sedate  "#9dccd7")
       (latex-warning "#FF8F8F")
       (tex-fold      "#D6F08F")
       (tex-fold-bg   "#30362A")

       ;; Rainbow delimiters =
       (rd-powder-blue   "#93a8c6")
       (rd-vanilla       "#dddda3")
       (rd-muted-teal    "#97bb98")
       (rd-bright-blue   "#aebed8")
       (rd-pale-slate    "#b0b0b3")
       (rd-muted-teal-2  "#90a890")
       (rd-powder-blue-2 "#a2b6da")
       (rd-lighter-green "#9cb6ad")

       ;; Composite org-level faces
       (ol1 `(:height 1.3 :weight bold :overline ,heading1-fg :foreground ,heading1-fg :background ,heading1-bg))
       (ol2 `(:height 1.0 :weight bold :overline ,heading2-fg :foreground ,heading2-fg :background ,heading2-bg))
       (ol3 `(:height 1.0 :weight bold :overline ,org3-fg :foreground ,org3-fg :background ,heading3-bg))
       (ol4 `(:height 1.0 :weight bold :slant normal :overline ,org4-fg :foreground ,org4-fg :background ,org4-bg))
       (ol5 `(:height 1.0 :weight bold :slant normal :foreground ,org5-fg))
       (ol6 `(:height 1.0 :weight bold :slant italic :foreground ,org6-fg))
       (ol7 `(:height 1.0 :weight bold :slant italic :foreground ,org7-fg))
       (ol8 `(:height 1.0 :weight bold :slant italic :foreground ,org8-fg)))
  (custom-theme-set-faces
   'wombat-cute
   `(default		((,class (:background ,bg :foreground ,fg))))
   `(tooltip		((,class (:inherit variable-pitch :background ,bg :foreground ,cream))))
   `(flymake-error	((,class (:underline (:style wave :color ,orange-wave)))))
   `(cursor		((,class (:background ,fg))))
   ;; Parenthesis
   `(show-paren-match		  ((,class (:background ,paren-bg :foreground ,white))))
   `(show-paren-match-expression  ((,class (:background ,paren-bg :foreground ,white))))
   `(show-paren-mismatch	  ((,class (:foreground ,white :background ,error-red))))
   ;; Highlighting faces
   `(fringe			((,class (:background ,bg))))
   `(highlight			((,class (:background ,bg-highlight :foreground ,white :underline nil))))
   `(region			((,class (:background ,bg-region))))
   `(secondary-selection	((,class (:background ,bg-secondary :foreground ,secondary-fg))))
   `(isearch			((,class (:background ,bg-modeline :foreground ,isearch-fg))))
   `(lazy-highlight		((,class (:background ,bg-lazy :foreground ,lazy-fg))))
   ;; Mode line faces
   `(mode-line		((,class (:background ,bg-modeline :foreground ,modeline-fg))))
   `(mode-line-inactive ((,class (:background ,bg-modeline-inactive :foreground ,modeline-inactive-fg))))
   ;; Escape and prompt faces
   `(minibuffer-prompt	((,class (:foreground ,coral))))
   `(escape-glyph	((,class (:foreground ,apricot :weight bold))))
   `(homoglyph		((,class (:foreground ,apricot :weight bold))))
   ;; Font lock faces
   `(font-lock-comment-face		 ((,class (:foreground ,comment-gray))))
   `(font-lock-comment-delimiter-face    ((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face			 ((,class (:inherit 'font-lock-comment-face))))
   `(font-lock-doc-markup-face           ((,class (:inherit font-lock-doc-face))))
   `(font-lock-function-call-face        ((,class (:foreground ,grass))))
   `(font-lock-function-name-face        ((,class (:foreground ,grass))))
   `(font-lock-type-face                 ((,class (:foreground ,grass))))
   `(font-lock-preprocessor-face         ((,class (:foreground ,ash-gray))))
   `(font-lock-keyword-face              ((,class (:foreground ,sky))))
   `(font-lock-negation-char-face        ((,class (:foreground ,pink))))
   `(font-lock-string-face               ((,class (:foreground ,peach))))
   `(font-lock-regexp-face               ((,class (:foreground ,light-moss))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,light-apricot))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,light-apricot))))
   `(font-lock-builtin-face              ((,class (:foreground ,wisteria))))
   ;; Font lock defaults
   `(font-lock-bracket-face              ((,class (:inherit default))))
   `(font-lock-constant-face             ((,class (:inherit default))))
   `(font-lock-delimiter-face            ((,class (:inherit default))))
   `(font-lock-escape-face               ((,class (:inherit default))))
   `(font-lock-misc-punctuation-face     ((,class (:inherit default))))
   `(font-lock-number-face               ((,class (:inherit default))))
   `(font-lock-operator-face             ((,class (:inherit default))))
   `(font-lock-property-name-face        ((,class (:inherit default))))
   `(font-lock-property-use-face         ((,class (:inherit default))))
   `(font-lock-punctuation-face          ((,class (:inherit default))))
   `(font-lock-variable-name-face        ((,class (:inherit default))))
   `(font-lock-variable-use-face         ((,class (:inherit default))))
   `(font-lock-warning-face              ((,class (:inherit default))))
   ;; Help faces
   `(help-key-binding ((,class (:background ,bg-button :foreground ,fg))))
   ;; Button and link faces
   `(link		((,class (:foreground ,link-blue :underline t))))
   `(link-visited	((,class (:foreground ,coral :underline t))))
   `(button		((,class (:background ,bg-button :foreground ,fg))))
   `(header-line	((,class (:background ,bg-header :foreground ,header-fg))))
   ;; Gnus faces
   `(gnus-group-news-1		((,class (:weight bold :foreground ,green))))
   `(gnus-group-news-1-low	((,class (:foreground ,green))))
   `(gnus-group-news-2		((,class (:weight bold :foreground ,yellow-green))))
   `(gnus-group-news-2-low	((,class (:foreground ,yellow-green))))
   `(gnus-group-news-3		((,class (:weight bold :foreground ,tan))))
   `(gnus-group-news-3-low	((,class (:foreground ,tan))))
   `(gnus-group-news-4		((,class (:weight bold :foreground ,comment-gray))))
   `(gnus-group-news-4-low	((,class (:foreground ,comment-gray))))
   `(gnus-group-news-5		((,class (:weight bold :foreground ,yellow-green))))
   `(gnus-group-news-5-low	((,class (:foreground ,yellow-green))))
   `(gnus-group-news-low	((,class (:foreground ,comment-gray))))
   `(gnus-group-mail-1		((,class (:weight bold :foreground ,green))))
   `(gnus-group-mail-1-low	((,class (:foreground ,green))))
   `(gnus-group-mail-2		((,class (:weight bold :foreground ,yellow-green))))
   `(gnus-group-mail-2-low	((,class (:foreground ,yellow-green))))
   `(gnus-group-mail-3		((,class (:weight bold :foreground ,tan))))
   `(gnus-group-mail-3-low	((,class (:foreground ,tan))))
   `(gnus-group-mail-low	((,class (:foreground ,comment-gray))))
   `(gnus-header-content	((,class (:foreground ,link-blue))))
   `(gnus-header-from		((,class (:weight bold :foreground ,green))))
   `(gnus-header-subject	((,class (:foreground ,yellow-green))))
   `(gnus-header-name		((,class (:foreground ,link-blue))))
   `(gnus-header-newsgroups	((,class (:foreground ,yellow-green))))
   ;; Message faces
   `(message-header-name	((,class (:foreground ,link-blue :weight bold))))
   `(message-header-cc		((,class (:foreground ,green))))
   `(message-header-other	((,class (:foreground ,green))))
   `(message-header-subject	((,class (:foreground ,yellow-green))))
   `(message-header-to		((,class (:foreground ,yellow-green))))
   `(message-cited-text		((,class (:foreground ,comment-gray))))
   `(message-separator		((,class (:foreground ,coral :weight bold))))
   ;; ANSI colors
   `(ansi-color-black		((,class (:background ,bg :foreground ,bg))))
   `(ansi-color-red		((,class (:background ,ansi-red :foreground ,ansi-red))))
   `(ansi-color-green		((,class (:background ,ansi-green :foreground ,ansi-green))))
   `(ansi-color-yellow		((,class (:background ,tan :foreground ,tan))))
   `(ansi-color-blue		((,class (:background ,ansi-blue :foreground ,ansi-blue))))
   `(ansi-color-magenta		((,class (:background ,ansi-magenta :foreground ,ansi-magenta))))
   `(ansi-color-cyan		((,class (:background ,ansi-cyan :foreground ,ansi-cyan))))
   `(ansi-color-white		((,class (:background ,fg :foreground ,fg))))
   `(ansi-color-bright-black	((,class (:background ,gray-bright-black :foreground ,gray-bright-black))))
   `(ansi-color-bright-red	((,class (:background ,coral :foreground ,coral))))
   `(ansi-color-bright-green	((,class (:background ,green :foreground ,green))))
   `(ansi-color-bright-yellow	((,class (:background ,ansi-bright-yellow :foreground ,ansi-bright-yellow))))
   `(ansi-color-bright-blue	((,class (:background ,link-blue :foreground ,link-blue))))
   `(ansi-color-bright-magenta	((,class (:background ,ansi-bright-magenta :foreground ,ansi-bright-magenta))))
   `(ansi-color-bright-cyan	((,class (:background ,ansi-bright-cyan :foreground ,ansi-bright-cyan))))
   `(ansi-color-bright-white	((,class (:background ,white :foreground ,white))))
   ;; Org mode
   `(org-level-1 ((,class ,ol1)))
   `(org-level-2 ((,class ,ol2)))
   `(org-level-3 ((,class ,ol3)))
   `(org-level-4 ((,class ,ol4)))
   `(org-level-5 ((,class ,ol5)))
   `(org-level-6 ((,class ,ol6)))
   `(org-level-7 ((,class ,ol7)))
   `(org-level-8 ((,class ,ol8)))

   ;; Rainbow Delimiters
   `(rainbow-delimiters-base-error-face ((,class (:inherit show-paren-mismatch))))
   `(rainbow-delimiters-mismatched-face ((,class (:inherit show-paren-mismatch))))
   `(rainbow-delimiters-unmatched-face  ((,class (:inherit show-paren-mismatch))))

   `(rainbow-delimiters-base-face       ((,class (:foreground ,fg))))

   `(rainbow-delimiters-depth-1-face    ((,class (:foreground ,fg))))
   `(rainbow-delimiters-depth-2-face    ((,class (:foreground ,rd-powder-blue))))
   `(rainbow-delimiters-depth-3-face    ((,class (:foreground ,rd-vanilla))))
   `(rainbow-delimiters-depth-4-face    ((,class (:foreground ,rd-muted-teal))))
   `(rainbow-delimiters-depth-5-face    ((,class (:foreground ,rd-bright-blue))))
   `(rainbow-delimiters-depth-6-face    ((,class (:foreground ,rd-pale-slate))))
   `(rainbow-delimiters-depth-7-face    ((,class (:foreground ,rd-muted-teal-2))))
   `(rainbow-delimiters-depth-8-face    ((,class (:foreground ,rd-powder-blue-2))))
   `(rainbow-delimiters-depth-9-face    ((,class (:foreground ,rd-lighter-green))))

   ;; LaTeX faces (from AUCTex)
   `(font-latex-string-face	((,class (:foreground ,latex-string :inherit font-lock-string-face))))
   `(font-latex-italic-face	((,class (:foreground ,latex-string :slant italic))))
   `(font-latex-bold-face	((,class (:foreground ,latex-string :weight bold))))
   `(font-latex-underline-face	((,class (:foreground ,latex-string :underline t))))
   `(font-latex-math-face	((,class (:foreground ,latex-math))))
   `(font-latex-sedate-face	((,class (:foreground ,latex-sedate))))
   `(font-latex-warning-face    ((,class (:foreground ,latex-warning))))
   `(TeX-fold-folded-face       ((,class (:foreground ,tex-fold))))
   `(TeX-fold-unfolded-face     ((,class (:foreground ,tex-fold :background ,tex-fold-bg))))
   `(font-latex-verbatim-face	((,class (:inherit default :foreground ,latex5-fg :background ,bg-verbatim :extend t))))

   `(font-latex-sectioning-0-face ((,class (:inherit variable-pitch :height 2.0 :weight bold :foreground ,latex0-fg))))
   `(font-latex-sectioning-1-face ((,class (:inherit variable-pitch :height 1.8 :weight bold :foreground ,heading1-fg :overline ,heading1-fg :background ,heading1-bg))))
   `(font-latex-sectioning-2-face ((,class (:inherit variable-pitch :height 1.5 :weight bold :foreground ,heading2-fg :overline ,heading2-fg :background ,heading2-bg))))
   `(font-latex-sectioning-3-face ((,class (:inherit variable-pitch :height 1.2 :weight bold :foreground ,latex3-fg :overline ,latex3-fg :background ,heading3-bg))))
   `(font-latex-sectioning-4-face ((,class (:inherit variable-pitch :height 1.0 :weight bold :foreground ,latex4-fg :overline ,latex4-fg :background ,latex4-bg))))
   `(font-latex-sectioning-5-face ((,class (:weight bold :slant normal :foreground ,latex5-fg))))
   ))


(provide-theme 'wombat-cute)

;;; wombat-cute-theme.el ends here
