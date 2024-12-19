;;; polar-bear-theme.el --- ʕ•ᴥ•ʔ Dark theme loosely based on nord-like palette. -*- lexical-binding: t -*-

;; Copyright (c) 2024 Alexander Zakharov (GNU/GPL Licence)

;; Authors: Alexander Zakharov <apz_works@icloud.com>
;; URL: http://github.com/alee3x/polar-bear
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  Dark theme loosely based on nord-like palette.
;;  Made to support GUI only.

;;; Code:

(deftheme polar-bear
  "ʕ•ᴥ•ʔ Dark theme loosely based on nord-like palette.")

(let ((class '((class color) (min-colors 89)))
      (polar-bg          "#2E3440")
      (polar-bg-darker   "#252A33")
      (polar-bg-dim      "#3B4352")
      (polar-bg-active   "#4C566A")
      (polar-bg-inactive "#66738D")
      ;; fg
      (polar-fg          "#D8DEE9")
      (polar-fg-dim      "#8B95AB")
      (polar-fg-dimmer   "#5D6880")
      (polar-fg-alt      "#A1C0E6")
      ;; other colors
      (polar-white   "#fff")
      (polar-black   "#000")
      (polar-red         "#D5616D")
      ;; (polar-rust        "#D6926B")
      (polar-yellow      "#EBCB8B")
      ;; (polar-olive       "#93B371")
      (polar-green       "#7EB891")
      ;; (polar-sea-green   "#8FC7BA")
      (polar-blue        "#72A0CE")
      (polar-cyan        "#7BC6D1")
      (polar-magenta     "#B77CCB")
      (polar-pink        "#D0B3CF"))
  (custom-theme-set-faces
   'polar-bear

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; BUILTIN FUNCTIONS AND PACKAGES ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   `(default ((,class :foreground ,polar-fg :background ,polar-bg)))
   `(shadow  ((,class :foreground ,polar-fg-dim)))
   `(tooltip ((,class :foreground ,polar-fg :background ,polar-bg-active)))
   `(cursor  ((,class :foreground ,polar-bg :background ,polar-white)))
   `(fringe  ((,class :inherit default)))
   `(highlight   ((,class :foreground ,polar-fg :background ,polar-bg-active)))
   `(header-line ((,class :background ,polar-bg-dim :foreground ,polar-fg-dim)))
   `(help-key-binding    ((,class :inherit font-lock-comment-face)))
   `(trailing-whitespace ((,class :foreground ,polar-bg :background ,polar-red)))

   ;; search and replace
   `(isearch        ((,class :foreground ,polar-bg :background ,polar-blue)))
   `(isearch-fail   ((,class :foreground ,polar-red :background nil :inherit bold)))
   `(lazy-highlight ((,class :foreground ,polar-bg :background ,polar-magenta)))
   `(match          ((,class :foreground ,polar-bg :background ,polar-blue)))
   `(query-replace  ((,class :background ,polar-red :foreground ,polar-bg)))

   ;; buttons
   `(button        ((,class :box (:line-width -1 :style flat-button :color ,polar-bg-active) :foreground ,polar-fg :background ,polar-bg-active)))
   `(custom-button ((,class :box (:line-width -1 :style flat-button :color ,polar-bg-active) :foreground ,polar-fg :background ,polar-bg-active)))
   `(custom-button-mouse   ((,class :box (:line-width -1 :style flat-button :color ,polar-bg-active) :foreground ,polar-fg :background ,polar-bg-active)))
   `(custom-button-pressed ((,class :box (:line-width -1 :style pressed-button :color ,polar-bg-active) :foreground ,polar-fg :background ,polar-bg-dim)))

   ;; custom menu options
   `(custom-state ((,class :foreground ,polar-green)))
   `(custom-changed ((,class :foreground ,polar-bg-darker :background ,polar-blue)))
   `(custom-modified ((,class :inherit custom-changed)))
   `(custom-themed   ((,class :inherit custom-changed)))
   `(custom-set      ((,class :inherit custom-changed)))

   ;; custom variable states
   `(custom-rogue    ((,class :foreground ,polar-red :background ,polar-bg-darker)))
   `(custom-invalid ((,class :foreground ,polar-fg :background ,polar-red)))

   ;; custom tags
   `(custom-group-tag         ((,class :inherit default :foreground ,polar-blue :height 1.3)))
   `(custom-group-tag-1       ((,class :inherit default :foreground ,polar-pink :height 1.3)))
   `(custom-variable-tag      ((,class :inherit default :foreground ,polar-magenta)))
   `(custom-variable-obsolete ((,class :inherit default :foreground ,polar-fg-dim)))
   `(custom-variable-button   ((,class :inherit button)))

   ;; custom appearance of description-toggle button
   `(custom-visibility ((,class :inherit default :underline (:color ,polar-fg-dim :style line :position nil) :foreground ,polar-fg-dim)))

   ;; links
   `(link           ((,class :underline (:color ,polar-blue :style line :position nil) :foreground ,polar-blue)))
   `(link-visited   ((,class :underline (:color ,polar-magenta :style line :position nil) :foreground ,polar-magenta)))

   ;; borders
   `(border             ((,class :background ,polar-bg-active :foreground ,polar-bg-active)))
   `(vertical-border    ((,class :inherit border)))
   `(corfu-border       ((,class :inherit border)))
   `(child-frame-border ((,class :inherit border)))
   `(window-divider     ((,class :inherit border)))
   `(window-divider-first-pixel ((,class :inherit border)))

   ;; messages
   `(error      ((,class :foreground ,polar-red)))
   `(next-error ((,class :foreground ,polar-red)))
   `(success    ((,class :foreground ,polar-green)))
   `(warning    ((,class :foreground ,polar-yellow)))

   ;; minibuffer
   `(minibuffer-prompt   ((,class :foreground ,polar-cyan)))

   ;; selection
   `(region              ((,class :foreground ,polar-bg :background ,polar-cyan)))
   `(secondary-selection ((,class :foreground ,polar-fg :background ,polar-bg-active)))

   ;; diff
   `(diff-added ((,class :foreground ,polar-bg :background ,polar-green)))

   `(diff-error ((,class :inherit bold :foreground ,polar-red)))

   `(diff-file-header  ((,class :foreground ,polar-fg :background ,polar-bg-active)))
   `(diff-function     ((,class :foreground ,polar-fg :background ,polar-bg-dim)))
   `(diff-header       ((,class :foreground ,polar-fg :background ,polar-bg-dim)))
   `(diff-hunk-header  ((,class :foreground ,polar-fg :background ,polar-bg-dim)))
   `(diff-index        ((,class :foreground ,polar-fg :background ,polar-bg-active)))

   `(diff-indicator-added ((,class :foreground ,polar-green :background ,polar-bg-active)))
   `(diff-indicator-changed ((,class :foreground ,polar-yellow :background ,polar-bg-active)))
   `(diff-indicator-removed ((,class :foreground ,polar-red :background ,polar-bg-active)))

   `(diff-nonexistent ((,class :foreground ,polar-white :background ,polar-bg-inactive)))

   `(diff-refine-added ((,class :background ,polar-green :foreground ,polar-bg)))
   `(diff-refine-changed ((,class :background ,polar-yellow :foreground ,polar-bg)))
   `(diff-refine-removed ((,class :background ,polar-red :foreground ,polar-white)))

   ;; line numbers
   `(line-number              ((,class :inherit default :foreground ,polar-fg-dimmer)))
   `(line-number-current-line ((,class :inherit default :foreground ,polar-fg)))

   ;; parenthesis match
   `(show-paren-match    ((,class :foreground ,polar-bg :background ,polar-cyan)))
   `(show-paren-mismatch ((,class :foreground ,polar-bg :background ,polar-red)))

   ;; fill column mode
   `(fill-column-indicator ((,class :inherit nil :foreground ,polar-bg-active :background ,polar-bg-active)))

   ;; modeline
   `(mode-line           ((,class :box (:line-width -1 :style flat-button) :foreground ,polar-fg :background ,polar-bg-active)))
   `(mode-line-buffer-id ((,class :inherit bold)))
   `(mode-line-emphasis  ((,class :foreground ,polar-cyan :inherit bold)))
   `(mode-line-highlight ((,class :box (:line-width 1 :color ,polar-white :style flat-button) :foreground ,polar-white :background ,polar-bg-active)))
   `(mode-line-inactive  ((,class :box (:line-width -1 :style flat-button) :foreground ,polar-fg-dim :background ,polar-bg)))

   ;; widgets
   `(widget-field             ((,class :box (:line-width 1 :style flat-button :color ,polar-bg-inactive) :foreground ,polar-fg :background ,polar-bg-darker)))
   `(widget-single-line-field ((,class :box (:line-width -1 :style flat-button :color ,polar-bg-inactive) :foreground ,polar-fg :background ,polar-bg-darker)))
   `(widget-button-pressed    ((,class :foreground ,polar-red)))
   `(widget-documentation     ((,class :foreground ,polar-green)))

   ;; completions
   `(completions-annotations ((,class :foreground ,polar-fg-dim :background nil :slant normal)))
   `(completions-common-part ((,class :foreground ,polar-cyan :background nil)))
   `(completions-group-title ((,class :foreground ,polar-magenta :background nil :slant normal :height 1.1)))
   `(completions-first-difference ((,class :foreground ,polar-fg :background nil)))

   ;; flymake
   `(flymake-warning ((,class :underline (:style wave :color ,polar-blue))))
   `(flymake-note    ((,class :underline (:style wave :color ,polar-green))))
   `(flymake-error   ((,class :underline (:style wave :color ,polar-red))))

   `(flyspell-incorrect ((,class :inherit flymake-error)))
   `(flyspell-duplicate ((,class :underline (:style wave :color ,polar-yellow))))

   ;; eglot
   `(eglot-diagnostic-tag-deprecated-face  ((,class :background nil :foreground ,polar-fg :strike-through t)))
   `(eglot-diagnostic-tag-unnecessary-face ((,class :background nil :foreground ,polar-fg :slant italic)))
   `(eglot-inlay-hint-face       ((,class :foreground ,polar-bg :background ,polar-fg-dim :height 0.9)))
   `(eglot-highlight-symbol-face ((,class :background nil :foreground ,polar-magenta)))
   `(eglot-mode-line             ((,class :foreground ,polar-fg)))

   ;; tab-bar
   `(tab-bar     ((,class :background ,polar-bg-darker :foreground ,polar-fg-dim)))
   `(tab-bar-tab ((,class :background ,polar-bg :foreground ,polar-fg)))

   `(tab-bar-tab-group-current  ((,class :background ,polar-bg :foreground ,polar-blue)))
   `(tab-bar-tab-group-inactive ((,class :background ,polar-bg-dim :foreground ,polar-fg-dim)))

   `(tab-bar-tab-inactive   ((,class :box (:line-width 1 :color ,polar-fg-dim :style flat-button) :background ,polar-bg-darker :foreground ,polar-fg-dim)))
   `(tab-bar-tab-ungrouped  ((,class :background ,polar-bg :foreground ,polar-fg-dim)))

   ;; tab-line
   `(tab-line           ((,class :background ,polar-bg-darker :foreground ,polar-fg-dim)))
   `(tab-line-highlight ((,class :background ,polar-bg-dim :foreground ,polar-white)))
   ;;
   `(tab-line-tab         ((,class :background ,polar-bg :foreground ,polar-fg)))
   `(tab-line-tab-current ((,class :background ,polar-bg :foreground ,polar-fg)))

   `(tab-line-tab-group  ((,class :background ,polar-bg-dim :foreground ,polar-blue)))

   `(tab-line-tab-inactive   ((,class :box (:line-width 1 :color ,polar-fg-dim :style flat-button) :background ,polar-bg-darker :foreground ,polar-fg-dim)))
   `(tab-line-tab-inactive-alternate   ((,class :box (:line-width 1 :color ,polar-fg-dim :style flat-button) :background ,polar-bg-darker :foreground ,polar-fg-dim)))

   `(tab-line-tab-modified  ((,class :background ,polar-bg :foreground ,polar-fg :slant italic)))
   `(tab-line-tab-special  ((,class :background ,polar-bg :foreground ,polar-yellow :slant normal)))

   ;; tool-bar
   `(tool-bar ((,class :background ,polar-bg :foreground ,polar-fg :inherit nil)))

   ;; transient
   `(transient-disabled-suffix ((,class :background ,polar-red :foreground ,polar-black :inherit nil)))
   `(transient-enabled-suffix  ((,class :background ,polar-green :foreground ,polar-black :inherit nil)))
   `(transient-key-noop         ((,class :foreground ,polar-fg-dim)))

   ;; shr
   `(shr-mark ((,class :background ,polar-yellow :foreground ,polar-black :inherit nil)))
   `(shr-selected-link ((,class :background ,polar-red :foreground ,polar-black :inherit nil)))

   ;; dired
   `(dired-broken-symlink ((,class :foreground ,polar-red :background nil :inherit nil)))

   ;; which-function
   `(which-func ((,class :background nil :foreground ,polar-blue :inherit nil)))

   ;; cua-mode
   `(cua-global-mark ((,class :background ,polar-yellow :foreground ,polar-black :inherit nil)))
   `(cua-rectangle ((,class :background ,polar-red :foreground ,polar-black :inherit nil)))


   ;; CODE PARTS

   ;; colored
   `(font-lock-comment-delimiter-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-comment-face           ((,class :foreground ,polar-cyan)))
   `(font-lock-string-face            ((,class :foreground ,polar-fg-alt)))
   `(font-lock-constant-face          ((,class :foreground ,polar-fg-alt)))
   ;; `(font-lock-doc-face              ((,class :background nil :foreground ,polar-fg)))
   ;; `(font-lock-doc-markup-face       ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-warning-face           ((,class :foreground ,polar-yellow)))
   `(font-lock-negation-char-face     ((,class :inherit error)))
   `(trailing-whitespace              ((,class :background ,polar-red)))
   `(escape-glyph                     ((,class :foreground ,polar-magenta)))
   `(font-lock-escape-face            ((,class :foreground ,polar-magenta)))
   `(font-lock-keyword-face           ((,class :foreground ,polar-fg)))
   `(homoglyph                        ((,class :foreground ,polar-yellow)))
   `(font-lock-regexp-grouping-backslash ((,class :foreground ,polar-magenta)))
   `(font-lock-regexp-grouping-construct ((,class :foreground ,polar-magenta)))
   `(font-lock-regexp-face               ((,class :foreground ,polar-magenta)))
   `(markdown-highlight-face             ((,class :background ,polar-yellow :foreground ,polar-black)))

   ;; not colored
   `(font-lock-bracket-face          ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-builtin-face          ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-delimiter-face        ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-function-call-face    ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-function-name-face    ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-misc-punctuation-face ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-number-face           ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-operator-face         ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-preprocessor-face     ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-property-name-face    ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-property-use-face     ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-punctuation-face      ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-variable-name-face    ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-variable-use-face     ((,class :background nil :foreground ,polar-fg)))
   `(font-lock-type-face             ((,class :background nil :foreground ,polar-fg)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; OTHER PACKAGES (not builtin) ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; orderless
   `(orderless-match-face-0 ((,class :foreground ,polar-cyan)))
   `(orderless-match-face-1 ((,class :foreground ,polar-blue)))
   `(orderless-match-face-2 ((,class :foreground ,polar-magenta)))
   `(orderless-match-face-3 ((,class :foreground ,polar-pink)))

   ;; corfu
   `(corfu-default ((,class :foreground ,polar-fg :background ,polar-bg)))
   `(corfu-current ((,class :foreground ,polar-fg :background ,polar-bg-active)))
   `(corfu-indexed ((,class :foreground ,polar-fg :background ,polar-bg-dim)))
   `(corfu-bar     ((,class :foreground ,polar-fg :background ,polar-bg-inactive)))

   ;; marginalia
   `(marginalia-key ((,class :inherit nil :foreground ,polar-cyan)))
   `(marginalia-version ((,class :inherit nil :foreground ,polar-green)))

   ;; company
   `(company-echo ((,class :foreground ,polar-fg :background nil)))
   `(company-echo-common ((,class :foreground ,polar-red :background nil)))
   `(company-preview ((,class :foreground ,polar-fg :background ,polar-bg-dim)))
   `(company-preview-common ((,class :foreground ,polar-blue :background nil)))
   `(company-preview-search ((,class :foreground ,polar-blue :background nil)))
   `(company-template-field ((,class :foreground ,polar-yellow :background ,polar-bg-dim)))
   `(company-tooltip ((,class :foreground ,polar-fg :background ,polar-bg-darker)))
   `(company-tooltip ((,class :foreground ,polar-fg :background ,polar-bg-darker)))
   `(company-tooltip-annotation             ((,class :foreground ,polar-fg-dim :background)))
   `(company-tooltip-annotation-selection   ((,class :foreground ,polar-fg-dim :background)))
   `(company-tooltip-common                 ((,class :foreground ,polar-cyan :background)))
   `(company-tooltip-common-selection       ((,class :foreground ,polar-cyan :background)))
   `(company-tooltip-deprecated             ((,class :foreground ,polar-fg :strike-through t :background nil)))
   `(company-tooltip-mouse                  ((,class :foreground ,polar-fg :background ,polar-bg-active)))
   `(company-tooltip-quick-access           ((,class :foreground ,polar-fg-dim :background ,polar-bg)))
   `(company-tooltip-quick-access-selection ((,class :foreground ,polar-fg-dim :background ,polar-bg)))
   `(company-tooltip-scrollbar-thumb        ((,class :foreground ,polar-fg :background ,polar-bg-inactive)))
   `(company-tooltip-scrollbar-track        ((,class :foreground ,polar-fg :background ,polar-bg-dim)))
   `(company-tooltip-search                 ((,class :foreground ,polar-fg :background ,polar-bg-active)))
   `(company-tooltip-search-selection       ((,class :foreground ,polar-fg :background ,polar-bg-active)))
   `(company-tooltip-selection              ((,class :foreground ,polar-fg :background ,polar-bg-dim)))

   ;; avy
   `(avy-lead-face ((,class :background ,polar-red :foreground ,polar-white)))
   `(avy-lead-face-0 ((,class :background ,polar-blue :foreground ,polar-white)))
   `(avy-lead-face-1 ((,class :background ,polar-green :foreground ,polar-white)))
   `(avy-lead-face-2 ((,class :background ,polar-magenta :foreground ,polar-white)))

   ;; undo-tree
   `(undo-tree-visualizer-current-face      ((,class :foreground ,polar-red)))
   `(undo-tree-visualizer-default-face      ((,class :foreground ,polar-fg)))
   `(undo-tree-visualizer-register-face     ((,class :foreground ,polar-yellow)))
   `(undo-tree-visualizer-unmodified-face   ((,class :foreground ,polar-cyan)))
   ))

(provide-theme 'polar-bear)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'polar-bear-theme)

;;; polar-bear-theme.el ends here
