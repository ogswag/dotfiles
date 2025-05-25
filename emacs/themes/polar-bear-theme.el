;;; polar-bear-theme.el --- ʕ•ᴥ•ʔ Dark theme with arctic colors. -*- lexical-binding: t -*-

;; Copyright (c) 2025 Alexander Zakharov (GNU/GPL Licence)

;; Authors: Alexander Zakharov <alexz1243421@gmail.com>
;; URL: http://github.com/alexsacharow/polar-bear
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
;;

;;; Code:

(defcustom polar-bear-operator-color "#ddd"
  "Foreground color for operators in the Polar Bear theme."
  :type 'string
  :group 'polar-bear-theme)

(defcustom polar-bear-delimiter-color "#ddd"
  "Foreground color for delimiters in the Polar Bear theme."
  :type 'string
  :group 'polar-bear-theme)

(defcustom polar-bear-rainbow-delimiters-style 'subtle
  "Rainbow delimiters style for the Polar Bear theme.
   Possible values are 'strong (vivid colors) or 'subtle (muted theme colors)."
  :type '(choice (const strong) (const subtle))
  :group 'polar-bear-theme)

(deftheme polar-bear
  "ʕ•ᴥ•ʔ Dark theme with arctic colors.")

(let (
      ;; bg
      (pb-bg          "#141417")
      (pb-bg-darker   "#040407")
      (pb-bg-dim      "#2E2E2f")
      (pb-bg-active   "#5B5B5d")
      (pb-bg-inactive "#808083")
      (pb-bg-modeline "#4A5759")
      ;; fg
      (pb-fg          "#ddd")
      (pb-fg-dim      "#989898")
      (pb-fg-dimmer   "#515151")
      (pb-fg-alt      "#CBE8F2")
      (pb-fg-modeline "#E6ffff")
      ;; colors
      (pb-white  "#FFF")
      (pb-black  "#000")
      (pb-red    "brown2")
      (pb-yellow "#E0CF79")
      (pb-green  "#81C7AA")
      (pb-blue   "#7D9CD2")
      (pb-lblue  "#EFE3C2")
      (pb-mint   "#008a5e")
      (pb-cyan   "#8BB3B3")
      (pb-violet "#8788AC"))
  (when (eq polar-bear-rainbow-delimiters-style 'subtle)
    (setq pb-rainbow-1 pb-cyan)
    (setq pb-rainbow-2 pb-violet)
    (setq pb-rainbow-3 pb-mint)
    (setq pb-rainbow-4 pb-blue)
    (setq pb-rainbow-5 pb-cyan)
    (setq pb-rainbow-6 pb-green)
    (setq pb-rainbow-7 pb-yellow)
    (setq pb-rainbow-8 pb-red)
    (setq pb-rainbow-9 pb-fg-dim)
    (setq pb-rainbow-red pb-red))
  (when (eq polar-bear-rainbow-delimiters-style 'strong)
    (setq pb-rainbow-1   "RoyalBlue1")
    (setq pb-rainbow-2   "gold1")
    (setq pb-rainbow-3   "chartreuse2")
    (setq pb-rainbow-4   "RoyalBlue1")
    (setq pb-rainbow-5   "gold1")
    (setq pb-rainbow-6   "chartreuse2")
    (setq pb-rainbow-7   "RoyalBlue1")
    (setq pb-rainbow-8   "gold1")
    (setq pb-rainbow-9   "chartreuse2")
    (setq pb-rainbow-red "#brown2"))

  (custom-theme-set-faces
   'polar-bear

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; BUILTIN FUNCTIONS AND PACKAGES ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   `(default ((t :foreground ,pb-fg :background ,pb-bg)))
   `(shadow  ((t :foreground ,pb-fg-dim)))
   `(tooltip ((t :foreground ,pb-fg :background ,pb-bg-active)))
   `(cursor  ((t :foreground ,pb-bg :background ,pb-white)))
   `(fringe  ((t :inherit default)))
   `(highlight   ((t :foreground nil :background ,pb-bg-dim)))
   `(header-line ((t :background ,pb-bg-dim :foreground ,pb-fg-dim)))
   `(help-key-binding    ((t :inherit font-lock-comment-face)))
   `(trailing-whitespace ((t :foreground ,pb-bg :background ,pb-red)))

   ;; search and replace
   `(isearch        ((t :foreground ,pb-bg :background ,pb-blue)))
   `(isearch-fail   ((t :foreground ,pb-red :background nil :inherit bold)))
   `(lazy-highlight ((t :foreground ,pb-bg :background ,pb-violet)))
   `(match          ((t :foreground ,pb-bg :background ,pb-blue)))
   `(query-replace  ((t :background ,pb-red :foreground ,pb-bg)))

   ;; buttons
   `(button        ((t :box (:line-width -1 :style flat-button :color ,pb-bg-active) :foreground ,pb-fg :background ,pb-bg-active)))
   `(custom-button ((t :box (:line-width -1 :style flat-button :color ,pb-bg-active) :foreground ,pb-fg :background ,pb-bg-active)))
   `(custom-button-mouse   ((t :box (:line-width -1 :style flat-button :color ,pb-bg-active) :foreground ,pb-fg :background ,pb-bg-active)))
   `(custom-button-pressed ((t :box (:line-width -1 :style pressed-button :color ,pb-bg-active) :foreground ,pb-fg :background ,pb-bg-dim)))

   ;; custom menu options
   `(custom-state ((t :foreground ,pb-green)))
   `(custom-changed ((t :foreground ,pb-bg-darker :background ,pb-blue)))
   `(custom-modified ((t :inherit custom-changed)))
   `(custom-themed   ((t :inherit custom-changed)))
   `(custom-set      ((t :inherit custom-changed)))

   ;; custom variable states
   `(custom-rogue    ((t :foreground ,pb-red :background ,pb-bg-darker)))
   `(custom-invalid ((t :foreground ,pb-fg :background ,pb-red)))

   ;; custom tags
   `(custom-group-tag         ((t :inherit default :foreground ,pb-blue :height 1.3)))
   `(custom-group-tag-1       ((t :inherit default :foreground ,pb-violet :height 1.3)))
   `(custom-variable-tag      ((t :inherit default :foreground ,pb-violet)))
   `(custom-variable-obsolete ((t :inherit default :foreground ,pb-fg-dim)))
   `(custom-variable-button   ((t :inherit button)))

   ;; custom appearance of description-toggle button
   `(custom-visibility ((t :inherit default :underline (:color ,pb-fg-dim :style line :position nil) :foreground ,pb-fg-dim)))

   ;; links
   `(link           ((t :underline (:color ,pb-blue :style line :position nil) :foreground ,pb-blue)))
   `(link-visited   ((t :underline (:color ,pb-violet :style line :position nil) :foreground ,pb-violet)))

   ;; borders
   `(border             ((t :background ,pb-bg-active :foreground ,pb-bg-active)))
   `(vertical-border    ((t :inherit border)))
   `(corfu-border       ((t :inherit border)))
   `(child-frame-border ((t :inherit border)))
   `(window-divider     ((t :inherit border)))
   `(window-divider-first-pixel ((t :inherit border)))

   ;; messages
   `(error      ((t :foreground ,pb-red)))
   `(next-error ((t :foreground ,pb-red)))
   `(success    ((t :foreground ,pb-green)))
   `(warning    ((t :foreground ,pb-yellow)))

   ;; minibuffer
   `(minibuffer-prompt   ((t :foreground ,pb-cyan)))

   ;; selection
   `(region              ((t :foreground ,pb-bg :background ,pb-cyan)))
   `(secondary-selection ((t :foreground ,pb-fg :background ,pb-bg-active)))

   ;; diff
   `(diff-added ((t :foreground ,pb-bg :background ,pb-green)))

   `(diff-error ((t :inherit bold :foreground ,pb-red)))

   `(diff-file-header  ((t :foreground ,pb-fg :background ,pb-bg-active)))
   `(diff-function     ((t :foreground ,pb-fg :background ,pb-bg-dim)))
   `(diff-header       ((t :foreground ,pb-fg :background ,pb-bg-dim)))
   `(diff-hunk-header  ((t :foreground ,pb-fg :background ,pb-bg-dim)))
   `(diff-index        ((t :foreground ,pb-fg :background ,pb-bg-active)))

   `(diff-indicator-added ((t :foreground ,pb-green :background ,pb-bg-active)))
   `(diff-indicator-changed ((t :foreground ,pb-yellow :background ,pb-bg-active)))
   `(diff-indicator-removed ((t :foreground ,pb-red :background ,pb-bg-active)))

   `(diff-nonexistent ((t :foreground ,pb-white :background ,pb-bg-inactive)))

   `(diff-refine-added ((t :background ,pb-green :foreground ,pb-bg)))
   `(diff-refine-changed ((t :background ,pb-yellow :foreground ,pb-bg)))
   `(diff-refine-removed ((t :background ,pb-red :foreground ,pb-white)))

   ;; line numbers
   `(line-number              ((t :inherit default :foreground ,pb-fg-dimmer)))
   `(line-number-current-line ((t :inherit default :foreground ,pb-fg)))

   ;; parenthesis match
   `(show-paren-match    ((t :foreground ,pb-bg :background ,pb-cyan)))
   `(show-paren-mismatch ((t :foreground ,pb-bg :background ,pb-red)))

   ;; fill column mode
   `(fill-column-indicator ((t :inherit nil :foreground ,pb-bg-dim :background ,pb-bg-dim)))

   ;; modeline
   `(mode-line           ((t :box (:line-width -1 :style flat-button) :foreground ,pb-white :background ,pb-bg-modeline)))
   `(mode-line-inactive  ((t :box (:line-width -1 :style flat-button) :foreground ,pb-fg-dim :background ,pb-black)))
   `(mode-line-buffer-id ((t :inherit bold :foreground ,pb-fg-modeline)))
   `(mode-line-emphasis  ((t :foreground ,pb-cyan :inherit bold)))
   `(mode-line-highlight ((t :box (:line-width 1 :color ,pb-white :style flat-button) :inherit mode-line)))

   ;; widgets
   `(widget-field             ((t :box (:line-width 1 :style flat-button :color ,pb-bg-inactive) :foreground ,pb-fg :background ,pb-bg-darker)))
   `(widget-single-line-field ((t :box (:line-width -1 :style flat-button :color ,pb-bg-inactive) :foreground ,pb-fg :background ,pb-bg-darker)))
   `(widget-button-pressed    ((t :foreground ,pb-red)))
   `(widget-documentation     ((t :foreground ,pb-green)))

   ;; completions
   `(completions-annotations ((t :foreground ,pb-fg-dim :background nil :slant normal)))
   `(completions-common-part ((t :foreground ,pb-cyan :background nil)))
   `(completions-group-title ((t :foreground ,pb-violet :background nil :slant normal :height 1.1)))
   `(completions-first-difference ((t :foreground ,pb-fg :background nil)))

   ;; flymake
   `(flymake-warning ((t :underline (:style wave :color ,pb-blue))))
   `(flymake-note    ((t :underline (:style wave :color ,pb-green))))
   `(flymake-error   ((t :underline (:style wave :color ,pb-red))))

   `(flyspell-incorrect ((t :inherit flymake-error)))
   `(flyspell-duplicate ((t :underline (:style wave :color ,pb-yellow))))

   ;; eglot
   `(eglot-diagnostic-tag-deprecated-face  ((t :background nil :foreground ,pb-fg :strike-through t)))
   `(eglot-diagnostic-tag-unnecessary-face ((t :background nil :foreground ,pb-fg :slant italic)))

   `(eglot-highlight-symbol-face           ((t :inherit default :foreground ,pb-yellow)))
   `(eglot-code-action-indicator-face      ((t :inherit default :foreground ,pb-yellow)))

   `(eglot-mode-line                       ((t :foreground ,pb-fg)))

   `(eglot-inlay-hint-face                 ((t :height 0.8 :background ,pb-bg-dim :foreground ,pb-fg-dim)))
   `(eglot-parameter-hint-face             ((t :height 0.8 :background ,pb-bg-dim :foreground ,pb-fg-dim)))
   `(eglot-type-hint-face                  ((t :height 0.8 :background ,pb-bg-dim :foreground ,pb-fg-dim)))



   ;; menu and tty-menu
   `(menu     ((t :foreground ,pb-fg :background ,pb-bg-modeline)))
   `(tty-menu-disabled-face ((t :foreground ,pb-fg-dimmer :background ,pb-bg-dim)))
   `(tty-menu-enabled-face ((t :foreground ,pb-fg-dim :background ,pb-bg-dim)))
   `(tty-menu-selected-face ((t :foreground ,pb-fg :background ,pb-bg-active)))


   ;; tab-bar
   `(tab-bar     ((t :background ,pb-bg-darker :foreground ,pb-fg-dim)))
   `(tab-bar-tab ((t :background ,pb-bg :foreground ,pb-fg)))

   `(tab-bar-tab-group-current  ((t :background ,pb-bg :foreground ,pb-blue)))
   `(tab-bar-tab-group-inactive ((t :background ,pb-bg-dim :foreground ,pb-fg-dim)))

   `(tab-bar-tab-inactive   ((t :box (:line-width -1 :color ,pb-fg-dim :style flat-button) :background ,pb-bg-darker :foreground ,pb-fg-dim)))
   `(tab-bar-tab-ungrouped  ((t :background ,pb-bg :foreground ,pb-fg-dim)))

   ;; tab-line
   `(tab-line           ((t :background ,pb-bg-darker :foreground ,pb-fg-dim)))
   `(tab-line-highlight ((t :background ,pb-bg-dim :foreground ,pb-white)))

   `(tab-line-tab         ((t :background ,pb-bg :foreground ,pb-fg)))
   `(tab-line-tab-current ((t :background ,pb-bg :foreground ,pb-fg)))

   `(tab-line-tab-group  ((t :background ,pb-bg-dim :foreground ,pb-blue)))

   `(tab-line-tab-inactive   ((t :box (:line-width -1 :color ,pb-fg-dim :style flat-button) :background ,pb-bg-darker :foreground ,pb-fg-dim)))
   `(tab-line-tab-inactive-alternate   ((t :box (:line-width -1 :color ,pb-fg-dim :style flat-button) :background ,pb-bg-darker :foreground ,pb-fg-dim)))

   `(tab-line-tab-modified ((t :background ,pb-bg :foreground ,pb-fg :slant italic)))
   `(tab-line-tab-special  ((t :background ,pb-bg :foreground ,pb-yellow :slant normal)))

   ;; tool-bar
   `(tool-bar ((t :background ,pb-bg :foreground ,pb-fg :inherit nil)))

   ;; transient
   `(transient-disabled-suffix ((t :background ,pb-red :foreground ,pb-black :inherit nil)))
   `(transient-enabled-suffix  ((t :background ,pb-green :foreground ,pb-black :inherit nil)))
   `(transient-key-noop        ((t :foreground ,pb-fg-dim)))

   ;; shr
   `(shr-mark ((t :background ,pb-yellow :foreground ,pb-black :inherit nil)))
   `(shr-selected-link ((t :background ,pb-red :foreground ,pb-black :inherit nil)))

   ;; dired
   `(dired-broken-symlink ((t :foreground ,pb-red :background nil :inherit nil)))

   ;; which-function
   `(which-func ((t :background nil :foreground ,pb-blue :inherit nil)))

   ;; cua-mode
   `(cua-global-mark ((t :background ,pb-yellow :foreground ,pb-black :inherit nil)))
   `(cua-rectangle   ((t :background ,pb-red :foreground ,pb-black :inherit nil)))


   ;; CODE PARTS
   `(font-lock-comment-delimiter-face    ((t :inherit font-lock-comment-face)))
   `(font-lock-comment-face              ((t :foreground ,pb-cyan)))
   `(font-lock-string-face               ((t :foreground ,pb-green)))
   `(font-lock-constant-face             ((t :foreground ,pb-blue)))
   `(font-lock-doc-face                  ((t :inherit font-lock-comment-face)))
   `(font-lock-doc-markup-face           ((t :inherit font-lock-comment-face)))
   `(font-lock-warning-face              ((t :foreground ,pb-red)))
   `(font-lock-negation-char-face        ((t :inherit error)))
   `(trailing-whitespace                 ((t :background ,pb-red)))
   `(escape-glyph                        ((t :foreground ,pb-violet)))
   `(font-lock-escape-face               ((t :foreground ,pb-violet)))

   `(font-lock-keyword-face              ((t :foreground ,pb-blue)))

   `(homoglyph                           ((t :foreground ,pb-yellow)))

   `(font-lock-regexp-grouping-backslash ((t :foreground ,pb-violet)))
   `(font-lock-regexp-grouping-construct ((t :foreground ,pb-violet)))
   `(font-lock-regexp-face               ((t :foreground ,pb-violet)))
   `(markdown-highlight-face             ((t :background ,pb-yellow :foreground ,pb-black)))

   `(font-lock-bracket-face          ((t :background nil :foreground ,pb-fg)))
   `(font-lock-builtin-face          ((t :background nil :foreground ,pb-fg-alt)))

   `(font-lock-delimiter-face        ((t :background nil :foreground ,polar-bear-delimiter-color)))
   `(font-lock-operator-face         ((t :background nil :foreground ,polar-bear-operator-color)))

   `(font-lock-function-name-face    ((t :background nil :foreground ,pb-lblue)))
   `(font-lock-function-call-face    ((t :background nil :foreground ,pb-lblue)))

   `(font-lock-number-face           ((t :background nil :foreground ,pb-blue)))

   `(font-lock-preprocessor-face     ((t :background nil :foreground ,pb-fg-alt)))

   `(font-lock-property-name-face    ((t :background nil :foreground ,pb-cyan)))
   `(font-lock-property-use-face     ((t :background nil :foreground ,pb-cyan)))

   `(font-lock-misc-punctuation-face ((t :background nil :foreground ,pb-fg)))
   `(font-lock-punctuation-face      ((t :background nil :foreground ,pb-fg)))

   `(font-lock-type-face             ((t :background nil :foreground ,pb-fg-alt)))

   `(font-lock-variable-name-face    ((t :background nil :foreground ,pb-fg)))
   `(font-lock-variable-use-face     ((t :background nil :foreground ,pb-fg)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; OTHER PACKAGES (not builtin) ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; orderless
   `(orderless-match-face-0 ((t :foreground ,pb-blue)))
   `(orderless-match-face-1 ((t :foreground ,pb-yellow)))
   `(orderless-match-face-2 ((t :foreground ,pb-green)))
   `(orderless-match-face-3 ((t :foreground ,pb-violet)))

   ;; corfu
   `(corfu-default ((t :foreground ,pb-fg :background ,pb-bg)))
   `(corfu-current ((t :foreground ,pb-fg :background ,pb-bg-dim)))
   `(corfu-indexed ((t :foreground ,pb-fg :background ,pb-bg-dim)))
   `(corfu-bar     ((t :foreground ,pb-fg :background ,pb-bg-inactive)))

   ;; marginalia
   `(marginalia-key ((t :inherit nil :foreground ,pb-cyan)))
   `(marginalia-version ((t :inherit nil :foreground ,pb-green)))

   ;; company
   `(company-echo ((t :foreground ,pb-fg :background nil)))
   `(company-echo-common ((t :foreground ,pb-red :background nil)))
   `(company-preview ((t :foreground ,pb-fg :background ,pb-bg-dim)))
   `(company-preview-common ((t :foreground ,pb-blue :background nil)))
   `(company-preview-bluerch ((t :foreground ,pb-blue :background nil)))
   `(company-template-field ((t :foreground ,pb-yellow :background ,pb-bg-dim)))
   `(company-tooltip ((t :foreground ,pb-fg :background ,pb-bg-darker)))
   `(company-tooltip ((t :foreground ,pb-fg :background ,pb-bg-darker)))
   `(company-tooltip-annotation             ((t :foreground ,pb-fg-dim :background)))
   `(company-tooltip-annotation-selection   ((t :foreground ,pb-fg-dim :background)))
   `(company-tooltip-common                 ((t :foreground ,pb-cyan :background)))
   `(company-tooltip-common-selection       ((t :foreground ,pb-cyan :background)))
   `(company-tooltip-deprecated             ((t :foreground ,pb-fg :strike-through t :background nil)))
   `(company-tooltip-mouse                  ((t :foreground ,pb-fg :background ,pb-bg-active)))
   `(company-tooltip-quick-access           ((t :foreground ,pb-fg-dim :background ,pb-bg)))
   `(company-tooltip-quick-access-selection ((t :foreground ,pb-fg-dim :background ,pb-bg)))
   `(company-tooltip-scrollbar-thumb        ((t :foreground ,pb-fg :background ,pb-bg-inactive)))
   `(company-tooltip-scrollbar-track        ((t :foreground ,pb-fg :background ,pb-bg-dim)))
   `(company-tooltip-bluerch                 ((t :foreground ,pb-fg :background ,pb-bg-active)))
   `(company-tooltip-bluerch-selection       ((t :foreground ,pb-fg :background ,pb-bg-active)))
   `(company-tooltip-selection              ((t :foreground ,pb-fg :background ,pb-bg-dim)))

   ;; avy
   `(avy-lead-face   ((t :inherit bold :background ,pb-red     :foreground ,pb-black)))
   `(avy-lead-face-0 ((t :inherit bold :background ,pb-blue    :foreground ,pb-black)))
   `(avy-lead-face-1 ((t :inherit bold :background ,pb-green   :foreground ,pb-black)))
   `(avy-lead-face-2 ((t :inherit bold :background ,pb-violet :foreground ,pb-black)))

   ;; undo-tree
   `(undo-tree-visualizer-current-face      ((t :foreground ,pb-red)))
   `(undo-tree-visualizer-default-face      ((t :foreground ,pb-fg)))
   `(undo-tree-visualizer-register-face     ((t :foreground ,pb-yellow)))
   `(undo-tree-visualizer-unmodified-face   ((t :foreground ,pb-cyan)))


   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face   ((t :foreground ,pb-rainbow-1)))
   `(rainbow-delimiters-depth-2-face   ((t :foreground ,pb-rainbow-2)))
   `(rainbow-delimiters-depth-3-face   ((t :foreground ,pb-rainbow-3)))
   `(rainbow-delimiters-depth-4-face   ((t :foreground ,pb-rainbow-4)))
   `(rainbow-delimiters-depth-5-face   ((t :foreground ,pb-rainbow-5)))
   `(rainbow-delimiters-depth-6-face   ((t :foreground ,pb-rainbow-6)))
   `(rainbow-delimiters-depth-7-face   ((t :foreground ,pb-rainbow-7)))
   `(rainbow-delimiters-depth-8-face   ((t :foreground ,pb-rainbow-8)))
   `(rainbow-delimiters-depth-9-face   ((t :foreground ,pb-rainbow-9)))
   `(rainbow-delimiters-unmatched-face ((t :underline (:style wave :colors ,pb-rainbow-red) :foreground ,pb-rainbow-red :weight bold)))))


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'polar-bear-theme)

;;; polar-bear-theme.el ends here
