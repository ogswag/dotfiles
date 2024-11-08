;;; steam-engine-colorized-theme.el --- Dark theme based on the first Steam client.

;; Copyright (c) 2024 Alexander Zakharov (GNU/GPL Licence)

;; Authors: Alexander Zakharov <apz_works@icloud.com>
;; URL: http://github.com/alee3x/steam-engine
;; Version: 1.0.0
;; Package-Requires: ((autothemer "0.2") (emacs "27.1"))

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
;;  Dark theme based on the first Steam client.

;;; Made to support GUI only.

;;; Code:

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(autothemer-deftheme
 steam-engine-colorized "ʕ•ᴥ•ʔ Dark theme loosely based on nord-like palette."
 ((((class color) (min-colors #xFFFFFF))) ;; GUI only

  ;; Define colors
  ;; bg
  (sten-bg          "#3F4537")
  (sten-bg-dim      "#282E22")
  (sten-bg-active   "#65765B")
  (sten-bg-inactive "#61645E")
  ;; fg
  (sten-fg          "#E0E0E0")
  (sten-fg-dim      "#9A9A9A")
  (sten-fg-dimmer   "#6A6A6A")
  (sten-fg-alt      "#D8E099")
  ;; other colors
  (sten-white   "#fff")
  (sten-black   "#000")
  (sten-red         "#FF0000")
  (sten-orange      "#FF692E")
  (sten-rust        "#DD8B20")
  (sten-yellow      "#D2C646")
  (sten-green       "#90B063")
  (sten-blue        "#0000B8")
  (sten-magenta     "#8B008B")
  (sten-pink        "#FFC0CB")
  )

 ;; Customize faces
 ((default        (:foreground sten-fg :background sten-bg))
  (shadow              (:foreground sten-fg-dim))
  (tooltip             (:foreground sten-fg :background sten-bg-active))
  (cursor         (:foreground sten-bg :background sten-white))
  (fringe         (:inherit 'default))
  (header-line    (:background sten-bg-active :foreground sten-fg))
  (highlight      (:foreground sten-fg :background sten-bg-active))
  (help-key-binding    (:inherit 'font-lock-comment-face))
  ;; isearch
  (isearch        (:foreground sten-bg :background sten-yellow))
  (isearch-fail   (:foreground sten-fg :background sten-red :weight 'bold))
  (lazy-highlight (:foreground sten-fg :background sten-blue))
  (match               (:foreground sten-black :background sten-yellow)) ;; Face used to highlight matches permanently.
  (query-replace       (:background sten-blue :foreground sten-fg))
  ;; buttons
  (button         (:box (:line-width -1 :style 'flat-button :color sten-bg-active) :foreground sten-fg :background sten-bg-active))
  (custom-button         (:box (:line-width 1 :style 'released-button :color sten-black) :foreground sten-fg :background sten-bg))
  (custom-button-mouse   (:box (:line-width 1 :style 'released-button :color sten-black) :foreground sten-fg :background sten-bg))
  (custom-button-pressed (:box (:line-width 1 :style 'pressed-button :color sten-black) :foreground sten-fg :background sten-bg-dim))
  ;; custom menu options
  (custom-state (:foreground sten-green))
  (custom-changed (:foreground sten-bg-dim :background sten-yellow))
  (custom-modified (:inherit 'custom-changed))
  (custom-themed   (:inherit 'custom-changed))
  (custom-set      (:inherit 'custom-changed))
  ;; custom variable states
  (custom-rogue    (:foreground sten-orange :background sten-bg-dim))
  (custom-invalid (:foreground sten-fg :background sten-orange))
  ;; custom tags
  (custom-group-tag (:inherit 'default :foreground sten-yellow :height 1.3))
  (custom-group-tag-1 (:inherit 'default :foreground sten-rust :height 1.3))
  (custom-variable-tag (:inherit 'default :foreground sten-rust))
  (custom-variable-obsolete (:inherit 'default :foreground sten-fg-dim))
  (custom-variable-button (:inherit 'button))
  ;; custom appearance of description-toggle button
  (custom-visibility (:inherit 'default :underline (:color sten-fg-dim :style 'line :position nil) :foreground sten-fg-dim))
  ;; links
  (link           (:underline (:color sten-yellow :style 'line :position nil) :foreground sten-yellow))
  (link-visited   (:underline (:color sten-rust :style 'line :position nil) :foreground sten-rust))
  ;; borders
  (border (:background sten-fg-dim :foreground sten-fg-dim))
  (vertical-border (:inherit 'border))
  (corfu-border (:inherit 'border))
  (child-frame-border (:inherit 'border))
  (window-divider (:inherit 'border))
  (window-divider-first-pixel (:inherit 'border))
  ;; minibuffer
  (minibuffer-prompt   (:foreground sten-yellow))
  ;; completions
  (completions-annotations (:foreground sten-fg-dim :background 'nil :slant 'normal))
  (completions-common-part (:foreground sten-yellow :background 'nil))
  (completions-first-difference (:foreground sten-fg-alt :background 'nil))
  (completions-group-title  (:foreground sten-orange :background 'nil :slant 'normal :height 1.1))
  ;; messages
  (error      (:foreground sten-orange))
  (success    (:foreground sten-green))
  (warning    (:foreground sten-yellow))
  (next-error (:foreground sten-orange))
  ;; selection
  (region                           (:foreground sten-black :background sten-fg-dim))
  (secondary-selection              (:foreground sten-fg :background sten-bg-dim))
  ;; line numbers
  (line-number                      (:inherit 'default :foreground sten-fg-dimmer))
  (line-number-current-line         (:inherit 'default :foreground sten-fg))
  ;; parenthesis match
  (show-paren-match                 (:foreground sten-bg :background sten-yellow))
  (show-paren-mismatch              (:foreground sten-bg :background sten-orange))
  ;; fill column mode
  (fill-column-indicator            (:inherit 'nil :foreground sten-bg-active :background sten-bg-active))
  ;; widgets
  (widget-field                     (:box (:line-width 1 :style 'pressed-button :color sten-black) :foreground sten-fg :background sten-bg-dim))
  (widget-single-line-field         (:box (:line-width 1 :style 'pressed-button :color sten-black) :foreground sten-fg :background sten-bg-dim))
  (widget-button-pressed            (:foreground sten-orange))
  (widget-documentation             (:foreground sten-green))

  ;; git
  (diff-error                       (:inherit 'bold :foreground sten-orange))

  ;; code parts
  ;; colored
  ;; comments
  (font-lock-comment-delimiter-face (:inherit 'font-lock-comment-face))
  (font-lock-comment-face           (:foreground sten-yellow))
  ;; fg-alt
  (font-lock-string-face   (:foreground sten-fg-alt))
  (font-lock-keyword-face  (:foreground sten-fg-alt))
  (font-lock-constant-face (:foreground sten-fg-alt))
  (font-lock-punctuation-face   (:foreground sten-fg-alt))
  ;; (font-lock-bracket-face  (:inherit 'nil :foreground sten-fg-alt)) ;; inherits punctuation-face
  ;; (font-lock-delimiter-face        (:inherit 'default)) ;; inherits punctuation-face
  ;; (font-lock-misc-punctuation-face (:inherit 'default)) ;; inherits punctuation-face
  (font-lock-number-face   (:foreground sten-fg-alt))
  (font-lock-operator-face (:foreground sten-fg-alt))
  ;; other colors
  (font-lock-warning-face       (:foreground sten-yellow))
  (font-lock-negation-char-face (:inherit 'error))
  (trailing-whitespace          (:background sten-red))
  (escape-glyph                 (:foreground sten-red))
  (homoglyph                    (:foreground sten-yellow))
  (font-lock-builtin-face       (:inherit 'default))
  (font-lock-preprocessor-face     (:inherit 'nil :foreground sten-green)) ;; inherits builtin-face
  ;; (font-lock-doc-face              (:inherit 'default)) ;; by default inherits string-face
  ;; (font-lock-doc-markup-face       (:inherit 'default)) ;; by default inherits constant-face
  (font-lock-regexp-grouping-backslash  (:inherit 'nil :foreground sten-magenta))
  (font-lock-escape-face        (:inherit 'nil :foreground sten-magenta)) ;; inherits regexp-grouping-backslash
  (font-lock-regexp-grouping-construct  (:inherit 'nil :foreground sten-magenta))
  ;; default colors
  (font-lock-function-name-face    (:inherit 'default))
  ;; (font-lock-function-call-face    (:inherit 'default)) ;; inherits function-name-face
  (font-lock-variable-name-face    (:inherit 'default))
  ;; (font-lock-variable-use-face     (:inherit 'default)) ;; inherits property-name-face
  ;; (font-lock-property-name-face    (:inherit 'default)) ;; inherits variable-name-face
  ;; (font-lock-property-use-face     (:inherit 'default)) ;; inherits property-name-face
  (font-lock-type-face             (:inherit 'default))


  ;; modeline
  (mode-line           (:box (:line-width 1 :style 'released-button :color sten-bg-dim) :foreground sten-fg :background sten-bg))
  (mode-line-buffer-id (:inherit 'bold))
  (mode-line-emphasis  (:foreground sten-yellow :inherit 'bold))
  (mode-line-highlight (:box (:line-width 1 :color sten-fg-dim :style 'flat-button) :foreground sten-fg :background sten-bg))
  (mode-line-inactive  (:box (:line-width 1 :style 'pressed-button :color sten-black) :foreground sten-fg-dim :background sten-black))

  ;; flymake
  (flymake-warning (:underline (:style 'wave :color sten-yellow)))
  (flymake-note    (:underline (:style 'wave :color sten-green)))
  (flymake-error   (:underline (:style 'wave :color sten-orange)))
  ;; flyspell
  (flyspell-incorrect (:inherit 'flymake-error))
  (flyspell-duplicate (:underline (:style 'wave :color sten-fg)))

  ;; orderless
  (orderless-match-face-0 (:foreground sten-yellow))
  (orderless-match-face-1 (:foreground sten-yellow))
  (orderless-match-face-2 (:foreground sten-rust))
  (orderless-match-face-3 (:foreground sten-rust))

  ;; corfu
  (corfu-default (:foreground sten-fg :background sten-black))
  (corfu-current (:foreground sten-fg :background sten-bg-active))
  (corfu-indexed (:foreground sten-fg :background sten-bg-dim))
  (corfu-bar     (:foreground sten-fg :background sten-yellow))

  ;; marginalia
  (marginalia-key (:inherit 'nil :foreground sten-yellow))
  (marginalia-version (:inherit 'nil :foreground sten-green))
  (marginalia-documentation (:slant 'normal :foreground sten-fg-dim))

  ;; eglot
  (eglot-diagnostic-tag-deprecated-face (:inherit 'default :foreground sten-fg :strike-through t))
  (eglot-diagnostic-tag-unnecessary-face (:inherit 'default :foreground sten-fg :slant 'italic))
  (eglot-inlay-hint-face (:foreground sten-bg :background sten-fg-dim :height 0.9))
  (eglot-highlight-symbol-face (:inherit 'default))
  (eglot-mode-line (:foreground sten-fg))

  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'steam-engine-colorized)

;;; steam-engine-colorized-theme.el ends here
