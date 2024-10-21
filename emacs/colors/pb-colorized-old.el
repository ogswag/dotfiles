;;; polar-bear-colorized-theme.el --- ʕ•ᴥ•ʔ The same as polar-bear theme, but with more color.

;; Copyright (c) 2024 Alexander Zakharov (GNU/GPL Licence)

;; Authors: Alexander Zakharov <apz_works@icloud.com>
;; URL: http://github.com/alee3x/polar-bear
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
;;  ʕ•ᴥ•ʔ The same as polar-bear theme, but with more color.

;;; Made to support GUI only.

;;; Code:

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(autothemer-deftheme
 polar-bear-colorized "ʕ•ᴥ•ʔ The same as polar-bear theme, but with more color."

 ((((class color) (min-colors #xFFFFFF))) ;; GUI only

  ;; Define colors
  ;; bg
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
  (polar-rust        "#D6926B")
  (polar-yellow      "#EBCB8B")
  (polar-olive       "#93B371")
  (polar-green       "#7EB891")
  (polar-sea-green   "#8FC7BA")
  (polar-blue        "#72A0CE")
  (polar-cyan        "#7BC6D1")
  (polar-magenta     "#B77CCB")
  (polar-pink        "#D0B3CF")
  )

 ;; Customize faces
 ((default        (:foreground polar-fg :background polar-bg))
  (cursor         (:foreground polar-bg :background polar-white))
  (fringe         (:inherit 'default))
  (header-line    (:background polar-bg-dim :foreground polar-fg-dim))
  (highlight      (:foreground polar-fg :background polar-bg-active))
  (help-key-binding    (:inherit 'font-lock-comment-face))

  (isearch        (:foreground polar-bg :background polar-blue))
  (isearch-fail   (:foreground polar-red :background 'nil :weight 'bold))
  (lazy-highlight (:foreground polar-bg :background polar-magenta))


  (button         (:box (:line-width -1 :style 'flat-button :color polar-bg-active) :foreground polar-fg :background polar-bg-active))
  (custom-button         (:box (:line-width -1 :style 'flat-button :color polar-bg-active) :foreground polar-fg :background polar-bg-active))
  (custom-button-mouse   (:box (:line-width -1 :style 'flat-button :color polar-bg-active) :foreground polar-fg :background polar-bg-active))
  (custom-button-pressed (:box (:line-width -1 :style 'pressed-button :color polar-bg-active) :foreground polar-fg :background polar-bg-dim))

  (custom-state (:foreground polar-green))
  (custom-changed (:foreground polar-bg-darker :background polar-blue))
  (custom-modified (:inherit 'custom-changed))
  (custom-themed   (:inherit 'custom-changed))
  (custom-set      (:inherit 'custom-changed))

  (custom-rogue    (:foreground polar-red :background polar-bg-darker))
  (custom-invalid (:foreground polar-fg :background polar-red))

  (custom-group-tag (:inherit 'default :foreground polar-blue :height 1.3))
  (custom-group-tag-1 (:inherit 'default :foreground polar-pink :height 1.3))
  (custom-variable-tag (:inherit 'default :foreground polar-magenta))
  (custom-variable-obsolete (:inherit 'default :foreground polar-fg-dim))
  (custom-variable-button (:inherit 'button))

  (custom-visibility (:inherit 'default :underline (:color polar-fg-dim :style 'line :position nil) :foreground polar-fg-dim))

  (link           (:underline (:color polar-blue :style 'line :position nil) :foreground polar-blue))
  (link-visited   (:underline (:color polar-magenta :style 'line :position nil) :foreground polar-magenta))

  (border (:background polar-bg-active :foreground polar-bg-active))
  (vertical-border (:inherit 'border))
  (corfu-border (:inherit 'border))
  (child-frame-border (:inherit 'border))
  (window-divider (:inherit 'border))
  (window-divider-first-pixel (:inherit 'border))

  (error (:foreground polar-red))

  (match               (:foreground polar-bg :background polar-blue))

  (minibuffer-prompt   (:foreground polar-cyan))

  (next-error          (:foreground polar-red))

  (query-replace       (:background polar-red :foreground polar-bg))

  (region              (:foreground polar-fg :background polar-bg-active))
  (secondary-selection (:foreground polar-fg :background polar-bg-dim))

  (success             (:foreground polar-green))
  (warning             (:foreground polar-yellow))
  (shadow              (:foreground polar-fg-dim))

  (diff-error (:inherit 'bold :foreground polar-red))

  (tooltip             (:foreground polar-fg :background polar-bg-active))

  (line-number (:inherit 'default :foreground polar-fg-dimmer))
  (line-number-current-line (:inherit 'default :foreground polar-fg))

  (show-paren-match (:foreground polar-bg :background polar-cyan))
  (show-paren-mismatch (:foreground polar-bg :background polar-red))

  (fill-column-indicator (:inherit 'nil :foreground polar-bg-active :background polar-bg-active))

  ;; code parts
  ;; colored
  ;;comments
  (font-lock-comment-face           (:foreground polar-cyan))
  (font-lock-comment-delimiter-face (:inherit 'font-lock-comment-face))
  ;; fg-alt
  (font-lock-string-face   (:foreground polar-fg-alt))
  (font-lock-keyword-face  (:foreground polar-fg-alt))
  (font-lock-constant-face (:foreground polar-fg-alt))
  (font-lock-punctuation-face   (:foreground polar-fg-alt))
  ;; (font-lock-bracket-face  (:inherit 'nil :foreground polar-fg-alt)) ;; inherits punctuation-face
  ;; (font-lock-delimiter-face        (:inherit 'default)) ;; inherits punctuation-face
  ;; (font-lock-misc-punctuation-face (:inherit 'default)) ;; inherits punctuation-face
  (font-lock-number-face   (:foreground polar-fg-alt))
  (font-lock-operator-face (:foreground polar-fg-alt))
  ;; other colors
  (font-lock-warning-face       (:foreground polar-yellow))
  (font-lock-negation-char-face (:inherit 'error))
  (trailing-whitespace          (:background polar-red))
  (escape-glyph                 (:foreground polar-red))
  (homoglyph                    (:foreground polar-yellow))
  (font-lock-builtin-face       (:inherit 'default))
  (font-lock-preprocessor-face     (:inherit 'nil :foreground polar-sea-green)) ;; inherits builtin-face
  ;; (font-lock-doc-face              (:inherit 'default)) ;; by default inherits string-face
  ;; (font-lock-doc-markup-face       (:inherit 'default)) ;; by default inherits constant-face
  (font-lock-regexp-grouping-backslash  (:inherit 'nil :foreground polar-magenta))
  (font-lock-escape-face        (:inherit 'nil :foreground polar-magenta)) ;; inherits regexp-grouping-backslash
  (font-lock-regexp-grouping-construct  (:inherit 'nil :foreground polar-magenta))
  ;; default colors
  (font-lock-function-name-face    (:inherit 'default))
  ;; (font-lock-function-call-face    (:inherit 'default)) ;; inherits function-name-face
  (font-lock-variable-name-face    (:inherit 'default))
  ;; (font-lock-variable-use-face     (:inherit 'default)) ;; inherits property-name-face
  ;; (font-lock-property-name-face    (:inherit 'default)) ;; inherits variable-name-face
  ;; (font-lock-property-use-face     (:inherit 'default)) ;; inherits property-name-face
  (font-lock-type-face             (:inherit 'default))

  ;; modeline
  (mode-line           (:box (:line-width -1 :style 'flat-button) :foreground polar-fg :background polar-bg-active))
  (mode-line-buffer-id (:inherit 'bold))
  (mode-line-emphasis  (:foreground polar-cyan :inherit 'bold))
  (mode-line-highlight (:box (:line-width 1 :color polar-white :style 'flat-button) :foreground polar-white :background polar-bg-active))
  (mode-line-inactive  (:box (:line-width -1 :style 'flat-button) :foreground polar-fg-dim :background polar-bg))

  ;; widgets
  (widget-field             (:box (:line-width 1 :style 'flat-button :color polar-bg-inactive) :foreground polar-fg :background polar-bg-darker))
  (widget-single-line-field (:box (:line-width -1 :style 'flat-button :color polar-bg-inactive) :foreground polar-fg :background polar-bg-darker))
  (widget-button-pressed    (:foreground polar-red))
  (widget-documentation     (:foreground polar-green))

  ;; flymake
  (flymake-warning (:underline (:style 'wave :color polar-blue)))
  (flymake-note    (:underline (:style 'wave :color polar-green)))
  (flymake-error   (:underline (:style 'wave :color polar-red) ))

  (flyspell-incorrect (:inherit 'flymake-error))
  (flyspell-duplicate (:underline (:style 'wave :color polar-yellow)))


  ;; orderless
  (orderless-match-face-0 (:foreground polar-cyan))
  (orderless-match-face-1 (:foreground polar-blue))
  (orderless-match-face-2 (:foreground polar-magenta))
  (orderless-match-face-3 (:foreground polar-pink))


  ;; corfu
  (corfu-default (:foreground polar-fg :background polar-bg))
  (corfu-current (:foreground polar-fg :background polar-bg-active))
  (corfu-indexed (:foreground polar-fg :background polar-bg-dim))
  (corfu-bar     (:foreground polar-fg :background polar-bg-inactive))

  ;; marginalia
  (marginalia-key (:inherit 'nil :foreground polar-cyan))
  (marginalia-version (:inherit 'nil :foreground polar-green))

  ;; eglot
  (eglot-diagnostic-tag-deprecated-face (:inherit 'default :foreground polar-fg :strike-through t))
  (eglot-diagnostic-tag-unnecessary-face (:inherit 'default :foreground polar-fg :slant 'italic))
  (eglot-inlay-hint-face (:foreground polar-bg :background polar-fg-dim :height 0.9))
  (eglot-highlight-symbol-face (:inherit 'default))
  (eglot-mode-line (:foreground polar-fg))

  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'polar-bear-colorized)

;;; polar-bear-colorized-theme.el ends here
