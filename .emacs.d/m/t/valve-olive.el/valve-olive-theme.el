;;; valve-olive-theme.el --- Dark olive theme from the classic Steam skin -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: ported from valve-olive.nvim (Alexander Zakharov).
;; URL: https://github.com/ogswag/valve-olive.el
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0.0
;; Keywords: faces, themes
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; `valve-olive' is the standard-background variant: Normal is #3e4636, an
;; olive green lifted from the ~2010 Steam "vgui" skin.  Syntax is deliberately
;; minimal -- identifiers, variables, properties and operators stay in the base
;; foreground; only comments, strings, constants, keywords, functions and types
;; take a hue.
;;
;; It shares its palette, foregrounds and entire UI with `valve-olive-darker';
;; only the backgrounds differ.
;;
;; See `valve-olive-themes.el' for the shared engine and the README for
;; installation and the comment-contrast options.

;;; Code:

;; Make the shared engine requirable even when this file is only on
;; `custom-theme-load-path' (and not on `load-path').
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'valve-olive-themes)

(valve-olive-themes-deftheme valve-olive
  "Valve Olive: a dark olive-green theme from the classic Steam skin."
  default)

(provide 'valve-olive-theme)
;;; valve-olive-theme.el ends here
