;;; envy-colorful-theme.el --- Colorful light theme -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: ported from envy.nvim (Alexander Zakharov), originally vim-envy by
;;         Gadzhi Kharkharov.
;; URL: https://github.com/ogswag/envy.nvim
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0.0
;; Keywords: faces, themes
;; SPDX-License-Identifier: MPL-2.0

;;; Commentary:
;;
;; `envy-colorful' is the moderate-colour variant of the Envy light theme:
;; purple keywords, blue functions, cyan types, red constants, muted
;; (`#144E40') properties and parameters.  It shares its palette and entire UI
;; with `envy'; only syntax tokens differ.
;;
;; See `envy-themes.el' for the shared engine and the README for installation.

;;; Code:

;; Make the shared engine requirable even when this file is only on
;; `custom-theme-load-path' (and not on `load-path').
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'envy-themes)

(envy-themes-deftheme envy-colorful
  "Envy Colorful: a calm light theme with moderate extra syntax colour."
  colorful)

(provide 'envy-colorful-theme)
;;; envy-colorful-theme.el ends here
