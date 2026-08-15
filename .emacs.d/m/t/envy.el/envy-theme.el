;;; envy-theme.el --- Hyperlegible theme -*- no-byte-compile: t; lexical-binding: t; -*-

;; URL: https://github.com/ogswag/envy.nvim
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0.0
;; Keywords: faces, themes
;; SPDX-License-Identifier: MPL-2.0

;;; Commentary:
;;
;; `envy'

;;; Code:

;; Make the shared engine requirable even when this file is only on
;; `custom-theme-load-path' (and not on `load-path').
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'envy-themes)

(envy-themes-deftheme envy
  "Envy: a calm, minimal, near-monochrome light theme."
  envy)

(provide 'envy-theme)
;;; envy-theme.el ends here
