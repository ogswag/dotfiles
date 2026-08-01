;;; valve-olive-darker-theme.el --- Darker-background Valve Olive -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: ported from valve-olive.nvim (Alexander Zakharov).
;; URL: https://github.com/ogswag/valve-olive.el
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0.0
;; Keywords: faces, themes
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; `valve-olive-darker' is the deeper variant: the same olive hue shifted
;; darker, with Normal at #313829 instead of #3e4636.  It corresponds to the
;; `darker_bg = true' option of the Neovim colorscheme.
;;
;; Foregrounds and accents are identical to `valve-olive' -- they simply
;; contrast more strongly against these backgrounds.
;;
;; See `valve-olive-themes.el' for the shared engine and the README for
;; installation and the comment-contrast options.

;;; Code:

;; Make the shared engine requirable even when this file is only on
;; `custom-theme-load-path' (and not on `load-path').
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'valve-olive-themes)

(valve-olive-themes-deftheme valve-olive-darker
  "Valve Olive: the same olive hue with deeper backgrounds."
  darker)

(provide 'valve-olive-darker-theme)
;;; valve-olive-darker-theme.el ends here
