;;; init.el --- My Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; This file only decides what loads and in what order.  Everything else
;; lives in m/, one file per topic.
;;
;; Modules are `load'ed rather than `require'd: short feature names like `ui'
;; or `edit' would collide with real packages, and compile-angel hooks `load'
;; too, so byte-compilation still applies.

;;; Code:

(defun my/load (file)
  "Load FILE (basename, no extension) from the m/ directory."
  (load (expand-file-name (format "m/%s" file) user-emacs-directory)
        nil 'nomessage))

(mapc #'my/load
      '("pkg"      ; package archives, use-package, compile-angel — must be first
        "font"     ; font family selection
        "theme"    ; vendored themes, light/dark switching
        "ui"       ; startup, scrolling, fringes, line numbers
        "edit"     ; mark, selection, undo, backups, shell commands
        "persist"  ; savehist, saveplace, recentf, auto-revert
        "cmpl"     ; vertico, marginalia, which-key
        "mac"      ; modifiers, input methods, Finder
        "prog"     ; programming modes
        "lig"      ; ligature table
        "l"        ; LaTeX
	"emacs-solo-mode-line" ; mode-line
        "k"))      ; keybindings — last, so every command exists

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; init.el ends here
