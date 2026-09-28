;;; init.el --- My Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; load order. modules live in m/, loaded not required (short names collide).

;;; Code:

(defvar my-config-directory user-emacs-directory
  "The real config directory: init.el, early-init.el, and m/ live here.")

(defvar my-vendor-directory (expand-file-name "m/p" my-config-directory)
  "Vendored and local single-file packages, put on `load-path' by their users.")

(defun my/load (file)
  "Load FILE (basename, no extension) from the m/ directory."
  (load (expand-file-name (format "m/%s" file) my-config-directory)
        nil 'nomessage))

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name "local" my-config-directory)))
(make-directory user-emacs-directory t)

(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory)
      package-quickstart-file
      (expand-file-name "package-quickstart.el" user-emacs-directory)
      auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      abbrev-file-name
      (expand-file-name "abbrev_defs" user-emacs-directory)
      custom-file
      (expand-file-name "custom.el" user-emacs-directory)
      ;; Where `customize-create-theme' writes m/theme.el puts the vendored themes in m/t on.
      custom-theme-directory
      (expand-file-name "themes/" user-emacs-directory))
(when (boundp 'user-lisp-directory)
  (setq user-lisp-directory
        (expand-file-name "user-lisp/" user-emacs-directory)))
(when (and (fboundp 'startup-redirect-eln-cache)
           (boundp 'native-comp-eln-load-path))
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" user-emacs-directory)))

(mapc #'my/load
      '("pkg"           ; package archives, use-package, compile-angel - must be first
        "font"          ; font family selection
        "theme"         ; vendored themes, light/dark switching
        "ui"            ; startup, scrolling, fringes, line numbers
        "edit"          ; mark, selection, undo, backups, shell commands
        "persist"       ; savehist, saveplace, recentf, auto-revert
        "cmpl"          ; vertico, corfu, cape, tempel, nerd-icons, which-key
        "mac"           ; modifiers, input methods, Finder
        "prog"          ; programming modes, flymake, parens, structural editing
        "proj"          ; project.el: discovery, the known-project list
        "fmt"           ; apheleia - format on save, mirrors conform.nvim
        "lig"           ; ligature table
        "math"          ; maths detection, macro face, the $ cycle - before l
        "l"             ; TeX: modes, building, viewing
        "tex-symbols"   ; C-c s symbol picker, C-c w surround - after l
        "tbl"           ; C-c t org tables -> \table / \pmatrix / tabular
        "spell"         ; jinx
        "org"           ; org-mode config
        "markdown"      ; markdown-mode related configs
        "k"))           ; keybindings - last, so every command exists

(load custom-file 'noerror 'nomessage)

;;; init.el ends here
