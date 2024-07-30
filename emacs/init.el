;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.


(load "~/.emacs.d/lisp/general.el")
(load "~/.emacs.d/lisp/themes.el")
(load "~/.emacs.d/lisp/languages.el")
(load "~/.emacs.d/lisp/latex.el")
(load "~/.emacs.d/lisp/completion.el")
(load "~/.emacs.d/lisp/snippets.el")
(load "~/.emacs.d/lisp/git.el")

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
