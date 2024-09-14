;;; Personal configuration -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZE PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)
(require 'package)
(add-to-list 'package-archives '("gnu"	 . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (use-package quelpa
;;   :ensure t)
;; 
;; (use-package quelpa-use-package
;;   :ensure t)

(load "~/.emacs.d/lisp/general.el")
(load "~/.emacs.d/lisp/themes.el")
(load "~/.emacs.d/lisp/languages.el")
(load "~/.emacs.d/lisp/keys-layout.el")
(load "~/.emacs.d/lisp/latex.el")

(require 'diminish)

(diminish 'company-mode)
(diminish 'jinx-mode)
(diminish 'eldoc-mode)
(diminish 'visual-line-mode)
(diminish 'xah-fly-keys)
(diminish 'helm-mode)
(diminish 'which-key-mode)
(diminish 'auto-dark-mode)
(diminish 'undo-tree-mode)
(diminish 'yas/minor-mode)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
