;;; init.el --- Personal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;                To conf, or not to conf? That is the question.

;;; Code:
;; ┌──────────────────────────────────────────────────────────────────────────┐
;; │ STORE AUTOMATIC CUSTOMISATION OPTIONS ELSEWHERE                          │
;; └──────────────────────────────────────────────────────────────────────────┘
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; ┌──────────────────────────────────────────────────────────────────────────┐
;; │ INITIALIZE PACKAGES                                                      │
;; └──────────────────────────────────────────────────────────────────────────┘
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.emacs.d/personal/config")
(add-to-list 'load-path "~/.emacs.d/personal/packages")

;; personal/packages:
;; (load "treesitter")
(load "code-analysis")
;; (load "formatter")
(load "languages")
(load "ligature")
(load "macos-path")
(load "rainbow-mode")
;; (load "rmsbolt")
;; completion-bundle.el
;; documentation.el
;; magit.el

;; personal/config
(load "common")
(load "keymap")
(load "dired")
(load "modus-themes-customs")
(load "windows-frames")
(load "fonts-themes")

(load "modeline")

(provide 'init)
;;; init.el ends here
