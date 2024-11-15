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

;; personal/packages:
;; (load "treesitter")
(load "~/.emacs.d/personal/packages/code-analysis.el")
(load "~/.emacs.d/personal/packages/formatter.el")
(load "~/.emacs.d/personal/packages/languages.el")
(load "~/.emacs.d/personal/packages/ligature.el")
(load "~/.emacs.d/personal/packages/macos-path.el")
(load "~/.emacs.d/personal/packages/rainbow-mode.el")
(load "~/.emacs.d/personal/packages/rmsbolt.el")
(load "~/.emacs.d/personal/packages/completion-bundle.el")
;; completion-bundle.el
;; documentation.el
;; magit.el

;; personal/config
(load "~/.emacs.d/personal/config/common.el")
(load "~/.emacs.d/personal/config/project-management.el")
(load "~/.emacs.d/personal/config/keymap.el")
(load "~/.emacs.d/personal/config/dired.el")
(load "~/.emacs.d/personal/config/modus-themes-customs.el")
(load "~/.emacs.d/personal/config/windows-frames.el")
(load "~/.emacs.d/personal/config/fonts-themes.el")

(load "~/.emacs.d/personal/config/modeline.el")

(provide 'init)
;;; init.el ends here
