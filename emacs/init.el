;;; Personal configuration -*- lexical-binding: t -*-

;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ INITIALIZE PACKAGES                                                     │
;; └─────────────────────────────────────────────────────────────────────────┘
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("gnu"	 . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ THEMES, FONTS AND VISUAL OPTIONS                                        │
;; └─────────────────────────────────────────────────────────────────────────┘
;; Default frame size
(if (display-graphic-p)
	(progn
	  (setq initial-frame-alist
			'(
			  (tool-bar-lines . 0)
			  (width . 100) ; chars
			  (height . 55) ; lines
		      ))
	  (setq default-frame-alist
			'(
			  (tool-bar-lines . 0)
			  (width . 100)
			  (height . 55))))
  (progn
	(setq initial-frame-alist '( (tool-bar-lines . 0)))
	(setq default-frame-alist '( (tool-bar-lines . 0)))))

;; Resize frames and windows by pixels, not by chars
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Smooth scrolling
(pixel-scroll-precision-mode t)

;; Enable mouse in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(blink-cursor-mode 0) ;; Stop cursor blinking

;; Enable column numbering in `prog-mode'
(add-hook 'prog-mode-hook #'column-number-mode)
;; Enable line numbering in `prog-mode'
(dolist (hook '(prog-mode-hook LaTeX-mode-hook toml-ts-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
;; do not decrease line number width, results in less shifting
(setq-default display-line-numbers-grow-only t)
;; set line number width to a maximum needed number (no shifting when scrolling)
(setq-default display-line-numbers-width-start t)

;; Do not wrap line by default, unless in specific modes
(setq-default truncate-lines t)
(dolist (hook '(prog-mode-hook LaTeX-mode-hook text-mode-hook toml-ts-mode-hook))
  (add-hook hook #'visual-line-mode))

;; "Adaptive prefix" - lines are aligned when visually wrapped
(use-package adaptive-wrap
  :ensure t
  :hook (prog-mode . adaptive-wrap-prefix-mode))

;; Simple and clean whitespace mode setup
(progn
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))
  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
		;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
		'(
		  (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
		  (newline-mark 10 [182 10]) ; LINE FEED,
		  (tab-mark 9 [9655 9] [92 9]) ; tab
		  )))

;; Simplified dired setup
(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t "Удалять буфер при переходе в другой каталог.")
  (dired-listing-switches "-lh --group-directories-first --dired")
  :hook
  (dired-mode . dired-hide-details-mode))

;; Different setup for truncating lines
;; (defun az-no-line-wrap ()
;;   (setq truncate-lines t))
;; (add-hook 'dired-mode-hook 'az-no-line-wrap)
;; (add-hook 'minibuffer-mode-hook 'az-no-line-wrap)

;; >> SET DEFAULT FONT FACE <<
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
	  (set-frame-font "Consolas 12" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "SF Mono" (font-family-list))
	  (set-frame-font "SF Mono 12" t t)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
	  (set-frame-font "DejaVu Sans Mono 12" t t))))

;; >> INSTALL CUSTOM THEMES <<
(use-package one-themes
  :ensure t)
(use-package nimbus-theme
  :ensure t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(use-package monokai-theme
  :ensure t)
(use-package standard-themes
  :ensure t)

;; >> AUTO-DARK <<
;; Package for syncinng themes with system
(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-light-theme 'standard-light)
  (setq auto-dark-dark-theme 'nimbus)
  (setq auto-dark-polling-interval-seconds 5)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

;; ┌────────────────────────────────────────────────────────────────────────┐
;; │ EDITOR OPTIONS                                                         │
;; └────────────────────────────────────────────────────────────────────────┘
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)


(tool-bar-mode -1)    ;; No toolbar
(scroll-bar-mode -1)  ;; No scroll bars
(context-menu-mode 1) ;; Enable right click menus

(setq create-lockfiles nil)  ;; stop creating #auto-save# files
(setq make-backup-files nil) ;; disable automatic backup~ file

(global-auto-revert-mode 1)  ;; auto revert/refresh file when change detected

;; Automatically pair parentheses
(dolist (hook '(prog-mode-hook LaTeX-mode-hook text-mode-hook toml-ts-mode-hook))
  (add-hook hook #'electric-pair-mode))

(electric-indent-mode 1) ;; Automatically indent

;; Guess major mode from file name
(setq-default major-mode
			  (lambda ()
				(unless buffer-file-name
				  (let ((buffer-file-name (buffer-name)))
					(set-auto-mode)))))

(recentf-mode t)    ;; Save file history

(savehist-mode t)   ;; Save command history

(save-place-mode t) ;; Save place in buffer

(setq use-short-answers t) ;; Use y-n instead of yes-no

(global-goto-address-mode t) ;; Make links clickable

(setq-default tab-width 2) ;; Set tab length = 2

(setq-default indent-tabs-mode nil) ;; Indent by spaces

(delete-selection-mode 1) ;; Paste over selected region

;; >> UNDO-TREE <<
;; much better undo setup for emacs
(use-package undo-tree
  :ensure t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)

;; >> RMSbolt <<
;; compiler output viewer
(use-package rmsbolt
    :ensure t)

;; >> MARGINALIA <<
;; Marginalia (documentation and notes) in the minibuffer
(use-package marginalia
  :ensure t)

;; >> WHICH-KEY <<
;; Reminds about key combinations
(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package avy
     :ensure t)   ;; enable avy for quick navigation

;; Второй язык ввода
(use-package russian-techwriter
  :ensure t)
(setq-default default-input-method 'russian-techwriter)


;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ PROGRAMMING LANGUAGES SETUP                                             │
;; └─────────────────────────────────────────────────────────────────────────┘
;;; Go Support
(use-package go-mode
  :ensure t)

;;; Haskell Support
(use-package haskell-mode
  :ensure t)

;;; JSON Support
(use-package json-mode
  :ensure t)

;;; Lua Support
(use-package lua-mode
  :ensure t)

;;; NASM Support
(use-package nasm-mode
  :ensure t)

;;; PHP Support
(use-package php-mode
  :ensure t)

;;; Rust Support
(use-package rust-mode
  :ensure t)

;;; Additional Lisp support
(use-package sly
  :ensure t)

;;; Swift Support
(use-package swift-mode
  :ensure t)

;;; Typescript Support
(use-package typescript-mode
  :ensure t)

;;; YAML Support
(use-package yaml-mode
  :ensure t)

;;; Markdown support
(use-package markdown-mode
  :ensure t)


;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ COMPLETION                                                              │
;; └─────────────────────────────────────────────────────────────────────────┘
;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable completion by narrowing
(vertico-mode t)

;;; Extended completion utilities
(unless (package-installed-p 'consult)
  (package-install 'consult))
(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c j") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;; Enable automatic completion.
(setq corfu-auto t)


;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ DIMINISH (HIDE) MODES FROM THE MODELINE                                 │
;; │ (has to be loaded after everything)                                     │
;; └─────────────────────────────────────────────────────────────────────────┘
(use-package diminish
  :ensure t)

(require 'diminish)

(diminish 'eldoc-mode)
(diminish 'visual-line-mode)
(diminish 'which-key-mode)
(diminish 'auto-dark-mode)
(diminish 'undo-tree-mode)


;; ┌──────────────────────────────────────────────────────────────────────┐
;; │ Store automatic customisation options elsewhere                      │
;; └──────────────────────────────────────────────────────────────────────┘
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
