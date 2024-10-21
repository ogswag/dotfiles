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
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ┌──────────────────────────────────────────────────────────────────────────┐
;; │ STOP EMACS LITTERING                                                     │
;; └──────────────────────────────────────────────────────────────────────────┘
(setq
 make-backup-files nil  ;; disable automatic backup~ file
 auto-save-default nil
 create-lockfiles nil)  ;; stop creating #auto-save# files

;; ┌──────────────────────────────────────────────────────────────────────────┐
;; │ THEMES, FONTS AND GENERAL OPTIONS                                        │
;; └──────────────────────────────────────────────────────────────────────────┘
;; Default frame size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 100) ; chars
              (height . 45) ; lines
              ))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 100)
              (height . 45))))
  (progn
    (setq initial-frame-alist '((tool-bar-lines . 0)))
    (setq default-frame-alist '((tool-bar-lines . 0)))))

(when (window-system)
  (tool-bar-mode -1)    ;; No toolbar
  (scroll-bar-mode -1)  ;; No scroll bars
  (context-menu-mode 1)) ;; Enable right click menus

;; >> DIMINISH <<
;; Hide modes from the mode line
;; (has to be installed before everything to work)
(use-package diminish
  :ensure t)

(use-package ns-auto-titlebar
  :ensure t)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; Resize frames and windows by pixels, not by chars
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; Always ensure new line at the end of the file
(setq require-final-newline t)

;; Enable mouse in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(blink-cursor-mode 1) ;; Stop (or don't) cursor blinking
(setq-default cursor-type 'box) ;; Set cursor shape

;; Enable column numbering in `prog-mode'
(add-hook 'prog-mode-hook #'column-number-mode)

;; Enable line numbering for a few major modes
;; (dolist (hook '(prog-mode-hook LaTeX-mode-hook toml-ts-mode-hook yaml-mode-hook))
;; (add-hook hook #'display-line-numbers-mode))
;; do not decrease line number width, results in less shifting
(setq-default display-line-numbers-grow-only t)
;; set line number width to a maximum needed number (no shifting when scrolling)
(setq-default display-line-numbers-width-start t)

;; Do not wrap line by default, unless in specific modes
(setq-default truncate-lines t)
(use-package visual-line
  :diminish visual-line-mode
  :hook (LaTeX-mode toml-ts-mode))

;; Allow right-left scrolling with mouse
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

;; Smooth scrolling
(pixel-scroll-precision-mode t)

;; >>ADAPTIVE WRAP<<
;; Enables "adaptive prefix" - lines maintain alignment position when visually
;; wrapped
(use-package adaptive-wrap
  :ensure t
  :hook ((prog-mode text-mode) . adaptive-wrap-prefix-mode))

;; Simple and clean whitespace mode setup
(progn
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [182 10])
          (tab-mark 9 [9655 9] [92 9])
          )))

;; No sounds in emacs
(setq-default ring-bell-function 'ignore)

;; Simplified dired setup
(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :hook
  (dired-mode . dired-hide-details-mode))

;; >> SET DEFAULT FONT FACE <<
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas 12" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "JetBrains Mono" (font-family-list))
    (set-frame-font "JetBrains Mono Light 13" t t)
    (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono")
    (set-face-attribute 'variable-pitch nil :family "Arial")))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono 12" t t))))

;; >> ENABLE LIGATURES <<
(load "~/.emacs.d/lisp/packages/ligature.el")

;; >> INSTALL CUSTOM THEMES <<
(setq custom-safe-themes t)

(require-theme 'modus-themes)

(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-faint)

;; (setq-default modus-themes-italic-constructs t)

(load "~/.emacs.d/lisp/config/modus-themes-customs.el")

;; >> ALL THE ICONS <<
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook  (dired-mode-hook . all-the-icons-dired-mode))
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook ((marginalia-mode . all-the-icons-completion-marginalia-setup)
         (company-mode . all-the-icons-completion-mode))
  :config
  (all-the-icons-completion-mode t))

;; >> AUTO-DARK <<
;; Package for syncing themes with system
(use-package auto-dark
  :ensure t
  :diminish auto-dark-mode
  :config
  (setq auto-dark-light-theme 'modus-operandi)
  (setq auto-dark-dark-theme 'polar-bear)
  (setq auto-dark-polling-interval 3)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

(custom-set-faces
 ;; set fringe to no background for every theme
 '(fringe ((t (:background nil)))))

;; >> HTMLIZE <<
;; Export to html with code colors and formatting
(use-package htmlize
  :ensure t)

;; >> RESTART-EMACS <<
;; package for easy emacs restarting
(use-package restart-emacs
  :ensure t)

;; Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode))

;; auto revert/refresh file when change detected
(global-auto-revert-mode 1)

;; >> RECENT FILES<<
(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "private/tmp")
  (recentf-mode t))

(setq vc-follow-symlinks t) ;; auto follow symlinkgs without asking

(setq use-short-answers t) ;; Use y-n instead of yes-no

(global-goto-address-mode t) ;; Make links clickable

;; Ignore case in completion
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; >> UNDO-TREE <<
;; much better undo setup for emacs
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (("C-{" . #'undo-tree-undo)
         ("C-}" . #'undo-tree-redo))
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-dir")))
  (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode t)
  )

;; >> RMSbolt <<
;; compiler output viewer
(use-package rmsbolt
  :ensure t)

;; >> WHICH-KEY <<
;; Reminds about key combinations
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;; >> AVY <<
;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t
  :bind (("C-c z" . #'avy-goto-line)
         ("C-c x" . #'avy-goto-word-1)
         ("C-c c" . #'avy-goto-char)
         ("C-c v" . #'avy-resume)))   ;; enable avy for quick navigation

;; >> EXEC PATH FROM SHELL <<
;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; >> PROJECTILE <<
;; Project Interaction Library for Emacs
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Projects/")))

(save-place-mode t) ;; Save place in buffer

;; >> RAINBOW MODE <<
;; colorize color names in buffers
(use-package rainbow-mode
  :ensure t)


;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │ EDITING OPTIONS                                                            │
;; └────────────────────────────────────────────────────────────────────────────┘
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(setq-default tab-width 2) ;; Set tab length = 2

;; Fill column = 80
(setq-default fill-column 80)
;; Fill column ruler
;; (use-package display-fill-column-indicator-mode
;; :hook (prog-mode LaTeX-mode))
;; (global-display-fill-column-indicator-mode t)

(setq-default indent-tabs-mode nil) ;; Indent by spaces

(delete-selection-mode 1) ;; Paste over selected region

;; Navigate inside camelCaseWords
(subword-mode t)

;; Smart parenthesis matching
(use-package smartparens
  :ensure t
  :diminish
  :hook (prog-mode text-mode LaTeX-mode toml-ts-mode markdown-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

(electric-pair-mode 0)

(electric-indent-mode 1) ;; Automatically indent

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))


;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ PROGRAMMING LANGUAGES SETUP                                             │
;; └─────────────────────────────────────────────────────────────────────────┘

;; Python venv support
(use-package pyvenv
  :ensure t
  :config (pyvenv-mode t))

;; Vim Script support
(use-package vimrc-mode
  :ensure t)

;; Go Support
(use-package go-mode
  :ensure t)

;; Haskell Support
(use-package haskell-mode
  :ensure t)

;; JSON Support
(use-package json-mode
  :ensure t)

;; Lua Support
(use-package lua-mode
  :ensure t)

;; NASM Support
(use-package nasm-mode
  :ensure t)

;; PHP Support
(use-package php-mode
  :ensure t)

;; Rust Support
(use-package rust-mode
  :ensure t)

;; Additional Lisp support
(use-package sly
  :ensure t)

;; Swift Support
(use-package swift-mode
  :ensure t)

;; Typescript Support
(use-package typescript-mode
  :ensure t)

;; YAML Support
(use-package yaml-mode
  :ensure t)

;; Markdown support
(use-package markdown-mode
  :ensure t)

;; Doxygen highlighting
(use-package highlight-doxygen
  :ensure t
  :hook (c++-ts-mode))

;; >> ELDOC <<
;; Show function arglist or variable docstring in echo area
(use-package eldoc
  :diminish eldoc-mode)

;; >> DEVDOCS <<
;; Read documentation from https://devdocs.io offline
(use-package devdocs
  :ensure t)

;; >> FLYMAKE <<
;; Inline static analysis
(use-package flymake
  :diminish flymake-mode
  :hook (prog-mode))
(use-package flymake-shell
  :ensure t)

;; >> FORMAT ALL <<
;; easy formatting using unified commands
(use-package format-all
  :ensure t
  :diminish
  :hook (emacs-lisp-mode c++-ts-mode python-ts-mode))
(setq format-all-formatters
      '(("C++" (clang-format "--style=Google"))
        ("Python" (yapf "--style=Google"))))

;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ COMPLETION                                                              │
;; └─────────────────────────────────────────────────────────────────────────┘

(load "~/.emacs.d/lisp/packages/completion-bundle.el")

;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ OTHER PACKAGES                                                          │
;; └─────────────────────────────────────────────────────────────────────────┘

(load "~/.emacs.d/lisp/packages/magit.el")
(load "~/.emacs.d/lisp/packages/treesitter.el")


;;;;;;;;;;;;;;;;;;;;;
;; KEYMAP SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/lisp/config/keymap.el")

(provide 'init)
;;; init.el ends here
