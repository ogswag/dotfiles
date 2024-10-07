;;; Personal configuration -*- lexical-binding: t -*-

;; ┌──────────────────────────────────────────────────────────────────────────┐
;; │ Store automatic customisation options elsewhere                          │
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
;; │ THEMES, FONTS AND VISUAL OPTIONS                                         │
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
              (height . 55))))
  (progn
    (setq initial-frame-alist '((tool-bar-lines . 0)))
    (setq default-frame-alist '((tool-bar-lines . 0)))))

(use-package ns-auto-titlebar
  :ensure t)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))
; (when (eq system-type 'darwin) (add-to-list 'default-frame-alist '(undecorated-round . t)))

;; Resize frames and windows by pixels, not by chars
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Smooth scrolling
(pixel-scroll-precision-mode t)

;; Enable mouse in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(blink-cursor-mode 1) ;; Stop (or don't) cursor blinking
(setq-default cursor-type 'box) ;; Set cursor shape

;; Enable column numbering in `prog-mode'
(add-hook 'prog-mode-hook #'column-number-mode)

;; Enable line numbering for a few major modes
(dolist (hook '(prog-mode-hook LaTeX-mode-hook toml-ts-mode-hook yaml-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
;; do not decrease line number width, results in less shifting
(setq-default display-line-numbers-grow-only t)
;; set line number width to a maximum needed number (no shifting when scrolling)
(setq-default display-line-numbers-width-start t)

;; Do not wrap line by default, unless in specific modes
(setq-default truncate-lines t)
(dolist (hook '(prog-mode-hook LaTeX-mode-hook text-mode-hook toml-ts-mode-hook))
  (add-hook hook #'visual-line-mode))

;; >>ADAPTIVE WRAP<<
;; Enables "adaptive prefix" - lines maintain alignment position when visually
;; wrapped
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

;; No sounds in emacs
(setq-default ring-bell-function 'ignore)

;; Simplified dired setup
(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  ;; (dired-listing-switches "-lh --group-directories-first --dired")
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
  (when (member "JetBrains Mono" (font-family-list))
    (set-frame-font "JetBrains Mono 13" t t)
    (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono")
    (set-face-attribute 'variable-pitch nil :family "Arial")))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono 12" t t))))

(load "~/.emacs.d/lisp/packages/ligature.el")

;; >> INSTALL CUSTOM THEMES <<
(require-theme 'modus-themes)

(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-faint)

(setq-default modus-themes-italic-constructs t)

(load "~/.emacs.d/lisp/config/modus-themes-customs.el")

;; >> AUTO-DARK <<
;; Package for syncing themes with system
(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-light-theme 'modus-operandi)
  (setq auto-dark-dark-theme 'modus-vivendi)
  (setq auto-dark-polling-interval 3)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t))

(custom-set-faces
 ;; set fringe to no background for every theme
 '(fringe ((t (:background nil)))))

(auto-dark-mode t)

(use-package htmlize
  :ensure t)

;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │ EDITOR OPTIONS                                                             │
;; └────────────────────────────────────────────────────────────────────────────┘
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; >> RAINBOW MODE <<
;; colorize color names in buffers
(use-package rainbow-mode
  :ensure t)

;; >> RESTART-EMACS <<
;; package for easy emacs restarting
(use-package restart-emacs
  :ensure t)

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

(save-place-mode t) ;; Save place in buffer

(setq use-short-answers t) ;; Use y-n instead of yes-no

(global-goto-address-mode t) ;; Make links clickable

(setq-default tab-width 2) ;; Set tab length = 2

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode t)

(setq-default indent-tabs-mode nil) ;; Indent by spaces

(delete-selection-mode 1) ;; Paste over selected region

(setq vc-follow-symlinks t) ;; auto follow symlinkgs without asking

;; >> UNDO-TREE <<
;; much better undo setup for emacs
(use-package undo-tree
  :ensure t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)
(global-set-key (kbd "C-{") #'undo-tree-undo)
(global-set-key (kbd "C-}") #'undo-tree-redo)

;; >> RMSbolt <<
;; compiler output viewer
(use-package rmsbolt
  :ensure t)

;; >> WHICH-KEY <<
;; Reminds about key combinations
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; >> AVY <<
;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t)   ;; enable avy for quick navigation
(global-set-key (kbd "C-c z") 'avy-goto-line)
(global-set-key (kbd "C-c x") 'avy-goto-word-1)
(global-set-key (kbd "C-c c") 'avy-goto-char)
(global-set-key (kbd "C-c v") 'avy-resume)

;; >> GOD MODE <<
;; >> no more RSI
;; Minor mode for God-like command entering
;; (similar to minor modal mode)
(use-package god-mode
  :ensure t)
(global-set-key (kbd "C-.") #'god-local-mode)

;; Set better `set-mark-command' keybinding
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-@"))
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") #'set-mark-command)

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(global-set-key (kbd "<C-M-wheel-up>") 'ignore)
(global-set-key (kbd "<C-M-wheel-down>") 'ignore)

(global-set-key (kbd "s-C-f") #'toggle-frame-maximized)
(global-set-key (kbd "s-S-C-f") #'toggle-frame-fullscreen)

;; >> RUSSIAN TECHWRITER <<
;; Second input method
(use-package russian-techwriter
  :ensure t)
(setq-default default-input-method 'russian-techwriter)

;; >> FORMAT ALL <<
;; easy formatting using unified commands
(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (emacs-lisp-mode . format-all-mode)
  )
;; (setq format-all-formatters
;;       '(("C++" (clang-format "--style=Google"))
;;         ("Python" (ruff))))

;; >> EXEC PATH FROM SHELL <<
;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ PROGRAMMING LANGUAGES SETUP                                             │
;; └─────────────────────────────────────────────────────────────────────────┘
;;; Vim Script support
(use-package vimrc-mode
  :ensure t)

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

;; (use-package auctex
;; :ensure t)

(use-package devdocs
  :ensure t)

;; ┌─────────────────────────────────────────────────────────────────────────┐
;; │ COMPLETION                                                              │
;; └─────────────────────────────────────────────────────────────────────────┘
;; >> VERTICO <<
;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  )
;; Enable completion by narrowing
(vertico-mode t)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; >> ORDERLESS <<
;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; >> CONSULT <<
;; Extended completion utilities
(use-package consult
  :ensure t)
(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c l") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; >> MARGINALIA <<
;; Marginalia (documentation and notes) in the minibuffer
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; >> EGLOT <<
;; LSP Support
(use-package eglot
  :ensure t)
;; Enable LSP support by default in programming buffers
(add-hook 'c++-mode-hook #'eglot-ensure)
;; Create a memorable alias for `eglot-ensure'.
;; (defalias 'start-lsp-server #'eglot)

(with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
        '((c-mode c++-mode)
             . ("clangd"
                   "--fallback-style=Google"
                   "--background-index"
                   "--clang-tidy"
                   "--completion-style=detailed"
                   "--pch-storage=memory"
                   ))))

;; >> FLYMAKE <<
;; Inline static analysis
;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; >> CORFU <<
;; Pop-up completion
(use-package corfu
  :ensure t
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'directory) ;; Always preselect the prompt
  (corfu-auto t)               ;; Enable auto completion
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-popupinfo-delay 0.2)
  (corfu-auto-delay  0.3)
  (corfu-auto-prefix 2)

  :bind
  ;; Use TAB for cycling, default is `corfu-complete'.
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("ESC" . corfu-quit)
        ([escape] . corfu-quit)
        ))
(corfu-echo-mode t)
(corfu-popupinfo-mode t)
(corfu-history-mode t)
;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)     ;; Complete word from current buffers. See also dabbrev-capf on Emacs 29.
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete Elisp in Org or Markdown code block.
  (add-hook 'completion-at-point-functions #'cape-file)        ;; Complete file name.
  (add-hook 'completion-at-point-functions #'cape-history)     ;; Complete from Eshell, Comint or minibuffer history.
  (add-hook 'completion-at-point-functions #'cape-keyword)     ;; Complete programming language keyword.
  (add-hook 'completion-at-point-functions #'cape-tex)         ;; Complete Unicode char from TeX command, e.g. `\hbar'.
  )


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
(diminish 'eglot)
(diminish 'format-all-mode)
