(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq native-comp-async-report-warnings-errors 'silent)

(setq mac-command-modifier 'hyper) ; make cmd key do Meta
(setq mac-option-modifier 'meta) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control


(fringe-mode '(8 . 0))
(setq-default indicate-empty-lines t)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(context-menu-mode 1)


(setq
 make-backup-files nil  ;; disable automatic backup~ file
 auto-save-default nil
 create-lockfiles nil)  ;; stop creating #auto-save# files


(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)


(setq-default ring-bell-function 'ignore)

(setq vc-follow-symlinks t) ;; auto follow symlinkgs without asking

(setq use-short-answers t) ;; Use y-n instead of yes-no

(global-goto-address-mode t) ;; Make links clickable


;; Fill column
(setq-default fill-column 240)
;; Fill column ruler
(use-package display-fill-column-indicator-mode
    :hook (prog-mode LaTeX-mode))


(setq-default indent-tabs-mode nil) ;; Indent by spaces
(setq-default tab-width 4)          ;; Set tab length = N
(setq-default c-basic-offset 4)     ;; Set default offset for C-like modes
(setq-default python-indent-guess-indent-offset nil)


(electric-indent-mode 1)
(electric-pair-mode 1)


(blink-cursor-mode 1) ;; Stop (or don't) cursor blinking
(setq-default cursor-type 'box) ;; Set cursor shape


(delete-selection-mode 1) ;; Paste over selected region


(subword-mode 1) ;; Navigate inside camelCaseWords


;; Enable mouse in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))


(add-hook 'prog-mode-hook #'column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers 'relative)
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-grow-only t)
(setq-default display-line-numbers-width-start t)


;; Do not wrap line by default, unless in specific modes
(setq-default truncate-lines t)
(use-package visual-line
  :hook (LaTeX-mode toml-ts-mode))


(global-hl-line-mode t)


;; Allow right-left scrolling with mouse
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)


;; Delete trailing whitespace before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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


(savehist-mode t) ;; Persist history over Emacs restarts
(save-place-mode t) ;; Save place in buffer
(global-auto-revert-mode 1) ;; auto revert/refresh file when change detected
(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "private/tmp")
  (recentf-mode 1))


;; Ignore case in completion
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)


(setq custom-safe-themes t)

(require 'project)

;; Define your projects
;; (setq project--list
;;       '(("~/Workspace/dotfiles/" . "Dotfiles")))


(setq-default default-input-method 'russian-computer)

(load "~/.emacs.d/elisp/colors.el")
(load "~/.emacs.d/elisp/font.el")


(add-to-list 'load-path "~/.emacs.d/package-local/")

(use-package vimrc-mode
  :defer t
  :commands (vimrc-mode))

(use-package rainbow-mode
  :defer t
  :commands (rainbow-mode))

(use-package rainbow-delimiters
  :demand t
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package which-key
  :demand t
  :config (which-key-mode 1))

(use-package avy
  :demand t
  :bind ("C-c c" . 'avy-goto-char))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer t
  :commands (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))

(use-package highlight-doxygen
  :defer t
  :commands (highlight-doxygen-global-mode)
  :hook (c++-mode c++-ts-mode))

(use-package markdown-mode
  :defer t
  :mode "\\.md\\'"
  :commands (markdown-mode))

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(use-package rmsbolt
  :ensure t
  :defer t
  :commands (rmsbolt))

(use-package ace-window
  :ensure t
  :demand t
  :bind ("C-c j" . ace-window))

(use-package undo-fu
  :ensure t
  :demand t
  :bind
  (("C-z" . undo-fu-only-undo)
   ("C-S-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :demand t
  :config
  (undo-fu-session-global-mode t))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :init (vertico-mode))

(use-package consult
  :ensure t
  :bind (("C-c i"   . #'consult-imenu)
         ("C-c l"   . #'consult-line)
         ("C-c b"   . #'consult-buffer)
         ("C-x b"   . #'consult-buffer)
         ("C-c r"   . #'consult-recent-file)
         ("C-c R"   . #'consult-bookmark)
         ("C-c `"   . #'consult-flymake)
         ("C-c h"   . #'consult-ripgrep)))
