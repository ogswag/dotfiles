;; No Tool Bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(context-menu-mode 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; stop creating #auto-save# files
(setq create-lockfiles nil)
;; auto revert/refresh file when change detected
(global-auto-revert-mode 1)
;; disable emacs automatic backup~ file
(setq make-backup-files nil)


(if (display-graphic-p)
	(progn
	  (setq initial-frame-alist
			'(
			  (tool-bar-lines . 0)
			  (width . 120) ; chars
			  (height . 47) ; lines
		      ))
	  (setq default-frame-alist
			'(
			  (tool-bar-lines . 0)
			  (width . 120)
			  (height . 47))))
  (progn
	(setq initial-frame-alist '( (tool-bar-lines . 0)))
	(setq default-frame-alist '( (tool-bar-lines . 0)))))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Stop cursor blinking
(blink-cursor-mode 0)

;; Enable column numbering in `prog-mode'
(add-hook 'prog-mode-hook #'column-number-mode)
;; Enable line numbering in `prog-mode'
(dolist (hook '(prog-mode-hook LaTeX-mode-hook toml-ts-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
;; do not decrease line number width, results in less shifting
(setq-default display-line-numbers-grow-only t)
;; set line number width to a maximum needed number (no shifting when scrolling)
(setq-default display-line-numbers-width-start t)

;; Paste over selected region
(delete-selection-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Line Wrapping Setup
(setq-default truncate-lines t)
(dolist (hook '(prog-mode-hook LaTeX-mode-hook text-mode-hook toml-ts-mode-hook))
  (add-hook hook #'visual-line-mode))
(use-package adaptive-wrap
  :ensure t)
;; (defun az-no-line-wrap ()
;;   (setq truncate-lines t))
;; (add-hook 'dired-mode-hook 'az-no-line-wrap)
;; (add-hook 'minibuffer-mode-hook 'az-no-line-wrap)

;; Automatically pair parentheses
(dolist (hook '(prog-mode-hook LaTeX-mode-hook text-mode-hook toml-ts-mode-hook))
  (add-hook hook #'electric-pair-mode))
;; Automatically indent
(electric-indent-mode 1)

;; Miscellaneous options
(setq-default major-mode
			  (lambda () ; guess major mode from file name
				(unless buffer-file-name
				  (let ((buffer-file-name (buffer-name)))
					(set-auto-mode)))))
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(setq use-short-answers t)

;; Simple and clean whitespace mode setup, put the following in your emacs init.
(progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://xahlee.info/emacs/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
		;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
		'(
		  (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
		  (newline-mark 10 [182 10]) ; LINE FEED,
		  (tab-mark 9 [9655 9] [92 9]) ; tab
		  )))

(package-initialize)
(require 'package)
(add-to-list 'package-archives '("gnu"	 . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package undo-tree
  :ensure t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)

;;; Jump to arbitrary positions
(use-package avy
  :ensure t)
(global-set-key (kbd "C-c z") #'avy-goto-word-1)

;; ;;; Vim Emulation
;; (use-package evil
;;   :ensure t)
;; ;; Enable Vim emulation
;; (evil-mode t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package rmsbolt
  :ensure t)

(use-package eat
  :ensure t)

(use-package writeroom-mode
  :ensure t)

(use-package olivetti
  :ensure t)

;; ,----
;; | Make a boxquote around any text, like this one
;; `---
(use-package boxquote
  :ensure t)

;; Use MacOS Spotlight search
(use-package spotlight
  :ensure t)

(use-package meow
  :ensure t)
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
(require 'meow)
(meow-setup)
(meow-global-mode 1)
