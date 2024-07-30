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
			  (width . 110) ; chars
			  (height . 46) ; lines
		      ))
	  (setq default-frame-alist
			'(
			  (tool-bar-lines . 0)
			  (width . 110)
			  (height . 46))))
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
;; (defun dired-no-line-wrap ()
;;   (setq truncate-lines t))
;; (add-hook 'dired-mode-hook 'dired-no-line-wrap)

;; Automatically pair parentheses
(electric-pair-mode t)
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

;;; Vim Emulation
(use-package evil
  :ensure t)

;; Enable Vim emulation
(evil-mode t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package rmsbolt
  :ensure t)

(use-package eat
  :ensure t)
