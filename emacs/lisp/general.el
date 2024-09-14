;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL/TEXT EDITING OPTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(global-goto-address-mode t)

(setq-default tab-width 2) ;; Set tab length = 2

(setq-default indent-tabs-mode nil) ;; Indent by spaces

(delete-selection-mode 1) ;; Paste over selected region

(use-package undo-tree
  :ensure t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)

;; >> RMSbolt <<
;; compiler output viewer
(use-package rmsbolt
    :ensure t)

;; >> KAOMOJI <<
;; cool and simple kaomoji picker (・∀・)つ
(use-package kaomoji
  :ensure t)


;;;;;;;;;;;;;;;;;;;;
;; VISUAL OPTIONS ;;
;;;;;;;;;;;;;;;;;;;;

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

;; Enable mouse in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(blink-cursor-mode 0) ;; Stop cursor blinking

(global-hl-line-mode 1) ;; Highlight current line

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

;; Different setup for truncating lines
;; (defun az-no-line-wrap ()
;;   (setq truncate-lines t))
;; (add-hook 'dired-mode-hook 'az-no-line-wrap)
;; (add-hook 'minibuffer-mode-hook 'az-no-line-wrap)

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

(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t "Удалять буфер при переходе в другой каталог.")
  (dired-listing-switches "-lh --group-directories-first --dired")
  :hook
  (dired-mode . dired-hide-details-mode))
(use-package diminish
  :ensure t)

