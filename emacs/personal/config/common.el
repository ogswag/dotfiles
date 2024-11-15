;; Stop Emacs littering
(setq
 make-backup-files nil  ;; disable automatic backup~ file
 auto-save-default nil
 create-lockfiles nil)  ;; stop creating #auto-save# files

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
;; Fill column
(setq-default fill-column 110)
;; Fill column ruler
(use-package display-fill-column-indicator-mode
    :hook (prog-mode LaTeX-mode))
;; (global-display-fill-column-indicator-mode t)

(setq-default indent-tabs-mode nil) ;; Indent by spaces
(setq-default tab-width 4)          ;; Set tab length = N
(setq-default c-basic-offset 4)     ;; Set default offset for C-like modes
(setq-default python-indent-guess-indent-offset nil)
(electric-indent-mode nil)          ;; Automatically indent

(delete-selection-mode 1) ;; Paste over selected region

;; Navigate inside camelCaseWords
(subword-mode t)
;; Enable mouse in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(electric-pair-mode 1)

(blink-cursor-mode 1) ;; Stop (or don't) cursor blinking
(setq-default cursor-type 'box) ;; Set cursor shape

;; Enable column numbering in `prog-mode'
(add-hook 'prog-mode-hook #'column-number-mode)

(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
;; Enable line numbering for a few major modes
(use-package display-line-numbers-mode
  :hook (prog-mode-hook))

;; do not decrease line number width, results in less shifting
(setq-default display-line-numbers-grow-only t)
;; set line number width to a maximum needed number (no shifting when scrolling)
(setq-default display-line-numbers-width-start t)

;; Do not wrap line by default, unless in specific modes
(setq-default truncate-lines t)
(use-package visual-line
  :diminish visual-line-mode
  :hook (LaTeX-mode toml-ts-mode))

(global-hl-line-mode t)

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

(save-place-mode t) ;; Save place in buffer

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
