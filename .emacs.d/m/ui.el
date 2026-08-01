;;; ui.el --- Interface: startup, scrolling, fringes, line numbers -*- lexical-binding: t; -*-

;;; Commentary:
;; Frame geometry lives in early-init.el; everything here is about what the
;; frame contains once it exists.

;;; Code:

;;;; Startup

(fset 'display-startup-echo-area-message #'ignore)
(setq-default initial-major-mode 'fundamental-mode
	      initial-scratch-message nil
	      inhibit-splash-screen t)

;;;; Chrome

(tool-bar-mode -1)
(tooltip-mode t)
(context-menu-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq visual-bell t)
(setq ring-bell-function 'ignore)

;;;; Scrolling

(setq pixel-scroll-precision-use-momentum nil)
;; (pixel-scroll-precision-mode 1)
(setq-default scroll-margin 10)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position 'always)

;;;; Line numbers

(use-package display-line-numbers :ensure nil
  :demand t
  :hook (prog-mode text-mode LaTeX-mode conf-mode)
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  ;; (display-line-numbers-type 'relative)
  )

;;;; Fringes
;;
;; Width zero, but the indicator alist is still set so the indicators show in
;; the margins where they can.

(set-fringe-style 0)
(setq-default fringe-indicator-alist
              '((truncation left-triangle right-triangle)
                (continuation left-curly-arrow right-curly-arrow)
                (overlay-arrow . right-triangle)
                (up . up-arrow)
                (down . down-arrow)
                (top top-left-angle top-right-angle)
                (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                (empty-line . empty-line)
                (unknown . question-mark)))

;;;; Lines

(setq-default truncate-lines t)
(global-visual-wrap-prefix-mode t)

;;;; Buffer names

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "✦")
  (uniquify-after-kill-buffer-p t))

;;;; Outlines

(setq outline-minor-mode-cycle t)          ; Tab cycles visibility on headings
(setq outline-minor-mode-use-buttons 'in-margins)
;; (setq outline-minor-mode-highlight 'override)

;;;; Mode line

(use-package hide-mode-line :ensure t
  :commands (hide-mode-line-mode global-hide-mode-line-mode))

;; (use-package echo-bar
;;   :vc (:url "https://github.com/chenanton/echo-bar")
;;   :custom
;;   (echo-bar-layout
;;    '(:center ("buffer-position" "buffer-name" "major-mode")
;;      :right  ("project" "vcs" "time" "battery")))
;;   :config
;;   (echo-bar-mode 1))

;;; ui.el ends here
