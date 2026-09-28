;;; ui.el --- Interface: startup, scrolling, fringes, line numbers -*- lexical-binding: t; -*-

;;; Commentary:
;; chrome inside the frame once it exists (geometry is early-init).

;;; Code:

;;;; Startup

(fset 'display-startup-echo-area-message #'ignore)
(setq-default initial-major-mode 'fundamental-mode
	      initial-scratch-message nil
	      inhibit-splash-screen t)

;;;; Chrome

(context-menu-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq visual-bell t)
(setq ring-bell-function 'ignore)

(use-package seek
  :load-path my-vendor-directory
  :defer t
  :commands (seek seek-with)
  :autoload (seek-context-menu)
  :init (add-hook 'context-menu-functions #'seek-context-menu 90))

;;;; Line numbers

(use-package display-line-numbers :ensure nil
  :demand t
  :hook (prog-mode text-mode LaTeX-mode conf-mode)
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t))

;;;; Fringes

;; (set-fringe-style 0)
(setq-default fringe-indicator-alist
              '((overlay-arrow . right-triangle)
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

;;;; Invisible characters

(declare-function my/theme-shade "theme" (percent))

(defcustom my/whitespace-shade 30
  "How far the whitespace marks sit off the page, as a percentage."
  :type 'natnum
  :group 'whitespace)

(defcustom my/whitespace-hooks '(prog-mode-hook text-mode-hook conf-mode-hook)
  "Mode hooks whose buffers show their whitespace."
  :type '(repeat variable)
  :group 'whitespace)

(defun my/whitespace--refresh-faces (&rest _)
  "Point the whitespace marks at a shade of the current background."
  (let ((shade (my/theme-shade my/whitespace-shade)))
    (dolist (face '(whitespace-space whitespace-hspace whitespace-tab
				     whitespace-newline whitespace-trailing whitespace-empty
				     whitespace-indentation whitespace-big-indent
				     whitespace-space-after-tab whitespace-space-before-tab
				     whitespace-missing-newline-at-eof))
      (when (facep face)
        (set-face-attribute face nil
                            :foreground shade :background 'unspecified
                            :inherit 'unspecified :weight 'normal
                            :underline nil :box nil :inverse-video nil)))))

(use-package whitespace :ensure nil
  :demand t
  :custom
  ;; Marks only - no `lines' or `lines-tail'.
  (whitespace-style '(face tabs tab-mark newline newline-mark
                           trailing missing-newline-at-eof))
  ;; The stock glyphs are `$' for a newline and `»' for a tab.
  (whitespace-display-mappings
   `((tab-mark ?\t ,(vector (if (char-displayable-p ?›) ?› ?>) ?\t) [?\\ ?\t])
     (newline-mark ?\n ,(vector (if (char-displayable-p ?﹩) ?﹩ ?$) ?\n) [?$ ?\n])
     (space-mark ?\s ,(vector (if (char-displayable-p ?·) ?· ?.)) [?.])))
  :config
  (my/whitespace--refresh-faces)
  (add-hook 'enable-theme-functions #'my/whitespace--refresh-faces)
  (dolist (hook my/whitespace-hooks)
    (add-hook hook #'whitespace-mode)))

;;;; Buffer names

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "✦")
  (uniquify-after-kill-buffer-p t))

;;;; Outlines

(setq outline-minor-mode-highlight t)

;;; ui.el ends here
