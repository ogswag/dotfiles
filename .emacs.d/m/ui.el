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

(tool-bar-mode -1)
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

;;;; Scrolling

(setq pixel-scroll-precision-use-momentum nil)

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
   `((tab-mark ?\t ,(vector (if (char-displayable-p ?>) ?> ?>) ?\t) [?\\ ?\t])
     ;; (newline-mark ?\n ,(vector (if (char-displayable-p ?¶) ?¶ ?$) ?\n) [?$ ?\n])
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

;; (setq outline-minor-mode-cycle t)
(setq outline-minor-mode-use-buttons 'in-margins)
(setq outline-minor-mode-highlight t)
;; (setq outline-minor-mode-highlight 'override)

;;;; Outline Icons

(require 'icons)

(defface my/outline-margin '((t nil))
  "Face for the outline fold markers in the margin.
Colour only.  No `:family': the markers are images now, and the characters
behind them are ones every font has -- see below."
  :group 'outline)


(with-eval-after-load 'outline
  (define-icon outline-open-in-margins nil
    '((image "outline-open.svg" "outline-open.pbm" :height line)
      (symbol "▼" :face my/outline-margin)
      (text "v"))
    "Opened-section marker in the margin.
An image, so it is the same marker in every font; sized per window, so it
follows `text-scale-adjust'."
    :group outline
    :version "31.1")

  (define-icon outline-close-in-margins nil
    '((image "outline-open.svg" "outline-open.pbm" :height line :rotation -90)
      (symbol "▶" :face my/outline-margin)
      (text ">"))
    "Closed-section marker in the margin.
The opened marker turned a quarter turn, which is how Emacs's own default
draws it."
    :group outline
    :version "31.1"))

(defvar outline--margin-width)
(defvar outline--use-rtl)
(defvar outline--button-icons)
(declare-function outline--create-button-icons "outline" ())
(declare-function outline--fix-buttons "outline" (&optional beg end))

(defun my/outline-refit-margin ()
  "Redraw the outline markers at the buffer's current text scale."
  (when (and (bound-and-true-p outline-minor-mode)
             (eq outline-minor-mode-use-buttons 'in-margins)
             outline--margin-width
             (not outline--use-rtl))
    ;; Rebuild at the size in force now, then re-place every button from them.
    (setq-local outline--button-icons (outline--create-button-icons))
    (outline--fix-buttons (point-min) (point-max))
    (let ((new (max 1 (ceiling (/ (string-pixel-width
                                   (icon-string 'outline-open-in-margins)
                                   (current-buffer))
                                  (* (frame-char-width) 1.0))))))
      (unless (= new outline--margin-width)
        (setq-local left-margin-width
                    (+ (- left-margin-width outline--margin-width) new))
        (setq outline--margin-width new))
      ;; Margins are read when a buffer is put in a window.
      (when (eq (current-buffer) (window-buffer))
        (set-window-buffer nil (window-buffer))))))

(add-hook 'text-scale-mode-hook #'my/outline-refit-margin)

;;; ui.el ends here
