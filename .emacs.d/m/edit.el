;;; edit.el --- Editing behaviour -*- lexical-binding: t; -*-

;;; Commentary:
;; point, mark, selection, undo, autosave littering.

;;; Code:

;; Smart C-a / C-e
(use-package mwim :ensure t
  :commands (mwim-beginning mwim-end))

;;;; Prompts

(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;;; Files

;; Untitled buffers, optionally typed.  No autoloads are generated for m/p/,
(use-package neofile
  :load-path my-vendor-directory
  :defer t
  :commands (neofile-new-file-fast neofile-new-file-with-type))

(setq-default vc-follow-symlinks t)

;;;; Images

(use-package imgdrop
  :load-path my-vendor-directory
  :defer t
  :commands (imgdrop-mode imgdrop-insert-image imgdrop-insert-image-gui)
  :hook ((TeX-mode markdown-mode org-mode mhtml-mode html-mode) . imgdrop-mode))

;; No stray #foo# / foo~ litter; undo-fu-session covers the actual recovery case.
(setq auto-save-default nil)
(setq make-backup-files nil)

(defvar my/keep-trailing-whitespace-modes '(markdown-mode diff-mode)
  "Modes whose trailing whitespace survives a save.")

(defun my/delete-trailing-whitespace-maybe ()
  "Strip trailing whitespace unless this buffer's mode needs it."
  (unless (derived-mode-p my/keep-trailing-whitespace-modes)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'my/delete-trailing-whitespace-maybe)

;;;; Cursor

(setq-default cursor-type '(bar . 2))


(declare-function my/theme-shade "theme" (percent))

(defface my/stretch-cursor '((t nil))
  "Face for the tab under point, standing in for a stretched block cursor.
Set from `my/stretch-cursor--refresh-face' rather than here: both attributes
are the theme's and neither survives a theme change."
  :group 'cursor)

(defun my/stretch-cursor--refresh-face (&rest _)
  "Colour `my/stretch-cursor' like the block cursor it stands in for."
  (let ((cursor (face-attribute 'cursor :background nil t)))
    (set-face-attribute 'my/stretch-cursor nil
                        :background (if (stringp cursor)
                                        cursor
                                      (my/theme-shade 25))
                        :foreground (face-attribute 'default :background nil t)
                        :inherit 'unspecified)))

(defvar my/stretch-cursor--overlay nil
  "The one overlay, moved from buffer to buffer as point lands on a tab.")

(defun my/stretch-cursor--update ()
  "Cover the tab under point, or uncover the last one."
  (unless (overlayp my/stretch-cursor--overlay)
    (setq my/stretch-cursor--overlay (make-overlay 1 1))
    (overlay-put my/stretch-cursor--overlay 'face 'my/stretch-cursor)
    (overlay-put my/stretch-cursor--overlay 'priority 100)
    (overlay-put my/stretch-cursor--overlay 'window t))
  (if (and (eq (char-after) ?\t)
           (not (region-active-p))
           (not (minibufferp))
           ;; A bar has no width of its own; a box already stretches itself, given `x-stretch-cursor'. Both.
           (eq (or (car-safe cursor-type) cursor-type) 'bar))
      (move-overlay my/stretch-cursor--overlay
                    (point) (1+ (point)) (current-buffer))
    (delete-overlay my/stretch-cursor--overlay)))

(define-minor-mode my/stretch-cursor-mode
  "Show how wide the tab under point is, the way `x-stretch-cursor' would."
  :global t
  :group 'cursor
  (if my/stretch-cursor-mode
      (progn
        (my/stretch-cursor--refresh-face)
        (add-hook 'enable-theme-functions #'my/stretch-cursor--refresh-face)
        (add-hook 'post-command-hook #'my/stretch-cursor--update))
    (remove-hook 'enable-theme-functions #'my/stretch-cursor--refresh-face)
    (remove-hook 'post-command-hook #'my/stretch-cursor--update)
    (when (overlayp my/stretch-cursor--overlay)
      (delete-overlay my/stretch-cursor--overlay))))

(my/stretch-cursor-mode 1)

;;;; Mark and selection

(setq mark-even-if-inactive nil)

(setq shift-select-mode 'permanent)

;; S-<down-mouse-1> adjusts the region instead of popping the appearance menu.
(when (fboundp 'mouse-shift-adjust-mode)
  (mouse-shift-adjust-mode 1))

;; Deactivating the mark should really deactivate it - no lingering ring.
(add-hook 'deactivate-mark-hook
          (lambda ()
            (setq mark-ring nil)
            (set-marker (mark-marker) nil)))

(use-package delsel :ensure nil ; built-in
  :hook (after-init . delete-selection-mode))


(declare-function delete-selection-pre-hook "delsel" ())

(defun my/delsel-hook-first (&rest _)
  "Move `delete-selection-pre-hook' to the head of `pre-command-hook'."
  (when (memq #'delete-selection-pre-hook (default-value 'pre-command-hook))
    ;; Off and on again: the only way to record a depth for a function that is already there.
    (remove-hook 'pre-command-hook #'delete-selection-pre-hook)
    (add-hook 'pre-command-hook #'delete-selection-pre-hook -90)))

(advice-add 'delete-selection-mode :after #'my/delsel-hook-first)

;;;; Undo

(use-package undo-fu :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))

(use-package undo-fu-session :ensure t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;;;; Shell commands

(setq shell-file-name "zsh")
(setq shell-command-switch "-c")

(declare-function ansi-color-apply-on-region "ansi-color")

(defun my/shell-command-colorize (&rest _)
  "Render ANSI colour escapes in `*Shell Command Output*'."
  (require 'ansi-color)
  (when-let* ((buffer (get-buffer "*Shell Command Output*")))
    (with-current-buffer buffer
      (ansi-color-apply-on-region (point-min) (point-max)))))

(advice-add 'shell-command :after #'my/shell-command-colorize)

;;; edit.el ends here
