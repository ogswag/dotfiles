;;; edit.el --- Editing behaviour -*- lexical-binding: t; -*-

;;; Commentary:
;; Point, mark, selection, undo, and the things Emacs writes to disk on your
;; behalf.

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

(setq-default vc-follow-symlinks t)

;; No stray #foo# / foo~ litter; undo-fu-session covers the actual recovery case.
(setq auto-save-default nil)
(setq make-backup-files nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Cursor

(setq x-stretch-cursor t)
(setq-default cursor-type 'box)

;;;; Mark and selection

(transient-mark-mode t)
(setq mark-even-if-inactive nil)

;; `permanent' keeps a shift-selected region alive across unshifted motion —
;; only an explicit deactivation (C-g, self-insert, ...) drops it.
(setq shift-select-mode 'permanent)

;; Org otherwise steals S-<arrow> for todo/priority/timestamp cycling.
;; `setq-default' rather than `setq' only to keep the byte-compiler quiet about
;; a variable org has not defined yet; the defcustom will not clobber it.
(setq-default org-support-shift-select 'always)

;; S-<down-mouse-1> adjusts the region instead of popping the appearance menu.
(when (fboundp 'mouse-shift-adjust-mode)
  (mouse-shift-adjust-mode 1))

;; Deactivating the mark should really deactivate it — no lingering ring to
;; jump back into by accident.
(add-hook 'deactivate-mark-hook
          (lambda ()
            (setq mark-ring nil)
            (set-marker (mark-marker) nil)))

(use-package delsel :ensure nil ; built-in
  :hook (after-init . delete-selection-mode))

(electric-pair-mode -1)

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
;;
;; NOTE: "-ic" sources the full interactive zsh rc for every M-! — that is what
;; makes shell commands feel slow.  Drop the "i" if you stop needing rc aliases.

(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

(declare-function ansi-color-apply-on-region "ansi-color")

;; Was previously hung off `shell-command-setup-hook' calling
;; `ansi-color-apply-on-buffer' — neither of which exists, so it never ran.
(defun my/shell-command-colorize (&rest _)
  "Render ANSI colour escapes in `*Shell Command Output*'."
  (require 'ansi-color)
  (when-let* ((buffer (get-buffer "*Shell Command Output*")))
    (with-current-buffer buffer
      (ansi-color-apply-on-region (point-min) (point-max)))))

(advice-add 'shell-command :after #'my/shell-command-colorize)

;;; edit.el ends here
