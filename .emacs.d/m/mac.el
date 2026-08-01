;;; mac.el --- macOS integration and input methods -*- lexical-binding: t; -*-

;;; Code:

;;;; Modifiers

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

;; Open files in the existing frame rather than spawning new ones.
(setq ns-pop-up-frames nil)

;;;; Input methods

;; C-\ toggles between this and English.
(setq-default default-input-method 'russian-computer)

;; emacs-mac: fall back to ASCII input for key sequences, so C-x etc. keep
;; working while a non-Latin input source is active.
(when (fboundp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

;; Makes Emacs bindings work under a Russian system layout by translating
;; keys back through the input method.
(use-package reverse-im :ensure t :demand t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;;;; Finder

(defun my/open-curdir ()
  "Open the current file's directory in Finder on macOS."
  (interactive)
  (let ((dir (or (and buffer-file-name
                      (file-name-directory buffer-file-name))
                 default-directory)))
    (shell-command (concat "open " (shell-quote-argument dir)))))

;;; mac.el ends here
