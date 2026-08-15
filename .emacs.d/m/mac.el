;;; mac.el --- macOS integration and input methods -*- lexical-binding: t; -*-

;;; Commentary
;; macOS related options.

;;; Code:

;;;; Modifiers

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier '(:function alt :mouse alt))
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

;; Open files in the existing frame rather than spawning new ones.
(setq ns-pop-up-frames nil)

;;;; Input methods

;; C-\ toggles between this and English.
(setq-default default-input-method 'russian-computer)

;; emacs-mac: fall back to ASCII input for key sequences, so C-x etc. keep
(when (fboundp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

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

;;;; Homebrew

(use-package homebrew
  :load-path my-vendor-directory
  :if (eq system-type 'darwin)
  :defer t
  :commands (homebrew-status
             homebrew-dispatch
             homebrew-search
             homebrew-info
             homebrew-install
             homebrew-uninstall
             homebrew-upgrade
             homebrew-upgrade-all
             homebrew-update))

(add-to-list 'display-buffer-alist
             '("\\`\\*homebrew\\(?:-[^*]*\\)?\\*\\'"
               (display-buffer-reuse-window display-buffer-below-selected)
               (window-height . 0.4)))

;;; mac.el ends here
