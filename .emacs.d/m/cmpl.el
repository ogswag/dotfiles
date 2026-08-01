;;; cmpl.el --- Completion and discoverability -*- lexical-binding: t; -*-

;;; Code:

(use-package which-key :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(use-package vertico :ensure t
  :init (vertico-mode))

(use-package vertico-directory :ensure nil ; ships with vertico
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)       ; Enter directories
              ("DEL" . vertico-directory-delete-char) ; Smart backspace
              ("C-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia :ensure t
  :after vertico
  :commands (marginalia-mode marginalia-cycle)
  :init (marginalia-mode))

;;; cmpl.el ends here
