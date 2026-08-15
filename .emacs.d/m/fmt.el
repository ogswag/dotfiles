;;; fmt.el --- Formatting -*- lexical-binding: t; -*-

;;; Commentary:
;; formatting.

;;; Code:

(declare-function apheleia--get-formatters "apheleia")
(declare-function apheleia-format-buffer "apheleia")

(use-package apheleia :ensure t
  :commands (apheleia-format-buffer apheleia-mode apheleia-global-mode)
  :hook (after-init . apheleia-global-mode)
  :config
  (add-to-list 'apheleia-mode-alist '(plain-TeX-mode . nil))
  (add-to-list 'apheleia-mode-alist '(lua-mode . stylua)))

(defun my/format-dwim ()
  "Format the buffer with `apheleia', or indent it if no formatter applies."
  (interactive)
  (require 'apheleia)
  (if-let* ((formatters (apheleia--get-formatters)))
      (apheleia-format-buffer formatters)
    (if (use-region-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (point-min) (point-max)))))

;;; fmt.el ends here
