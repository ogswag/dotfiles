;; >> FLYMAKE <<
;; Inline static analysis
(use-package flymake
  :diminish flymake-mode
  :hook (prog-mode))
(use-package flymake-shell
  :ensure t)

