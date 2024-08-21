;;; LaTeX support
(use-package auctex
  :ensure t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package cdlatex
  :ensure t)
(use-package xenops
  :ensure t)
;; Enable LaTeX math support
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
