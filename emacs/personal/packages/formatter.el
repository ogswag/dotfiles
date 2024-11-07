;; >> FORMAT ALL <<
;; easy formatting using unified commands
(use-package format-all
  :ensure t
  :diminish
  :hook (emacs-lisp-mode c++-ts-mode python-ts-mode))
(setq format-all-formatters
      '(("C++" (clang-format "--style=Google"))
        ("Python" (yapf "--style=Google"))))

