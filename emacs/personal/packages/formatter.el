;; >> FORMAT ALL <<
;; easy formatting using unified commands
(use-package format-all
  :ensure t
  :diminish
  :hook (emacs-lisp-mode c++-mode c-mode python-mode))
(setq format-all-formatters
      '(("C++" (clang-format "--style=Google"))
        ("Python" (ruff "--config=~/ruff.conf.toml" "format"))))
