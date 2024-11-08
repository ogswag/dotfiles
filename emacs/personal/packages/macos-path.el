;; >> EXEC PATH FROM SHELL <<
;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
