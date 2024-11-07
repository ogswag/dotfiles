;;; completion-bundle.el --- LSP and Minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:
;;; LSP, Minibuffer and other completion related settings in one package

;; >> MARGINALIA <<
;; Marginalia (documentation and notes) in the minibuffer
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(provide 'completion-bundle)
;;; completion-bundle.el ends here
