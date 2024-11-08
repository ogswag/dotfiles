;; Simplified dired setup
(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :hook
  (dired-mode . dired-hide-details-mode))

