(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

(load-theme 'nimbus t)

(custom-set-faces
 ;; make fringe transparent for any theme
 '(fringe ((t (:background nil)))))
