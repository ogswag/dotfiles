(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

(use-package monokai-theme
  :ensure t)

;; (load-theme 'nimbus t)

(custom-set-faces
 ;; make fringe transparent for any theme
 '(fringe ((t (:background nil)))))

(use-package auto-dark
  :custom
  (auto-dark-themes '((monokai) (standard-light)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript t)
  ;; (auto-dark-detection-method nil) ;; dangerous to be set manually
  :hook
  (auto-dark-dark-mode
   . (lambda ()
        ;; something to execute when dark mode is detected
        ))
  (auto-dark-light-mode
   . (lambda ()
        ;; something to execute when light mode is detected
        ))
  :init (auto-dark-mode))
