;; Default frame size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 100) ; chars
              (height . 30) ; lines
              ))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 100)
              (height . 30))))
  (progn
    (setq initial-frame-alist '((tool-bar-lines . 0)))
    (setq default-frame-alist '((tool-bar-lines . 0)))))

(tool-bar-mode 0)    ;; No toolbar
(scroll-bar-mode 0)  ;; No scroll bars
(context-menu-mode 1) ;; Enable right click menus

(use-package ns-auto-titlebar
  :ensure t)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; Resize frames and windows by pixels, not by chars
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(use-package ace-window
  :ensure t)

(keymap-global-set "C-c w j" #'ace-window)
;; You can also start by calling ace-window and then decide to switch the action to delete or swap etc. By default the bindings are:

;;     x - delete window
;;     m - swap windows
;;     M - move window
;;     c - copy window
;;     j - select buffer
;;     n - select the previous window
;;     u - select buffer in the other window
;;     c - split window fairly, either vertically or horizontally
;;     v - split window vertically
;;     b - split window horizontally
;;     o - maximize current window
;;     ? - show these command bindings

(use-package diminish-buffer
  :ensure t)
(diminish-buffer-mode 1)
(setq diminish-buffer-list '("*Messages*"
                             "*Quail Comletions*"
                             "*Compile-Log*"))
