;; >> RUSSIAN TECHWRITER <<
;; Second input method
(use-package russian-techwriter
  :ensure t)
(setq-default default-input-method 'russian-techwriter)

;; >> GOD MODE <<
;; No more RSI
;; Minor mode for God-like command entering
;; (similar to minor modal editing mode)
(use-package god-mode
  :ensure t
  :bind (("C-." . #'god-local-mode)))

;; Unset default undo keys
(keymap-global-unset "C-_")
(keymap-global-unset "C-z")

;; Set better `set-mark-command' keybinding
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-@"))
(global-unset-key (kbd "C-s"))
(keymap-global-set "C-s" #'set-mark-command)

;; Stop Emacs from zooming when holding CTRL + Mouse Wheel
(keymap-global-set "<pinch>" 'ignore)
(keymap-global-set "C-<wheel-up>" 'ignore)
(keymap-global-set "C-<wheel-down>" 'ignore)
(keymap-global-set "C-M-<wheel-up>" 'ignore)
(keymap-global-set "C-M-<wheel-down>" 'ignore)

;; Keybindings for toggling frame maximized and fullscreen
(keymap-global-set "C-M-f" #'toggle-frame-maximized)
(keymap-global-set "C-M-S-f" #'toggle-frame-fullscreen)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGE MACOS-SPECIFIC KEYS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unset mouse key
(keymap-global-unset "<drag-mouse-1>")
(keymap-global-unset "C-M-<drag-mouse-1>")
(keymap-global-unset "C-<drag-mouse-1>")
(keymap-global-unset "M-<drag-mouse-1>")

(keymap-global-unset "M-<mouse-1>")
(keymap-global-unset "S-<mouse-1>")

(keymap-global-unset "<down-mouse-1>")
;; (keymap-global-set   "<down-mouse-1>" "<mouse-1>")
(keymap-global-unset "C-M-<down-mouse-1>")
(keymap-global-unset "C-<down-mouse-1>")
(keymap-global-unset "M-<down-mouse-1>")
(keymap-global-unset "C-<drag-mouse-1>")

(keymap-global-set "C--" #'text-scale-adjust) ;; text-scale-adjust
(keymap-global-set "C-0" #'text-scale-adjust) ;; text-scale-adjust
(keymap-global-set "C-=" #'text-scale-adjust) ;; text-scale-adjust

;; Unset shortcuts with the Command Key
(keymap-global-unset "s-&") ;; kill-current-buffer
(keymap-global-unset "s-'") ;; next-window-any-frame
(keymap-global-unset "s-+") ;; text-scale-adjust
(keymap-global-unset "s-,") ;; customize
(keymap-global-unset "s--") ;; text-scale-adjust
(keymap-global-unset "s-0") ;; text-scale-adjust
(keymap-global-unset "s-:") ;; ispell
(keymap-global-unset "s-<kp-bar>") ;; shell-command-on-region
(keymap-global-unset "s-<left>")   ;;	move-beginning-of-line
(keymap-global-unset "s-<right>")  ;; move-end-of-line
(keymap-global-unset "s-=") ;; text-scale-adjust
(keymap-global-unset "s-?") ;; info
(keymap-global-unset "s-C") ;; ns-popup-color-panel
(keymap-global-unset "s-D") ;; dired
(keymap-global-unset "s-E") ;; edit-abbrevs
(keymap-global-unset "s-F") ;; isearch-backward
(keymap-global-unset "s-H") ;; ns-do-hide-others
(keymap-global-unset "s-L") ;; shell-command
(keymap-global-unset "s-M") ;; manual-entry
(keymap-global-unset "s-S") ;; ns-write-file-using-panel
(keymap-global-unset "s-^") ;; kill-some-buffers
(keymap-global-unset "s-`") ;; other-frame
(keymap-global-unset "s-a") ;; disable mark-whole-buffer
(keymap-global-unset "s-c") ;; ns-copy-including-secondary
(keymap-global-unset "s-d") ;; isearch-repeat-backward
(keymap-global-unset "s-e") ;; isearch-yank-kill
(keymap-global-unset "s-f") ;; isearch-forward
(keymap-global-unset "s-g") ;; isearch-repeat-forward
(keymap-global-unset "s-h") ;; ns-do-hide-emacs
(keymap-global-unset "s-j") ;; exchange-point-and-mark
(keymap-global-unset "s-k") ;; kill-current-buffer
(keymap-global-unset "s-l") ;; goto-line
(keymap-global-unset "s-m") ;; iconify-frame
(keymap-global-unset "s-n") ;; make-frame
(keymap-global-unset "s-o") ;; ns-open-file-using-panel
(keymap-global-unset "s-p") ;; ns-print-buffer
(keymap-global-unset "s-q") ;; save-buffers-kill-emacs
(keymap-global-unset "s-s") ;; disable save-buffer
(keymap-global-unset "s-t") ;; menu-set-font
(keymap-global-unset "s-u") ;; revert-buffer
(keymap-global-unset "s-v") ;; yank
(keymap-global-unset "s-w") ;; delete-frame
(keymap-global-unset "s-x") ;; kill-region
(keymap-global-unset "s-y") ;; ns-paste-secondary
(keymap-global-unset "s-z") ;; disable undo
(keymap-global-unset "s-|") ;; shell-command-on-region
(keymap-global-unset "s-~") ;; ns-prev-frame
