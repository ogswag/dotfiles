;;; keys.el --- keybindings config -*- no-byte-compile: t; lexical-binding: t; -*-

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
(keymap-global-unset "s-j") ;; exchange-point-and-mark
(keymap-global-unset "s-k") ;; kill-current-buffer
(keymap-global-unset "s-l") ;; goto-line
(keymap-global-unset "s-n") ;; make-frame
(keymap-global-unset "s-o") ;; ns-open-file-using-panel
(keymap-global-unset "s-p") ;; ns-print-buffer
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
(keymap-global-unset "s-h") ;; ns-do-hide-emacs
(keymap-global-unset "s-m") ;; iconify-frame


;; Stop Emacs from zooming when holding CTRL + Mouse Wheel
(keymap-global-set "<pinch>" 'ignore)
(keymap-global-set "C-<wheel-up>" 'ignore)
(keymap-global-set "C-<wheel-down>" 'ignore)
(keymap-global-set "C-M-<wheel-up>" 'ignore)
(keymap-global-set "C-M-<wheel-down>" 'ignore)


;; Unset mouse keys
;; (keymap-global-unset "<drag-mouse-1>")
;; (keymap-global-unset "<down-mouse-1>")
;; (keymap-global-set   "<down-mouse-1>" "<mouse-1>")
(keymap-global-unset "C-<down-mouse-1>")
(keymap-global-unset "C-<drag-mouse-1>")
(keymap-global-unset "C-<drag-mouse-1>")
(keymap-global-unset "C-M-<down-mouse-1>")
(keymap-global-unset "C-M-<drag-mouse-1>")
(keymap-global-unset "M-<down-mouse-1>")
(keymap-global-unset "M-<drag-mouse-1>")
(keymap-global-unset "M-<mouse-1>")
(keymap-global-unset "S-<mouse-1>")


;; Frame, Window and Buffer Shortcuts
(when (and (display-graphic-p) (eq system-type 'darwin))
  (keymap-global-set "s-\\ m" #'iconify-frame)
  (keymap-global-set "s-\\ h" #'ns-do-hide-emacs)
  (keymap-global-set "s-\\ n" #'make-frame)
  (keymap-global-set "s-\\ k" #'delete-frame)
  (keymap-global-set "s-\\ f" #'toggle-frame-maximized)
  (keymap-global-set "s-\\ F" #'toggle-frame-fullscreen)
  ;; Windows
  (keymap-global-set "s-1" #'delete-other-windows)
  (keymap-global-set "s-0" #'delete-window)
  (keymap-global-set "s-8" #'kill-current-buffer)
  (keymap-global-set "s-9" #'kill-buffer-and-window)
  (keymap-global-set "s-2" #'next-window-any-frame)
  (keymap-global-set "s-3" #'split-window-horizontally)
  (keymap-global-set "s-4" #'split-window-vertically)
  (keymap-global-set "s-=" #'balance-windows)
  ;; Buffer
  (keymap-global-set "s-5" #'previous-buffer)
  (keymap-global-set "s-6" #'next-buffer))

;; Text scaling
(keymap-global-set "C--" #'global-text-scale-adjust) ;; text-scale-adjust
(keymap-global-set "C-0" #'global-text-scale-adjust) ;; text-scale-adjust
(keymap-global-set "C-=" #'global-text-scale-adjust) ;; text-scale-adjust

;; Set better `set-mark-command' keybinding
(keymap-global-unset "C-SPC")
(keymap-global-unset "C-@")
(keymap-global-unset "C-s")
(keymap-global-set "C-s" #'set-mark-command)

;; Undo-fu
(keymap-global-unset "C-z")
(keymap-global-set "s-[" #'undo-fu-only-undo)
(keymap-global-set "s-]" #'undo-fu-only-redo)

;; Affe
(keymap-global-set "s-f" #'affe-find)
(keymap-global-set "s-g" #'affe-grep)

;; Compilation and build commands
(keymap-global-set "s-c" #'compile)
(keymap-global-set "s-r" #'async-shell-command)

;; Bookmarks
(keymap-global-set "s-b" #'consult-bookmark)


;; -----------------------------------------------------------------------------
;;;; EDITING
;; -----------------------------------------------------------------------------

(keymap-global-set "s-j" #'join-line)

(use-package puni
  :ensure t
  :defer t)

(setq-default puni-confirm-when-delete-unbalanced-active-region 'f)

(require 'puni)

;; New function to search forward for delimiter
(defun az/change-forward (char &optional mode)
  "Search forward for CHAR and delete sexp at that position.
With MODE 'outer, delete entire sexp (including delimiters).
Otherwise delete only the inner content."
  (interactive "cSearch for delimiter: \nP")
  (let ((search-string (string char)))
    ;; Search forward across entire buffer
    (when (search-forward search-string nil t)
      (backward-char) ; Move back to the delimiter
      ;; Get bounds of the expression
      (let* ((bounds (puni-bounds-of-sexp-at-point))
             (rb (car bounds))
             (re (cdr bounds)))
        (if (eq mode 'outer)
            (kill-region rb re) ; Delete entire expression
          ;; Delete inner content
          (let* ((insides (progn (goto-char (1+ rb))
                                 (puni-bounds-of-list-around-point)))
                 (olen (- (car insides) rb))
                 (clen (- re (cdr insides))))
            (kill-region (+ rb olen) (- re clen))))))))

;; New function to search forward and delete inner content
(defun az/change-forward-inside (char)
  "Search forward for CHAR and delete inner content of sexp."
  (interactive "cSearch for delimiter: ")
  (az/change-forward char))

;; New function to search forward and delete entire sexp
(defun az/change-forward-around (char)
  "Search forward for CHAR and delete entire sexp (including delimiters)."
  (interactive "cSearch for delimiter: ")
  (az/change-forward char 'outer))

;; Keybindings
(keymap-global-set "s-d" #'az/change-forward-inside)
(keymap-global-set "s-a" #'az/change-forward-around)

;; -----------------------------------------------------------------------------
;;;; MOVEMENT
;; -----------------------------------------------------------------------------
