;; My keybindings config -*- no-byte-compile: t; lexical-binding: t; -*-
;; CODE

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; UNBINDS
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
(keymap-global-unset "C-M-<wheel-down>") ; mouse-wheel-global-text-scale
(keymap-global-unset "C-M-<wheel-up>")   ; mouse-wheel-global-text-scale
(keymap-global-unset "C-<wheel-down>")   ; mouse-wheel-text-scale
(keymap-global-unset "C-<wheel-up>")     ; mouse-wheel-text-scale
(keymap-global-unset "C-<mouse-5>")      ; mouse-wheel-text-scale down
(keymap-global-unset "C-<mouse-4>")      ; mouse-wheel-text-scale up
(keymap-global-unset "C-M-<mouse-5>")    ; mouse-wheel-global-text-scale down
(keymap-global-unset "C-M-<mouse-4>")    ; mouse-wheel-global-text-scale up

(keymap-global-set "M-l" #'downcase-dwim)  ; downcase-word
(keymap-global-set "M-u" #'upcase-dwim)  ; upcase-word
(keymap-global-set "M-c" #'capitalize-dwim)  ; capitalize-word

(keymap-global-unset "<mouse-2>")   ; middle mouse button secondary yank
(keymap-global-unset "M-<mouse-1>") ; set secondary selection start
(keymap-global-unset "M-<mouse-3>") ; set secondary selection end

(keymap-global-unset "C-M-<down-mouse-1>")

(keymap-global-unset "C-z")
(keymap-global-unset "M-<drag-mouse-1>")
(keymap-global-unset "M-<mouse-1>")
;; (keymap-global-unset "M-i")
;; (keymap-global-unset "M-j")
;; (keymap-global-unset "M-m")
;; (keymap-global-unset "M-o")
;; (keymap-global-unset "M-{") ; backward-paragraph
;; (keymap-global-unset "M-}") ; forward-paragraph
;; (keymap-global-unset "s--")
;; (keymap-global-unset "s-0")
;; (keymap-global-unset "s-<left>")
;; (keymap-global-unset "s-<right>")
;; (keymap-global-unset "s-=")
;; (keymap-global-unset "s-Z")
(keymap-global-unset "s-L")
;; (keymap-global-unset "s-q")
;; (keymap-global-unset "s-z")
;; (keymap-global-unset  "s-j")
;; (keymap-global-unset "s-k")


;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; GENERAL
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; (keymap-global-set "M-8" #'toggle-frame-fullscreen)
;; (keymap-global-set "M-0" #'iconify-frame)

;;;; macOS-standard Super (Cmd) bindings
(keymap-global-set "s-c" #'kill-ring-save)
(keymap-global-set "s-x" #'kill-region)
(keymap-global-set "s-v" #'yank)
(keymap-global-set "s-V" #'consult-yank-pop)
(keymap-global-set "s-z" #'undo-fu-only-undo)
(keymap-global-set "s-Z" #'undo-fu-only-redo)
(keymap-global-set "s-a" #'mark-whole-buffer)
(keymap-global-set "s-s" #'save-buffer)
(keymap-global-set "s-S" #'write-file)
(keymap-global-set "s-l" #'my/mark-line)
(keymap-global-set "s-L" #'goto-line)
(keymap-global-set "s-f" #'isearch-forward)
(keymap-global-set "s-F" #'isearch-backward)
(keymap-global-set "s-g" #'isearch-repeat-forward)
(keymap-global-set "s-w" #'delete-window)
(keymap-global-set "s-W" #'delete-frame)
(keymap-global-set "s-n" #'make-frame-command)
;; (keymap-global-set "s-m" #'iconify-frame)
(keymap-global-set "s-`" #'other-frame)
(keymap-global-set "C-s-f" #'toggle-frame-fullscreen)

(keymap-global-unset "C-z" t)
(keymap-global-unset "C-Z" t)
(keymap-global-unset "C-z" t)
(keymap-global-unset "C-S-z" t)
;; (keymap-global-set "C-z" #'undo-fu-only-undo)
;; (keymap-global-set "C-S-z" #'undo-fu-only-redo)

(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)
(keymap-global-set "C-0" #'text-scale-adjust)

(keymap-global-set "M-[" #'backward-paragraph)
(keymap-global-set "M-]" #'forward-paragraph)

(keymap-global-unset "C-<left>")
(keymap-global-unset "C-<right>")
(keymap-global-unset "C-M-/" t)

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; SET ARROW KEYS IN ISEARCH
;; left/right is backward/forward.
;; up/down is search history.
;; This way, searching forward/backward is just one single key press, no key combination.
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
(keymap-set isearch-mode-map "<up>" #'isearch-ring-retreat)
(keymap-set isearch-mode-map "<down>" #'isearch-ring-advance)

(keymap-set isearch-mode-map "<left>" #'isearch-repeat-backward)
(keymap-set isearch-mode-map "<right>" #'isearch-repeat-forward)

(keymap-set isearch-mode-map "C-p" #'isearch-repeat-backward)
(keymap-set isearch-mode-map "C-n" #'isearch-repeat-forward)

(keymap-set minibuffer-local-isearch-map "<left>" #'isearch-reverse-exit-minibuffer)
(keymap-set minibuffer-local-isearch-map "<right>" #'isearch-forward-exit-minibuffer)

(keymap-set minibuffer-local-map "C-p" #'previous-line-or-history-element)
(keymap-set minibuffer-local-map "C-n" #'next-line-or-history-element)

(require 'viper-cmd)
(keymap-global-set "M-<left>" #'viper-backward-word)
(keymap-global-set "M-<right>" #'viper-forward-word)
(keymap-global-set "M-b" #'viper-backward-word)
(keymap-global-set "M-f" #'viper-forward-word)

(defun my/backward-delete-word (arg)
  "Delete (not kill) ARG words backward."
  (interactive "p")
  (delete-region (point) (progn (viper-backward-word arg) (point))))
(defun my/forward-delete-word (arg)
  "Delete (not kill) ARG words forward."
  (interactive "p")
  (delete-region (point) (progn (viper-forward-word arg) (point))))
(keymap-global-set "M-<backspace>" #'my/backward-delete-word)
(keymap-global-set "C-<backspace>" #'my/backward-delete-word)
(keymap-global-set "M-d" #'my/forward-delete-word)
(keymap-global-set "C-<delete>" #'my/forward-delete-word)

(keymap-global-set "M-S-<down-mouse-1>" #'mouse-drag-region-rectangle)

(keymap-global-set "M-j" #'join-line)

(keymap-global-unset "C-/")
(keymap-global-set "C-/" #'comment-line)

(defun my/mark-line (arg)
  "Delete (not kill) ARG words forward."
  (interactive "p")
  (set-mark-command (beginning-of-line))
  (end-of-line))

(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(keymap-global-set "C-g" #'my/keyboard-quit-dwim)

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; TURN ESCAPE INTO A LEADER KEY
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
(define-prefix-command 'my-esc-map)
(keymap-global-set "<escape>" 'my-esc-map)

(keymap-set my-esc-map "<ESC>" #'keyboard-quit)

(keymap-set my-esc-map "f r" #'recentf-open)

(keymap-set my-esc-map "r s" #'replace-string)
(keymap-set my-esc-map "r r" #'replace-regexp)

(keymap-set my-esc-map "c c" #'compile)
(keymap-set my-esc-map "c r" #'recompile)

(keymap-set my-esc-map "1" #'shell-command)
(keymap-set my-esc-map "2" #'async-shell-command)

(defun my/open-curdir ()
  "Open the current file's directory in Finder on macOS."
  (interactive)
  (let ((dir (or (and buffer-file-name
                      (file-name-directory buffer-file-name))
                 default-directory)))
    (shell-command (concat "open " (shell-quote-argument dir)))))
(keymap-set my-esc-map "3" #'my/open-curdir)

;;; input.el ends here
