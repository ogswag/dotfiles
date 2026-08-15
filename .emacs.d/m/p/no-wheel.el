;;; no-wheel.el --- plain wheel scrolling only, no gestures -*- lexical-binding: t; -*-

;;; Commentary:
;; only plain scroll survives; kill ctrl/alt/pinch/zoom wheel bindings globally.

;;; Code:

;;;; What to keep

(defconst no-wheel-allowed-events
  '("wheel-up" "wheel-down"      ; vertical
    "wheel-left" "wheel-right"   ; horizontal
    "mouse-4" "mouse-5" "mouse-6" "mouse-7")
  "Base events that keep their normal binding when used *without* modifiers.")

;;;; What to kill

(defconst no-wheel-modifiers '("C-" "H-" "M-" "S-" "s-")
  "Modifier prefixes, in Emacs's canonical order (A-C-H-M-S-s).")

(defconst no-wheel-wheel-events
  '("wheel-up" "wheel-down" "wheel-left" "wheel-right"   ; mac/NS/w32 ports
    "mouse-4" "mouse-5" "mouse-6" "mouse-7")             ; X11-style, harmless
  "Wheel and tilt events.  Each also exists as a double-/triple- variant.")

(defconst no-wheel-gesture-events
  '("magnify-up" "magnify-down"                          ; pinch zoom (emacs-mac)
    "rotate-left" "rotate-right"                         ; two-finger rotate
    "swipe-up" "swipe-down" "swipe-left" "swipe-right"   ; swipes
    "pinch")                                             ; Emacs 29+ pinch event
  "Trackpad gesture events.  These are killed with and without modifiers.")

(defconst no-wheel-areas
  '(mode-line header-line tab-line tab-bar vertical-line
    left-margin right-margin left-fringe right-fringe
    vertical-scroll-bar horizontal-scroll-bar
    right-divider bottom-divider)
  "Window areas that can prefix a mouse event, e.g. [mode-line C-wheel-up].")

;;;; Keymap construction

(defun no-wheel--modifier-prefixes (mods)
  "Return all ordered subsets of MODS, concatenated into strings."
  (if (null mods)
      (list "")
    (let ((rest (no-wheel--modifier-prefixes (cdr mods))))
      (append rest (mapcar (lambda (s) (concat (car mods) s)) rest)))))

(defun no-wheel--event-symbols ()
  "Every wheel/gesture event symbol that should be ignored."
  (let (events)
    (dolist (mod (no-wheel--modifier-prefixes no-wheel-modifiers) events)
      (dolist (base no-wheel-wheel-events)
        (unless (and (equal mod "") (member base no-wheel-allowed-events))
          ;; Undefined double-/triple- events fall back to the single-click binding.
          (dolist (click '("" "double-" "triple-"))
            (push (intern (concat mod click base)) events))))
      (dolist (gesture no-wheel-gesture-events)
        (push (intern (concat mod gesture)) events)))))

(defun no-wheel--ignore-map (events)
  "Return a keymap binding each event in EVENTS to `ignore'."
  (let ((map (make-sparse-keymap)))
    (dolist (event events map)
      (define-key map (vector event) #'ignore))))

(defvar no-wheel-map
  (let* ((events (no-wheel--event-symbols))
         (map (no-wheel--ignore-map events)))
    (dolist (area no-wheel-areas map)
      (define-key map (vector area) (no-wheel--ignore-map events))))
  "Keymap binding every unwanted wheel and gesture event to `ignore'.")

(defvar no-wheel-emulation-alist `((no-wheel-mode . ,no-wheel-map))
  "Entry for `emulation-mode-map-alists', so `no-wheel-map' outranks minor-mode and major-mode keymaps.")

;;;; The mode

(define-minor-mode no-wheel-mode
  "Allow plain wheel scrolling only; ignore modified wheel events and gestures."
  :global t
  :init-value nil
  (cond
   (no-wheel-mode
    ;; No modifier entries at all: this alone strips C-wheel text scaling.
    (setq mouse-wheel-scroll-amount '(1)
          mouse-wheel-progressive-speed nil
          mouse-wheel-tilt-scroll t      ; wheel-left/right scroll horizontally
          mouse-wheel-flip-direction t)
    ;; Re-run the setup so the new value takes effect.
    (if (fboundp 'mac-mouse-wheel-mode)
        (funcall #'mac-mouse-wheel-mode 1)  ; emacs-mac: keeps smooth scrolling
      (mouse-wheel-mode 1)))
   (t
    ;; Restore stock wheel behaviour, text scaling included.
    (custom-reevaluate-setting 'mouse-wheel-scroll-amount)
    (if (fboundp 'mac-mouse-wheel-mode)
        (funcall #'mac-mouse-wheel-mode 1)
      (mouse-wheel-mode 1))
    (setq-default scroll-margin 0)
    (setq scroll-margin 0))))

(add-to-list 'emulation-mode-map-alists 'no-wheel-emulation-alist)
(no-wheel-mode 1)


(provide 'no-wheel)
;;; no-wheel.el ends here
