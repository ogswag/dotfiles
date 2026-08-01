;;; no-wheel.el --- plain wheel scrolling only, no gestures -*- lexical-binding: t; -*-

;;; Commentary:
;; Keeps exactly one thing: scrolling the buffer up/down and left/right with an
;; unmodified wheel or a two-finger trackpad swipe.  Everything else that the
;; wheel/trackpad can trigger is swallowed: C-wheel and C-M-wheel text scaling,
;; S-wheel horizontal scrolling, M-wheel, pinch-to-zoom, rotation, swipes.
;;
;; The dead bindings live in an emulation keymap, so they shadow major-mode and
;; package maps too (pdf-tools, doc-view, image-mode, ...), not only the global
;; map.  Plain wheel events are deliberately left *unbound* in that keymap so
;; they fall through to the normal scrolling machinery — on the emacs-mac port
;; that means the port's own smooth scrolling keeps working.
;;
;; `M-x no-wheel-mode' toggles the whole thing.

;;; Code:

;;;; What to keep

(defconst no-wheel-allowed-events
  '("wheel-up" "wheel-down"      ; vertical
    "wheel-left" "wheel-right"   ; horizontal
    "mouse-4" "mouse-5" "mouse-6" "mouse-7")
  "Base events that keep their normal binding when used *without* modifiers.
Drop \"wheel-left\"/\"wheel-right\" here to kill horizontal scrolling again.")

;;;; What to kill

(defconst no-wheel-modifiers '("C-" "H-" "M-" "S-" "s-")
  "Modifier prefixes, in Emacs's canonical order (A-C-H-M-S-s).
On macOS: control, fn/hyper, option, shift, command.
Prepend \"A-\" if your setup can actually produce alt events.")

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
  "Every wheel/gesture event symbol that should be ignored.
Unmodified events listed in `no-wheel-allowed-events' are left out, so they
keep falling through to the normal scroll bindings."
  (let (events)
    (dolist (mod (no-wheel--modifier-prefixes no-wheel-modifiers) events)
      (dolist (base no-wheel-wheel-events)
        (unless (and (equal mod "") (member base no-wheel-allowed-events))
          ;; Undefined double-/triple- events fall back to the single-click
          ;; binding, so the allowed ones must stay unbound here as well.
          (dolist (click '("" "double-" "triple-"))
            (push (intern (concat mod click base)) events))))
      (dolist (gesture no-wheel-gesture-events)
        (push (intern (concat mod gesture)) events)))))

(defun no-wheel--ignore-map (events)
  "Return a keymap binding each event in EVENTS to `ignore'."
  (let ((map (make-sparse-keymap)))
    (dolist (event events map)
      ;; `ignore' swallows the event silently.  Use #'undefined instead if you
      ;; would rather hear a beep while checking that this file works.
      (define-key map (vector event) #'ignore))))

(defvar no-wheel-map
  (let* ((events (no-wheel--event-symbols))
         (map (no-wheel--ignore-map events)))
    (dolist (area no-wheel-areas map)
      (define-key map (vector area) (no-wheel--ignore-map events))))
  "Keymap binding every unwanted wheel and gesture event to `ignore'.")

(defvar no-wheel-emulation-alist `((no-wheel-mode . ,no-wheel-map))
  "Entry for `emulation-mode-map-alists', so `no-wheel-map' outranks
minor-mode and major-mode keymaps.")

;;;; The mode

(define-minor-mode no-wheel-mode
  "Allow plain wheel scrolling only; ignore modified wheel events and gestures."
  :global t
  :init-value nil
  (cond
   (no-wheel-mode
    ;; No modifier entries at all: this alone strips C-wheel text scaling,
    ;; S-wheel hscroll and M-wheel from the global map.  The emulation map
    ;; above is the second line of defence, for packages that rebind them.
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

;; `scroll-margin' is capped at `maximum-scroll-margin' (0.25 by default), i.e.
;; a quarter of the window height, so 10 lines only take full effect in windows
;; of 40+ lines.  Raise the cap if you want the margin honoured in short
;; windows too:
;; (setq maximum-scroll-margin 0.4)

;; Note: `pixel-scroll-precision-mode' is left untouched.  It does its own wheel
;; scrolling and does not honour `scroll-margin' the same way, so turn it off if
;; the margin misbehaves: (pixel-scroll-precision-mode -1)

;; Optional, emacs-mac only: discrete instead of smooth scrolling.
;; (setq mac-mouse-wheel-smooth-scroll nil)

;; If some gesture still does something, find its real event name with
;; `C-h k' followed by the gesture, or `M-x view-lossage' right after it,
;; then add the base name to `no-wheel-gesture-events' and re-evaluate.

(provide 'no-wheel)
;;; no-wheel.el ends here
