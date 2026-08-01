;;; early-init.el --- Pre-GUI setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs before the package system and before the first frame is created.
;; Only two things belong here: garbage-collection tuning for startup, and
;; frame parameters — setting the latter in init.el makes the initial frame
;; visibly resize.

;;; Code:

;;;; Garbage collection

;; Threshold in effect before we raise it, restored after startup.
;;
;; NOTE: there is deliberately no backup of `gc-cons-percentage'.  Emacs
;; itself sets it to 1.0 in `normal-top-level' for the duration of startup
;; and restores it afterwards, so anything read here is that 1.0, not the
;; stock value — backing it up and "restoring" it would disable GC for the
;; rest of the session.  `my-gc-cons-percentage' below is an explicit value.
(defvar my-backup-gcct gc-cons-threshold)

;; Collecting during startup is pure overhead: almost nothing allocated then
;; is garbage.  Raised to the maximum, restored by `my--restore-gc' below.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defvar my-optimize-startup-gc t
  "If non-nil, increase `gc-cons-threshold' during startup to reduce pauses.
After Emacs finishes loading, `gc-cons-threshold' is restored to the value
stored in `my-gc-cons-threshold'.")

(defvar my-gc-cons-threshold (* 32 1024 1024)
  "Value to which `gc-cons-threshold' is set after Emacs startup.
Ignored if `my-optimize-startup-gc' is nil.")

(defvar my-gc-cons-percentage 0.1
  "Value to which `gc-cons-percentage' is set after Emacs startup.
Stated outright rather than captured — see the note above.  0.1 is the
Emacs default; raise it to trade memory for fewer collections.")

(defvar my-gc-cons-threshold-restore-delay nil
  "Seconds to wait after startup before restoring GC values.
Nil restores them immediately.  A small delay (0.5-1.0) avoids a second
collection while deferred `after-init' work is still running.")

(defun my--restore-gc-values ()
  "Restore garbage collection values to my-emacs.d values."
  (setq gc-cons-threshold my-gc-cons-threshold)
  (setq gc-cons-percentage my-gc-cons-percentage))

(defun my--restore-gc ()
  "Restore garbage collection settings."
  (if (and my-gc-cons-threshold-restore-delay
           ;; In noninteractive mode, the event loop does not run
           (not noninteractive))
      (run-with-timer my-gc-cons-threshold-restore-delay nil
                      #'my--restore-gc-values)
    (my--restore-gc-values)))

(if my-optimize-startup-gc
    ;; `gc-cons-threshold' is managed by my-emacs.d
    (add-hook 'emacs-startup-hook #'my--restore-gc 105)
  ;; gc-cons-threshold is not managed by my-emacs.d.
  (when (= gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold my-backup-gcct)
    (setq gc-cons-percentage my-gc-cons-percentage)))

;;;; Frames
;;
;; Set here rather than in init.el: parameters applied after the initial frame
;; exists cause a visible resize and a flash of chrome we immediately remove.

;; Position the initial frame near the top of the screen.
(push '(top . 100) initial-frame-alist)
(push '(left . 330) initial-frame-alist)

;; Size and chrome for every frame.
(push '(width . 120) default-frame-alist)
(push '(height . 50) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat "Emacs - [p] " (project-name project))
              (concat "Emacs - " (buffer-name))))))


;;;; Misc

(set-language-environment "UTF-8")

;; Read more from subprocesses per chunk (2 MB) — helps LSP-ish and
;; compilation output in particular.
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq package-enable-at-startup nil)  ; m/pkg.el handles this

;;; early-init.el ends here
