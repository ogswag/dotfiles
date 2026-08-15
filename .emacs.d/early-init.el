;;; early-init.el --- Pre-GUI setup -*- lexical-binding: t; -*-

;;; Commentary:
;; pre-frame stuff only - gc + frame params so the window doesn't jump.

;;; Code:

;;;; Garbage collection

(defvar my-backup-gcct gc-cons-threshold)

;; Collecting during startup is pure overhead: almost nothing allocated then
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defvar my-optimize-startup-gc t
  "If non-nil, raise `gc-cons-threshold' during startup to reduce pauses.")

(defvar my-gc-cons-threshold (* 32 1024 1024)
  "`gc-cons-threshold' after startup.  Ignored if `my-optimize-startup-gc' is nil.")

(defvar my-gc-cons-percentage 0.1
  "`gc-cons-percentage' after startup.  0.1 is the Emacs default.")

(defvar my-gc-cons-threshold-restore-delay nil
  "Seconds to wait after startup before restoring GC values, nil for immediately.")

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
    ;; `gc-cons-threshold' is managed by my-emacs.d.
    (add-hook 'emacs-startup-hook #'my--restore-gc 105)
  ;; gc-cons-threshold is not managed by my-emacs.d.
  (when (= gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold my-backup-gcct)
    (setq gc-cons-percentage my-gc-cons-percentage)))

;;;; Frames

;; Position the initial frame near the top of the screen.
(push '(top . 25) initial-frame-alist)
(push '(left . 100) initial-frame-alist)

;; Size and chrome for every frame.
(push '(width . 120) default-frame-alist)
(push '(height . 40) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(add-hook 'emacs-startup-hook
          (lambda ()
            (dolist (parameter '(width height))
              (when-let* ((value (alist-get parameter default-frame-alist)))
                (set-frame-parameter nil parameter value)))))

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

;; Read more from subprocesses per chunk (2 MB) - helps LSP-ish.
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq package-enable-at-startup nil)  ; m/pkg.el handles this

;;; early-init.el ends here
