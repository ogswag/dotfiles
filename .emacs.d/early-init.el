;; -*- lexical-binding: t; -*-

;; Backup of `gc-cons-threshold' and `gc-cons-percentage' before startup.
(defvar my-backup-gcct gc-cons-threshold)
(defvar my-backup-gccp gc-cons-percentage)

;; Backup of `gc-cons-threshold' and `gc-cons-percentage' before startup.
(defvar my-backup-gcct gc-cons-threshold)

;; Temporarily raise the garbage collection threshold to its maximum value.
;; It will be restored later to controlled values.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defvar my-optimize-startup-gc t
  "If non-nil, increase `gc-cons-threshold' during startup to reduce pauses.
After Emacs finishes loading, `gc-cons-threshold' is restored to the value
stored in `my-gc-cons-threshold'.")

(defvar my-gc-cons-threshold (* 32 1024 1024)
  "Value to which `gc-cons-threshold' is set after Emacs startup.
Ignored if `my-optimize-startup-gc' is nil.")

(defvar my-gc-cons-percentage gc-cons-percentage
  "Value to which `gc-cons-percentage' is set after Emacs startup.")

(defun my--restore-gc-values ()
  "Restore garbage collection values to my-emacs.d values."
  (setq gc-cons-threshold my-gc-cons-threshold)
  (setq gc-cons-percentage my-gc-cons-percentage))

(defun my--restore-gc ()
  "Restore garbage collection settings."
  (if (and (bound-and-true-p my-gc-cons-threshold-restore-delay)
           ;; In noninteractive mode, the event loop does not run
           (not noninteractive))
      ;; Defer garbage collection during initialization to avoid 2 collections.
      (run-with-timer my-gc-cons-threshold-restore-delay nil
                      #'my--restore-gc-values)
    (my--restore-gc-values)))

(if my-optimize-startup-gc
    ;; `gc-cons-threshold' is managed by my-emacs.d
    (add-hook 'emacs-startup-hook #'my--restore-gc 105)
  ;; gc-cons-threshold is not managed by my-emacs.d.
  (when (= gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold my-baackup-gcct)
    (setq gc-cons-percentage my-backup-gccp)))

(set-language-environment "UTF-8")

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

(setq process-adaptive-read-buffering nil)

(setq package-enable-at-startup nil)  ; Let the init.el file handle this
