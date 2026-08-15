;;; theme.el --- Themes and light/dark switching -*- lexical-binding: t; -*-

;;; Commentary:
;; theme load + light/dark follow system.

;;; Code:

(setq custom-safe-themes t)

(let ((root (expand-file-name "m/t" my-config-directory)))
  (dolist (dir (cons root
                     (seq-filter #'file-directory-p
                                 (directory-files root t "\\`[^.]"))))
    (add-to-list 'load-path dir)
    (add-to-list 'custom-theme-load-path dir)))

(setq valve-olive-contrast-comments t)

(defvar my/dark-theme 'modus-vivendi)

(defvar my/light-theme 'envy)

(defun my/system-dark-p ()
  "Non-nil if macOS is in dark mode, nil otherwise (including non-Mac builds)."
  (cond
   ((fboundp 'mac-application-state)          ; emacs-mac (Mitsuharu)
    (string-match-p "Dark" (or (plist-get (mac-application-state) :appearance) "")))
   ((boundp 'ns-system-appearance)            ; NS port, Emacs 28+
    (eq ns-system-appearance 'dark))))

(defun my/apply-theme (&rest _)
  "Load the theme matching the current system appearance."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (if (my/system-dark-p) my/dark-theme my/light-theme) t))

(my/apply-theme)

(cond
 ((boundp 'mac-effective-appearance-change-hook)
  (add-hook 'mac-effective-appearance-change-hook #'my/apply-theme))
 ((boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)))

;;;; Colours derived from the one in force

(require 'color)

(defun my/theme-rgb (colour)
  "COLOUR as three floats from 0 to 1, or nil if it cannot be read."
  (when (and (stringp colour) (not (string-prefix-p "unspecified" colour)))
    (or (when (string-match "\\`#\\([[:xdigit:]]\\{6\\}\\)\\'" colour)
          (let ((n (string-to-number (match-string 1 colour) 16)))
            (list (/ (ash n -16) 255.0)
                  (/ (logand (ash n -8) 255) 255.0)
                  (/ (logand n 255) 255.0))))
        (ignore-errors (color-name-to-rgb colour)))))

(defun my/theme-hex (colour)
  "COLOUR as #rrggbb, or nil when it cannot be read."
  (when-let* ((rgb (my/theme-rgb colour)))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

(defun my/theme-dark-p ()
  "Non-nil when the theme in force is a dark one."
  (when-let* ((rgb (my/theme-rgb (face-attribute 'default :background nil t))))
    (< (+ (* 0.299 (nth 0 rgb)) (* 0.587 (nth 1 rgb)) (* 0.114 (nth 2 rgb)))
       0.5)))

(defun my/theme-shade (percent)
  "The buffer background moved PERCENT of the way towards the other end."
  (let ((background (face-attribute 'default :background nil t))
        (fraction (/ percent 100.0))
        (up (my/theme-dark-p)))
    (if-let* ((rgb (my/theme-rgb background)))
        (apply #'color-rgb-to-hex
               (append (mapcar (lambda (channel)
                                 (if up
                                     (+ channel (* (- 1.0 channel) fraction))
                                   (- channel (* channel fraction))))
                               rgb)
                       '(2)))
      background)))

;;; theme.el ends here
