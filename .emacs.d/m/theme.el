;;; theme.el --- Themes and light/dark switching -*- lexical-binding: t; -*-

;;; Commentary:
;; Themes are vendored under m/t/ rather than installed from ELPA.  The load
;; path is built by walking that directory, so dropping a new theme in needs
;; no change here.

;;; Code:

(setq custom-safe-themes t)

(global-font-lock-mode 1)

;; m/t itself plus every immediate subdirectory (base16/, base2tone/, envy.el/,
;; valve-olive.el/, ...).
(let ((root (expand-file-name "m/t" user-emacs-directory)))
  (dolist (dir (cons root
                     (seq-filter #'file-directory-p
                                 (directory-files root t "\\`[^.]"))))
    (add-to-list 'load-path dir)
    (add-to-list 'custom-theme-load-path dir)))

(setq valve-olive-contrast-comments t)

(defvar my/dark-theme 'valve-olive)

(defvar my/light-theme 'envy)

(defun my/system-dark-p ()
  "Non-nil if macOS is in dark mode, nil otherwise (incl. non-Mac builds)."
  (cond
   ((fboundp 'mac-application-state)          ; emacs-mac (Mitsuharu)
    (string-match-p "Dark" (or (plist-get (mac-application-state) :appearance) "")))
   ((boundp 'ns-system-appearance)            ; NS port, Emacs 28+
    (eq ns-system-appearance 'dark))))

(defun my/apply-theme (&rest _)
  "Load the theme matching the current system appearance.
Takes and ignores arguments so it can serve as an appearance-change hook."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (if (my/system-dark-p) my/dark-theme my/light-theme) t))

(my/apply-theme)

(cond
 ((boundp 'mac-effective-appearance-change-hook)
  (add-hook 'mac-effective-appearance-change-hook #'my/apply-theme))
 ((boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)))

;;; theme.el ends here
