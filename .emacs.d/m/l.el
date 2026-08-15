;;; l.el --- TeX: which mode, how it builds, where it is viewed -*- lexical-binding: t; -*-

;;; Commentary:
;; tex defaults: mode, compile, output dir, skim sync. optex stuff is m/optex.el.

;;; Code:

(eval-when-compile
  (require 'tex nil t)
  (require 'plain-tex nil t)
  (require 'latex nil t)
  (require 'tex-mode nil t))      ; the built-in one, for `tex-mode-map'

(declare-function TeX-active-master "tex" (&optional extension nondirectory ignore))
(declare-function TeX-active-process "tex" ())
(declare-function TeX-command "tex" (name file-fn &optional override-confirm))
(declare-function TeX-error-report-has-errors-p "tex" ())
(declare-function TeX-master-file "tex" (&optional extension nondirectory ask))
(declare-function TeX-output-extension "tex" ())
(declare-function TeX-view "tex" ())
(declare-function my/math-insert "math" ())
(declare-function laas-mathp "laas" ())
(declare-function laas-object-on-left-condition "laas" ())
(declare-function laas-wrap-previous-object "laas" (tex-command))
(declare-function aas-set-snippets "aas" (name &rest args))
(declare-function texmathp "texmathp" ())
(declare-function my/math-in-math-p "math" (&optional pos))


;;;; Which mode a .tex file opens in

(setq TeX-force-default-mode nil)

;; Only reached for a file with no content to judge: a new, empty one.  Those are OpTeX here.
(setq TeX-default-mode #'plain-TeX-mode)

;; Same choice for the built-in dispatcher, for the paths where AUCTeX's remap
(setq tex-default-mode #'plain-tex-mode)


;;;; Plain TeX here means OpTeX

(with-eval-after-load 'tex
  (add-to-list 'TeX-engine-alist '(optex "OpTeX" "optex" "optex" "optex")))

(defun my/tex-use-optex-engine ()
  "Compile this buffer with optex rather than with plain pdftex."
  (setq-local TeX-engine 'optex))

(add-hook 'plain-TeX-mode-hook #'my/tex-use-optex-engine)

(defconst my/optex-rerun-regexp "TeX me again"
  "OpTeX's way of saying the run must be repeated.")

(defun my/optex-rerun-if-asked (_process name)
  "Offer NAME again when OpTeX reported that a further pass is needed."
  (when (and (string= name "TeX")
             (buffer-live-p TeX-command-buffer)
             (with-current-buffer TeX-command-buffer (eq TeX-engine 'optex))
             (save-excursion
               (goto-char (point-min))
               (re-search-forward my/optex-rerun-regexp nil t)))
    (setq TeX-command-next name)))

(with-eval-after-load 'tex
  (advice-add 'TeX-TeX-sentinel :after #'my/optex-rerun-if-asked))


;;;; Where the build files go

(setq-default TeX-output-dir "out")

;; OpTeX's cross-reference file.
(with-eval-after-load 'plain-tex
  (add-to-list 'plain-TeX-clean-intermediate-suffixes "\\.ref"))

(setq TeX-auto-save nil
      TeX-parse-self nil)

;; Save the document without asking first.  With live preview on, the prompt
(setq TeX-save-query nil)
(setq TeX-clean-confirm nil)


;;;; Keeping the source and the PDF in sync

(setq TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server t)

(with-eval-after-load 'tex
  (add-to-list 'TeX-view-program-list
               '("Skim" "displayline -r -b %n %o %b" "displayline"))
  (add-to-list 'TeX-view-program-list
               '("Skim (background)" "displayline -r -b -g %n %o %b"
                 "displayline")))

(setq TeX-view-program-selection
      '((output-pdf "Skim")
        (output-dvi "open")
        (output-html "open")))


;;;; Live preview

(defcustom my/tex-live-delay 0.6
  "Seconds to wait after a save before starting a live-preview build."
  :type 'number
  :group 'TeX-command)

(defvar my/tex-live--timer nil
  "Pending build, so that a burst of saves collapses into one run.")

(defvar my/tex-live-mode)

(defun my/tex-live--schedule (buffer)
  "Arrange for BUFFER's document to be built in `my/tex-live-delay' seconds."
  (when (timerp my/tex-live--timer) (cancel-timer my/tex-live--timer))
  (setq my/tex-live--timer
        (run-with-timer my/tex-live-delay nil #'my/tex-live--build buffer)))

(defun my/tex-live--build (buffer)
  "Compile the document BUFFER belongs to."
  (when (and my/tex-live-mode (buffer-live-p buffer))
    (with-current-buffer buffer
      (if (TeX-active-process)
          ;; One already in flight.
          (my/tex-live--schedule buffer)
        (TeX-command TeX-command-default #'TeX-master-file 0)))))

(defun my/tex-live--after-save ()
  "Queue a live-preview build for this buffer.  For `after-save-hook'."
  (when (and my/tex-live-mode
             (derived-mode-p 'TeX-mode)
             (buffer-file-name))
    (my/tex-live--schedule (current-buffer))))

(defun my/tex-run-compilation-finished (_process _name)
  "Announce a finished plain-TeX build the way the LaTeX sentinel does."
  (unless (TeX-error-report-has-errors-p)
    (run-hook-with-args 'TeX-after-compilation-finished-functions
                        (with-current-buffer TeX-command-buffer
                          (expand-file-name
                           (TeX-active-master (TeX-output-extension)))))))

(defun my/tex-live--view (&rest _)
  "Send the viewer to the line point is on, without raising it."
  (when (and my/tex-live-mode (buffer-live-p TeX-command-buffer))
    (with-current-buffer TeX-command-buffer
      (when (derived-mode-p 'TeX-mode)
        (let ((TeX-view-program-selection
               '((output-pdf "Skim (background)")
                 (output-dvi "open")
                 (output-html "open"))))
          (TeX-view))))))

;;;###autoload
(define-minor-mode my/tex-live-mode
  "Rebuild the document and re-sync the viewer on every save."
  :global t
  :lighter " live"
  :group 'TeX-command
  (if my/tex-live-mode
      (progn
        (add-hook 'after-save-hook #'my/tex-live--after-save)
        (add-hook 'TeX-after-compilation-finished-functions #'my/tex-live--view))
    (remove-hook 'after-save-hook #'my/tex-live--after-save)
    (remove-hook 'TeX-after-compilation-finished-functions #'my/tex-live--view)
    (when (timerp my/tex-live--timer)
      (cancel-timer my/tex-live--timer)
      (setq my/tex-live--timer nil))))

(with-eval-after-load 'tex
  (advice-add 'TeX-TeX-sentinel :after #'my/tex-run-compilation-finished)
  (keymap-set TeX-mode-map "C-c l" #'my/tex-live-mode))


;;;; AUCTeX

(use-package auctex :ensure t
  :defer t)

(use-package latex :ensure auctex
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad)))))))

  (setq LaTeX-indent-level 4
        LaTeX-item-indent 0
        TeX-brace-indent-level 4))

(with-eval-after-load 'tex
  (keymap-set TeX-mode-map "$" #'my/math-insert)
  (keymap-set TeX-mode-map "№" #'my/math-insert))

(with-eval-after-load 'tex-mode
  (keymap-set tex-mode-map "$" #'my/math-insert)
  (keymap-set tex-mode-map "№" #'my/math-insert))

;; `TeX-mode-hook', not `LaTeX-mode-hook': plain TeX buffers are the common
(add-hook 'TeX-mode-hook #'visual-line-mode)

(add-hook 'LaTeX-mode-hook #'outline-minor-mode)


;;;; Maths snippets

(defun my/latex-insert-sum ()
  "Insert \\sum_{}^{}, point in the lower limit."
  (interactive)
  (insert "\\sum_{}^{} ")
  (backward-char 5))

(defun my/latex-insert-span ()
  "Insert \\Span(), point inside the parentheses."
  (interactive)
  (insert "\\Span()")
  (backward-char 1))

(defun my/laas-mathp (orig)
  "Around advice for ORIG, `laas-mathp': answer for plain TeX as well."
  (if (derived-mode-p 'tex-mode) (my/math-in-math-p) (funcall orig)))

(use-package laas :ensure t
  :hook (TeX-mode . laas-mode)
  :config
  (advice-add 'laas-mathp :around #'my/laas-mathp)
  (aas-set-snippets 'laas-mode
    :cond #'laas-mathp ; expand only while in math
    "supp" "\\supp"
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    ;; bind to functions!
    "Sum" #'my/latex-insert-sum
    "Span" #'my/latex-insert-span
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;;; l.el ends here
