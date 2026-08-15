;;; math.el --- Maths: detection, highlighting, fast input -*- lexical-binding: t; -*-

;;; Commentary:
;; $ pair detection, math highlighting, flymake for unmatched delimiters.

;;; Code:

(require 'cl-lib)

(declare-function texmathp "texmathp" ())
(declare-function font-latex-faces-present-p "font-latex" (faces &optional pos))
(declare-function flymake-mode "flymake" (&optional arg))
(declare-function flymake-make-diagnostic "flymake"
                  (locus beg end type text &optional data overlay-properties))

(defgroup my-math nil
  "Maths detection, highlighting and input."
  :group 'tex
  :prefix "my/math-")


;;;; Where the $ pairs are

(defcustom my/math-search-n-paragraphs 2
  "How many paragraphs back to scan for an opening `$'. Same idea, and same default, as `texmathp-search-n-paragraphs': maths does not span a blank line, so there is no point looking further."
  :type 'natnum
  :group 'my-math)

(defun my/math--escaped-p (pos)
  "Non-nil if the character at POS is preceded by an odd number of backslashes."
  (save-excursion
    (goto-char pos)
    (cl-oddp (- (skip-chars-backward "\\\\")))))

(defun my/math--block-start (pos)
  "Return the start of the blank-line-delimited block POS is in."
  (save-excursion
    (goto-char pos)
    (if (re-search-backward "[\n\r][ \t]*[\n\r]" nil 1 my/math-search-n-paragraphs)
        (match-end 0)
      (point-min))))

(defcustom my/math-text-macros
  '("text" "textrm" "textit" "textbf" "textsf" "texttt" "textsc"
    "textnormal" "mbox" "hbox" "vbox" "vtop" "fbox" "framebox" "makebox"
    "parbox" "frame" "framed" "intertext" "shortintertext")
  "Macros whose braced argument is typeset as text even inside maths."
  :type '(repeat string)
  :group 'my-math)

(defun my/math--text-group-bounds (pos)
  "Return (BODY-BEG . CLOSE) of the innermost text group holding POS, or nil."
  (save-excursion
    (let ((case-fold-search nil)
          found)
      ;; Outermost first, so the last one that matches is the innermost.
      (dolist (open (nth 9 (syntax-ppss pos)) found)
        (when (and (eq (char-after open) ?\{)
                   (save-excursion
                     (goto-char open)
                     (and (looking-back "\\\\\\([a-zA-Z@]+\\)"
                                        (line-beginning-position))
                          (member (match-string 1) my/math-text-macros))))
          (setq found (cons (1+ open)
                            (or (ignore-errors (1- (scan-lists open 1 0)))
                                (point-max)))))))))

(defun my/math--pair-at (pos)
  "Return the `$' or `$$' construct containing POS, or nil."
  (save-excursion
    (let* ((group (my/math--text-group-bounds pos))
           (limit (cdr group)))
      (goto-char (if group (car group) (my/math--block-start pos)))
      (let (open kind)
        (catch 'done
          (while (search-forward "$" limit t)
            (unless (my/math--escaped-p (match-beginning 0))
              (let* ((beg (match-beginning 0))
                     ;; Outermost first, so the last one that matches is the innermost.
                     (len (if (and (eq (char-after) ?$) (/= pos (1+ beg))) 2 1)))
                (goto-char (+ beg len))
                (if open
                    ;; Closes the pair opened earlier.
                    (if (and (< open pos) (<= pos (point)))
                        (throw 'done
                               (list kind open
                                     (+ open (if (eq kind 'display) 2 1))
                                     beg (point)))
                      (setq open nil kind nil))
                  (when (>= beg pos) (throw 'done nil))
                  (setq open beg
                        kind (if (= len 2) 'display 'inline))))))
          nil)))))

(defun my/math--in-body-p (pos)
  "Non-nil if POS is between the delimiters of a `$' or `$$' construct."
  (pcase (my/math--pair-at pos)
    (`(,_ ,_ ,body-beg ,body-end ,_) (and (<= body-beg pos) (<= pos body-end)))))


;;;; The predicate

(defun my/math-in-math-p (&optional pos)
  "Non-nil if POS, or point, is inside maths."
  (let ((pos (or pos (point))))
    (when (derived-mode-p 'tex-mode)
      (and (or (fboundp 'texmathp) (require 'texmathp nil t))
           (save-excursion
             (goto-char pos)
             (ignore-errors (texmathp)))))))


;;;; Highlighting

(defface font-latex-math-keywords-face
  '((t :inherit font-lock-builtin-face))
  "Face for control sequences inside maths.
Named to sit beside `font-latex-math-face' in \\[customize-face], though
it is installed by m/math.el and not by font-latex."
  :group 'my-math)

(defcustom my/math-highlight-single-char-macros t
  "If non-nil, also face the one-character macros: \\=\\, \\=\\; \\=\\!"
  :type 'boolean
  :group 'my-math)

(defcustom my/math-detection 'auto
  "How the font-lock matcher decides that a candidate is inside maths."
  :type '(choice (const :tag "Probe the face where possible" auto)
                 (const :tag "Always ask the predicate" texmathp))
  :group 'my-math)

(defun my/math--macro-regexp ()
  "Regexp matching one control sequence."
  (if my/math-highlight-single-char-macros
      "\\\\\\(?:[a-zA-Z@]+\\*?\\|[^[:alpha:][:space:]]\\)"
    "\\\\[a-zA-Z@]+\\*?"))

(defun my/math--fontified-in-math-p (pos)
  "Non-nil if POS is inside maths."
  (if (and (eq my/math-detection 'auto)
           (derived-mode-p 'tex-mode)
           (fboundp 'font-latex-faces-present-p))
      (font-latex-faces-present-p 'font-latex-math-face pos)
    (my/math-in-math-p pos)))

(defun my/math-macro-matcher (limit)
  "Font-lock matcher: the next control sequence inside maths before LIMIT."
  (let ((re (my/math--macro-regexp)))
    (catch 'found
      (while (re-search-forward re limit t)
        ;; `save-match-data' is load-bearing.
        (let ((beg (match-beginning 0)))
          (when (save-match-data (my/math--fontified-in-math-p beg))
            (throw 'found t))))
      nil)))

(defvar-local my/math--keywords nil
  "The keyword list handed to `font-lock-add-keywords', so it can be removed.")

;;;###autoload
(define-minor-mode my/math-keywords-mode
  "Give every control sequence inside maths a face of its own."
  :lighter ""
  :group 'my-math
  (if my/math-keywords-mode
      (let ((keywords `((my/math-macro-matcher
                         0 'font-latex-math-keywords-face prepend))))
        (setq my/math--keywords keywords)
        (font-lock-add-keywords nil keywords 'append))
    (when my/math--keywords
      (font-lock-remove-keywords nil my/math--keywords)
      (setq my/math--keywords nil)))
  (when font-lock-mode (font-lock-flush)))


;;;; Fast input: the $ cycle

(defun my/math--literal-dollar-context-p ()
  "Non-nil where `$' must stay a plain `$': inside a TeX comment."
  (and (derived-mode-p 'tex-mode)
       (nth 4 (syntax-ppss))))

(defun my/math--wrap-region ()
  "Surround the region with inline delimiters, point after the closing one."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "$")
    (goto-char beg)
    (insert "$")
    (goto-char (+ end 2))))

(defun my/math--promote (open body-end)
  "Turn the `$...$' with delimiters at OPEN and BODY-END into `$$...$$'."
  (let ((pos (point)))
    (goto-char body-end)
    (insert "$")
    (goto-char open)
    (insert "$")
    (goto-char (+ pos (if (> pos body-end) 2 1)))))

(defun my/math--demote (open body-end)
  "Turn the `$$...$$' with delimiters at OPEN and BODY-END back into `$...$'."
  (let ((pos (point)))
    (delete-region body-end (1+ body-end))
    (delete-region open (1+ open))
    (goto-char (max open (- pos (if (> pos body-end) 2 1))))))

;;;###autoload
(defun my/math-insert ()
  "Insert maths delimiters, or act on the pair around point."
  (interactive)
  (cond
   ((my/math--literal-dollar-context-p) (self-insert-command 1))
   ((use-region-p) (my/math--wrap-region))
   (t
    (pcase (my/math--pair-at (point))
      (`(,kind ,open ,body-beg ,body-end ,close)
       (cond
        ((= body-beg body-end)
         (if (eq kind 'inline)
             (my/math--promote open body-end)
           (delete-region open close)
           (goto-char open)))
        ;; Something typed and point is still in it: leave maths.
        ((<= (point) body-end) (goto-char close))
        ;; Point is at or inside the closing delimiter: switch the kind.
        ((eq kind 'inline) (my/math--promote open body-end))
        (t (my/math--demote open body-end))))
      (_
       (cond
        ((and (my/math-in-math-p)
              (not (my/math--text-group-bounds (point))))
         (self-insert-command 1))
        ;; Point sits just before a pair.
        ((and (eq (char-after) ?$) (not (my/math--escaped-p (point))))
         (forward-char (if (eq (char-after (1+ (point))) ?$) 2 1)))
        (t (insert "$$")
           (backward-char 1))))))))

(put 'my/math-insert 'delete-selection nil)


;;;; Diagnostics: the `$' with no partner

(defcustom my/math-verbatim-regexps
  '(("^[ \t]*\\\\begtt\\>" . "^[ \t]*\\\\endtt\\>")
    ("\\\\begin{\\(?:verbatim\\|Verbatim\\|lstlisting\\|minted\\|alltt\\)\\*?}"
     . "\\\\end{\\(?:verbatim\\|Verbatim\\|lstlisting\\|minted\\|alltt\\)\\*?}"))
  "Regexp pairs delimiting regions where `$' is ordinary text. `syntax-ppss' knows TeX comments but nothing of verbatim, so the check has to be told."
  :type '(alist :key-type regexp :value-type regexp)
  :group 'my-math)

(defcustom my/math-check-delimiters t
  "If non-nil, report unmatched `$' delimiters through Flymake. Turning this off also stops `my/math-setup' switching."
  :type 'boolean
  :group 'my-math)

(defface my/math-unmatched-face
  '((((background dark))  :background "#a02020" :foreground "white" :weight bold)
    (((background light)) :background "#c33333" :foreground "white" :weight bold)
    (t :inherit error :inverse-video t))
  "Face for a `$' with no partner.
Worn by the Flymake overlay, which sits above font-lock, so it shows
through `font-latex-math-face' on the delimiter."
  :group 'my-math)

(put 'my/math-unmatched 'flymake-category 'flymake-error)
(put 'my/math-unmatched 'face 'my/math-unmatched-face)

(defun my/math--verbatim-regions ()
  "Return (BEG . END) for each `my/math-verbatim-regexps' region in the buffer."
  (save-excursion
    (let (regions)
      (pcase-dolist (`(,open . ,close) my/math-verbatim-regexps)
        (goto-char (point-min))
        (while (re-search-forward open nil t)
          (push (cons (match-beginning 0)
                      (if (re-search-forward close nil t) (match-end 0) (point-max)))
                regions)))
      ;; \verb takes whatever character follows it as its delimiter.
      (goto-char (point-min))
      (while (re-search-forward "\\\\verb\\*?\\([^A-Za-z*\n]\\)" nil t)
        (let ((beg (match-beginning 0))
              (delimiter (match-string 1))
              (eol (line-end-position)))
          (push (cons beg (if (search-forward delimiter eol t) (point) eol))
                regions)))
      regions)))

(defun my/math--delimiter-p (pos regions)
  "Non-nil if the `$' or brace at POS is really TeX syntax."
  (not (or (my/math--escaped-p pos)
           (save-excursion (nth 4 (syntax-ppss pos)))
           (cl-some (lambda (r) (and (<= (car r) pos) (< pos (cdr r)))) regions))))

(defun my/math--blank-line-between-p (beg end)
  "Non-nil if a blank line separates BEG from END."
  (and (< beg end)
       (save-excursion
         (goto-char beg)
         (re-search-forward "[\n\r][ \t]*[\n\r]" end t))))

(defun my/math-unmatched-delimiters ()
  "Return the `$' delimiters in the buffer that have no partner."
  (let ((regions (my/math--verbatim-regions))
        (state 'off) opener (last (point-min)) stack findings)
    (cl-flet ((unclosed (where)
                (push (list (car opener) (cdr opener)
                            (format "Unmatched `%s': maths is still open where the %s ends"
                                    (if (eq state 'display) "$$" "$") where))
                      findings)
                (setq state 'off opener nil)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[{}$]" nil t)
          (let ((pos (1- (point)))
                (char (char-before)))
            (when (my/math--delimiter-p pos regions)
              (pcase char
                ;; A group is a box until proved otherwise: text, whatever encloses it. See the Commentary above.
                (?\{
                 (push (list state opener last) stack)
                 (setq state 'off opener nil last (point)))
                (?\}
                 (unless (eq state 'off) (unclosed "group"))
                 (if stack
                     (pcase-let ((`(,s ,o ,l) (pop stack)))
                       (setq state s opener o last l))
                   (setq last (point))))
                (_
                 (when (and (not (eq state 'off))
                            (my/math--blank-line-between-p last pos))
                   (unclosed "paragraph"))
                 ;; A `$' straight after a `$' can be neither escaped nor commented.
                 (let ((n (save-excursion (goto-char pos)
                                          (skip-chars-forward "$"))))
                   (pcase state
                     ('off
                      (setq state (if (> n 1) 'display 'inline)
                            opener (cons pos (+ pos (if (> n 1) 2 1))))
                      (goto-char (cdr opener)))
                     ('inline
                       (setq state 'off opener nil)
                       (goto-char (1+ pos)))
                     ('display
                      (unless (> n 1)
                        (push (list pos (1+ pos)
                                    "Display maths closed with a single `$'")
                              findings))
                      (setq state 'off opener nil)
                      (goto-char (+ pos (if (> n 1) 2 1))))))
                 (setq last (point)))))))
        ;; Nothing follows to trigger the checks above, so they happen here.
        (dolist (scope (append (reverse stack) (list (list state opener last))))
          (pcase-let ((`(,s ,o ,l) scope))
            (when (and o (not (eq s 'off)))
              (setq state s opener o)
              (unclosed (if (my/math--blank-line-between-p l (point-max))
                            "paragraph"
                          "buffer")))))))
    ;; Not scan order: a scope left open reports at end of buffer.
    (sort (nreverse findings) :key #'car)))

(defun my/math-flymake (report-fn &rest _)
  "Flymake backend reporting unmatched `$' delimiters.  REPORT-FN as usual."
  (funcall report-fn
           (mapcar (pcase-lambda (`(,beg ,end ,message))
                     (flymake-make-diagnostic (current-buffer) beg end
                                              'my/math-unmatched message))
                   (my/math-unmatched-delimiters))))


;;;; Switching the macOS input source inside maths (emacs-mac only)

(defvar-local my/math--saved-source nil
  "Saved macOS input source from before point entered maths.")

(defconst my/mac-input-source-available-p
  (and (fboundp 'mac-input-source)
       (fboundp 'mac-select-input-source))
  "Non-nil if this build provides the macOS input-source functions.")

(defconst my/math-english-input-source "com.apple.keylayout.US"
  "Input source to switch to inside maths.")

(defun my/math-auto-input-source ()
  "Switch the macOS input source to English in maths, restore it on exit."
  (when (and my/mac-input-source-available-p
             (display-graphic-p)
             (derived-mode-p 'tex-mode)
             (eq (current-buffer) (window-buffer (selected-window))))
    (let ((current (ignore-errors (funcall 'mac-input-source)))
          (in-math (ignore-errors (my/math-in-math-p))))
      (cond
       ((and in-math (not my/math--saved-source))
        (setq my/math--saved-source
              (if (equal current my/math-english-input-source)
                  :already-english
                current))
        (unless (equal current my/math-english-input-source)
          (funcall 'mac-select-input-source my/math-english-input-source)))
       ((and (not in-math) my/math--saved-source)
        (unless (eq my/math--saved-source :already-english)
          (funcall 'mac-select-input-source my/math--saved-source))
        (setq my/math--saved-source nil))))))

(when my/mac-input-source-available-p
  (add-hook 'post-command-hook #'my/math-auto-input-source))


;;;; Installation

(defun my/math-setup ()
  "Turn on maths highlighting, and the unmatched-`$' check, in this buffer."
  (my/math-keywords-mode 1)
  (when my/math-check-delimiters
    (add-hook 'flymake-diagnostic-functions #'my/math-flymake nil t)
    (flymake-mode 1)))

(add-hook 'TeX-mode-hook #'my/math-setup)
(add-hook 'tex-mode-hook #'my/math-setup)

;;; math.el ends here
