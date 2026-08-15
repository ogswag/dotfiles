;;; tbl.el --- Tables typed as org tables, emitted as TeX -*- lexical-binding: t; -*-

;;; Commentary:
;; orgtbl in tex buffers, then convert to optex/latex. see doc/tables.org.

;;; Code:

(eval-when-compile
  (require 'tex nil t)
  (require 'tex-mode nil t)         ; the built-in one, for `tex-mode-map'
  (require 'latex nil t)            ; AUCTeX, for `LaTeX-current-environment'
  (require 'org-table nil t))

(declare-function my/math-in-math-p "math" (&optional pos))
(declare-function LaTeX-current-environment "latex" (&optional arg))
(declare-function org-at-table-p "org-table" (&optional table-type))
(declare-function org-table-align "org-table" ())
(declare-function org-table-begin "org-table" (&optional table-type))
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function orgtbl-mode "org-table" (&optional arg))
(declare-function orgtbl-to-generic "org-table" (table params))

(defvar vertico-preselect)

(defgroup my-tex-table nil
  "Type TeX tables as org tables."
  :group 'tex
  :prefix "my/tex-table-")


;;;; Options

(defcustom my/tex-table-dialect 'auto
  "Which TeX the generators target."
  :type '(choice (const :tag "From the major mode" auto)
                 (const :tag "LaTeX" latex)
                 (const :tag "OpTeX / plain TeX" optex))
  :group 'my-tex-table)

(defcustom my/tex-table-default-spec "ll"
  "Default alignment spec, offered as the default of the spec prompt."
  :type 'string
  :group 'my-tex-table)

(defcustom my/tex-table-rows 2
  "Body rows in a new non-maths table, counting the header."
  :type 'integer
  :group 'my-tex-table)

(defcustom my/tex-table-default-size "3x3"
  "Default answer to the matrix size prompt."
  :type 'string
  :group 'my-tex-table)

(defcustom my/tex-table-default-matrix-type "pmatrix"
  "Matrix type offered first, before anything is in the history."
  :type 'string
  :group 'my-tex-table)

(defcustom my/tex-table-math-macros
  '("pmatrix" "matrix" "bmatrix" "vmatrix" "Vmatrix" "smallmatrix"
    "cases" "eqalign" "eqalignno" "displaylines" "array" "aligned" "align")
  "Extra macro and environment names that already provide rows and columns."
  :type '(repeat string)
  :group 'my-tex-table)

(defcustom my/tex-table-matrix-types
  '(("pmatrix"
     :glyph "( )" :desc "the usual matrix"
     :latex "pmatrix" :optex ("\\pmatrix{" . "}"))
    ("bmatrix"
     :glyph "[ ]" :desc "square brackets"
     :latex "bmatrix" :optex ("\\left[\\matrix{" . "}\\right]"))
    ("Bmatrix"
     :glyph "{ }" :desc "curly brackets"
     :latex "Bmatrix" :optex ("\\left\\{\\matrix{" . "}\\right\\}"))
    ("vmatrix"
     :glyph "| |" :desc "determinant"
     :latex "vmatrix" :optex ("\\left|\\matrix{" . "}\\right|"))
    ("Vmatrix"
     :glyph "‖ ‖" :desc "norm"
     :latex "Vmatrix" :optex ("\\left\\Vert\\matrix{" . "}\\right\\Vert"))
    ("matrix"
     :glyph "   " :desc "no delimiters"
     :latex "matrix" :optex ("\\matrix{" . "}"))
    ("cases"
     :glyph "{  " :desc "case distinction"
     :max-cols 2
     :latex "cases" :optex ("\\cases{" . "}"))
    ("array"
     :glyph "   " :desc "explicit column spec -- asks for one"
     :latex "array" :spec t)
    ("smallmatrix"
     :glyph "   " :desc "inline-sized, undelimited (amsmath)"
     :latex "smallmatrix")
    ("aligned"
     :glyph "   " :desc "aligned equations inside a display (amsmath)"
     :latex "aligned")
    ("eqalign"
     :glyph "   " :desc "aligned equations, r/l column pairs"
     :optex ("\\eqalign{" . "}"))
    ("displaylines"
     :glyph "   " :desc "one centred formula per row"
     :max-cols 1
     :optex ("\\displaylines{" . "}"))
    ("bordermatrix"
     :glyph "( )" :desc "labelled first row and column"
     :optex ("\\bordermatrix{" . "}"))
    ("none"
     :glyph "   " :desc "no wrapper -- splice into a surrounding matrix"
     :splice t))
  "Matrix constructs offered by \\[my/tex-matrix], in menu order."
  :type '(alist :key-type (string :tag "Name") :value-type plist)
  :group 'my-tex-table)

(defcustom my/tex-table-spec-presets
  ;; (SPEC DIALECTS DESCRIPTION).
  '(("ll"                (optex latex) "two columns, flush left")
    ("lr"                (optex latex) "label and value")
    ("lcr"               (optex latex) "left, centred, right")
    ("ccc"               (optex latex) "three centred")
    ("|l|c|r|"           (optex latex) "ruled on every boundary")
    ("l||l"              (optex latex) "double rule between the columns")
    ("lp{5cm}"           (optex latex) "label plus a wrapping paragraph column")
    ("3c"                (optex)       "digit prefix repeats a declarator")
    ("c:(\\bf)c"         (optex)       "(...) sets a column, : places the boundary")
    ("c(\\tabskip=5pt)r" (optex)       "individual gap between two columns")
    ("|c|p{\\tsize}|"    (optex)       "for \\table pxto: p{} takes up the slack")
    ("*{3}{c}"           (latex)       "*{n}{...} repeats a group")
    ("l@{}r"             (latex)       "@{} removes the inter-column space")
    ("l>{\\bfseries}l"   (latex)       ">{...} prefixes a column (array)")
    ("lm{3cm}"           (latex)       "m{} is vertically centred (array)"))
  "Ready-made alignment specs offered by the spec prompt, in menu order."
  :type '(repeat (list (string :tag "Spec")
                       (repeat :tag "Dialects" symbol)
                       (string :tag "Description")))
  :group 'my-tex-table)


;;;; State

(defvar-local my/tex-table--spec nil
  "Alignment spec for the table currently being typed.")

(defvar-local my/tex-table--type nil
  "Matrix type for the table currently being typed.")

(defvar-local my/tex-table--dialect nil
  "Dialect captured when the current table was started.")

(defvar-local my/tex-table--context nil
  "`math' or `text', captured when the current table was started.")

(defvar my/tex-table-spec-history nil
  "Recently used alignment specs.")

(defvar my/tex-table-matrix-history nil
  "Recently used matrix types.")

(defvar my/tex-table-size-history nil
  "Recently used matrix sizes.")


;;;; Dialect and context

(defun my/tex-table--dialect ()
  "Return `latex' or `optex' for the current buffer."
  (if (eq my/tex-table-dialect 'auto)
      (if (derived-mode-p 'LaTeX-mode 'latex-mode) 'latex 'optex)
    my/tex-table-dialect))

(defun my/tex-table--context ()
  "Return `math' when point is in maths, else `text'."
  (if (and (fboundp 'my/math-in-math-p) (my/math-in-math-p)) 'math 'text))


;;;; Alignment specs

(defun my/tex-table--skip-group (spec i)
  "Index just past the balanced brace group at I in SPEC."
  (let ((len (length spec)))
    (if (or (>= i len) (/= (aref spec i) ?{))
        i
      (let ((depth 0) (j i) (done nil))
        (while (and (not done) (< j len))
          (pcase (aref spec j)
            (?\{ (setq depth (1+ depth)))
            (?\} (setq depth (1- depth))
                 (when (<= depth 0) (setq done t))))
          (setq j (1+ j)))
        j))))

(defun my/tex-table--group-at (spec i)
  "Return (CONTENTS . END) for the brace group at I in SPEC, or nil."
  (let ((end (my/tex-table--skip-group spec i)))
    (when (> end i)
      (cons (substring spec (1+ i) (1- end)) end))))

(defun my/tex-table--scan-columns (spec dialect)
  "Count the columns SPEC declares in DIALECT.  May return zero."
  (let ((latexp (eq dialect 'latex))
        (len (length spec))
        (i 0) (n 0) (repeat 1))
    (while (< i len)
      (let ((c (aref spec i)))
        (cond
         ((memq c '(?\s ?\t ?\n)) (setq i (1+ i)))
         ;; Rules and boundaries: no column, and they consume the repeat.
         ((eq c ?|) (setq repeat 1 i (1+ i)))
         ((and (not latexp) (eq c ?:)) (setq repeat 1 i (1+ i)))
         ;; OpTeX per-column setting, e.g. (\Red) or (\tabskip=5pt).
         ((and (not latexp) (eq c ?\())
          (let ((j (1+ i)))
            (while (and (< j len) (/= (aref spec j) ?\))) (setq j (1+ j)))
            (setq i (min len (1+ j)))))
         ((and latexp (memq c '(?@ ?! ?> ?<)))
          (setq i (my/tex-table--skip-group spec (1+ i))))
         ;; LaTeX *{n}{decls}.
         ((and latexp (eq c ?*))
          (let* ((count (my/tex-table--group-at spec (1+ i)))
                 (inner (and count (my/tex-table--group-at spec (cdr count)))))
            (if (not inner)
                (setq i (1+ i))
              (setq n (+ n (* repeat
                              (max 0 (truncate (string-to-number (car count))))
                              (my/tex-table--scan-columns (car inner) dialect)))
                    repeat 1
                    i (cdr inner)))))
         ;; OpTeX digit repeat: multiplies whatever the next token contributes.
         ((and (not latexp) (<= ?1 c ?9))
          (let ((j i))
            (while (and (< j len) (<= ?0 (aref spec j) ?9)) (setq j (1+ j)))
            (setq repeat (truncate (string-to-number (substring spec i j)))
                  i j)))
         ;; array's w{align}{width} and W{align}{width} take two groups.
         ((and latexp (memq c '(?w ?W)))
          (setq n (+ n repeat) repeat 1
                i (my/tex-table--skip-group
                   spec (my/tex-table--skip-group spec (1+ i)))))
         ;; Any other letter is a column.
         ((or (<= ?a c ?z) (<= ?A c ?Z))
          (setq n (+ n repeat) repeat 1
                i (my/tex-table--skip-group spec (1+ i))))
         (t (setq i (1+ i))))))
    n))

(defun my/tex-table--count-columns (spec &optional dialect)
  "Number of columns SPEC declares in DIALECT.  Always at least one."
  (max 1 (my/tex-table--scan-columns spec (or dialect (my/tex-table--dialect)))))

(defun my/tex-table-read-spec (&optional dialect)
  "Read an alignment spec for DIALECT, offering `my/tex-table-spec-presets'."
  (let* ((dialect (or dialect (my/tex-table--dialect)))
         (presets (seq-filter (lambda (p) (memq dialect (nth 1 p)))
                              my/tex-table-spec-presets))
         (cands (mapcar #'car presets))
         ;; A closure rather than a named function with a global to read the dialect from, which is how.
         (affix
          (lambda (cs)
            (mapcar
             (lambda (s)
               (list s
                     (format "%2d  " (my/tex-table--count-columns s dialect))
                     (propertize (concat "  " (or (nth 2 (assoc s presets)) ""))
                                 'face 'completions-annotations)))
             cs)))
         (table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . my/tex-table-spec)
                  (affixation-function . ,affix)
                  (display-sort-function . identity)
                  (cycle-sort-function . identity))
              (complete-with-action action cands string pred))))
         (prompt (if (eq dialect 'latex)
                     "Columns (tabular preamble): "
                   "Columns (\\table alignment spec): "))
         (vertico-preselect 'prompt)
         (choice (completing-read prompt table nil nil nil
                                  'my/tex-table-spec-history
                                  my/tex-table-default-spec)))
    (if (string-empty-p choice) my/tex-table-default-spec choice)))


;;;; Matrix types

(defun my/tex-table--matrix-entry (name)
  "Plist for matrix type NAME, or nil."
  (cdr (assoc name my/tex-table-matrix-types)))

(defun my/tex-table--matrix-usable-p (entry dialect)
  "Non-nil when ENTRY can be rendered in DIALECT."
  (or (plist-get entry :splice)
      (if (eq dialect 'latex)
          (plist-get entry :latex)
        (plist-get entry :optex))))

(defun my/tex-table--matrix-candidates (dialect)
  "Names from `my/tex-table-matrix-types' that DIALECT can render."
  (delq nil (mapcar (lambda (e)
                      (and (my/tex-table--matrix-usable-p (cdr e) dialect)
                           (car e)))
                    my/tex-table-matrix-types)))

(defun my/tex-table--matrix-form (entry dialect)
  "A one-line picture of what ENTRY emits in DIALECT."
  (cond
   ((plist-get entry :splice) "rows only")
   ((eq dialect 'latex)
    (format "\\begin{%s}%s" (plist-get entry :latex)
            (if (plist-get entry :spec) "{spec}" "")))
   (t (let ((w (plist-get entry :optex)))
        (concat (car w) "..." (cdr w))))))

(defun my/tex-table--matrix-affix (dialect)
  "Affixation function for DIALECT: glyph | name | construction and description."
  (lambda (cands)
    (mapcar
     (lambda (name)
       (let* ((e (my/tex-table--matrix-entry name))
              (max (plist-get e :max-cols)))
         (list name
               (concat (or (plist-get e :glyph) "   ") "  ")
               (propertize
                (format "  %-32s %s%s"
                        (my/tex-table--matrix-form e dialect)
                        (or (plist-get e :desc) "")
                        (if max (format " (%d col%s)" max (if (= max 1) "" "s")) ""))
                'face 'completions-annotations))))
     cands)))

(defun my/tex-table--matrix-sort (cands)
  "Sort CANDS: recently used first, then `my/tex-table-matrix-types' order."
  (let ((rank (make-hash-table :test #'equal))
        (order (make-hash-table :test #'equal))
        (i 0) (j 0))
    (dolist (h (append my/tex-table-matrix-history
                       (list my/tex-table-default-matrix-type)))
      (unless (gethash h rank) (puthash h i rank) (setq i (1+ i))))
    (dolist (e my/tex-table-matrix-types)
      (puthash (car e) j order) (setq j (1+ j)))
    (sort (copy-sequence cands)
          (lambda (a b)
            (let ((ra (gethash a rank most-positive-fixnum))
                  (rb (gethash b rank most-positive-fixnum)))
              (if (/= ra rb)
                  (< ra rb)
                (< (gethash a order 0) (gethash b order 0))))))))

(defun my/tex-table-read-matrix-type (&optional dialect)
  "Read a matrix type DIALECT can render, showing what each one emits."
  (let* ((dialect (or dialect (my/tex-table--dialect)))
         (cands (my/tex-table--matrix-candidates dialect))
         (affix (my/tex-table--matrix-affix dialect))
         (table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . my/tex-table-matrix)
                  (affixation-function . ,affix)
                  (display-sort-function . my/tex-table--matrix-sort)
                  (cycle-sort-function . my/tex-table--matrix-sort))
              (complete-with-action action cands string pred))))
         (choice (completing-read "Matrix: " table nil t nil
                                  'my/tex-table-matrix-history
                                  my/tex-table-default-matrix-type)))
    (and choice (not (string-empty-p choice)) choice)))

(defun my/tex-table--parse-size (string)
  "Parse STRING as ROWSxCOLS into (ROWS ."
  (let ((s (string-trim string)))
    (cond
     ((string-match "\\`\\([0-9]+\\)[ \t]*[xX×*,][ \t]*\\([0-9]+\\)\\'" s)
      (cons (string-to-number (match-string 1 s))
            (string-to-number (match-string 2 s))))
     ((string-match "\\`\\([0-9]+\\)[ \t]+\\([0-9]+\\)\\'" s)
      (cons (string-to-number (match-string 1 s))
            (string-to-number (match-string 2 s))))
     ((string-match "\\`[0-9]+\\'" s)
      (let ((n (string-to-number s))) (cons n n)))
     (t nil))))

(defun my/tex-table-read-size (&optional max-cols prompt)
  "Read a matrix size as ROWSxCOLS."
  ;; DEFAULT-VALUE rather than INITIAL-INPUT.
  (let* ((default (if (and max-cols (= max-cols 1))
                      "3"
                    my/tex-table-default-size))
         (input (read-string (or prompt
                                 (if max-cols
                                     (format "Size (rows x cols, max %d col%s): "
                                             max-cols (if (= max-cols 1) "" "s"))
                                   "Size (rows x cols): "))
                             nil 'my/tex-table-size-history default))
         (size (my/tex-table--parse-size input)))
    (unless size
      (user-error "Cannot read `%s' as ROWSxCOLS" input))
    (cons (max 1 (car size))
          (if max-cols
              (min max-cols (max 1 (cdr size)))
            (max 1 (cdr size))))))


;;;; Generators

(defun my/tex-table--to-optex (table spec)
  "Render TABLE as an OpTeX \\table with alignment SPEC."
  (orgtbl-to-generic
   table
   (list :tstart (concat "\\table{" spec "}{")
         :tend "}"
         :lstart "  "
         :lend " \\cr"
         :sep " & "
         :hline "  \\crl")))

(defun my/tex-table--to-latex (table spec)
  "Render TABLE as a LaTeX tabular with alignment SPEC."
  (orgtbl-to-generic
   table
   (list :tstart (concat "\\begin{tabular}{" spec "}")
         :tend "\\end{tabular}"
         :lstart "  "
         :lend " \\\\"
         :sep " & "
         :hline "  \\hline")))

(defun my/tex-table--matrix-wrap (rows type dialect &optional spec)
  "Put the wrapper for matrix TYPE in DIALECT around ROWS."
  (let ((entry (my/tex-table--matrix-entry type)))
    (cond
     ((or (null entry) (plist-get entry :splice)) rows)
     ((eq dialect 'latex)
      (let ((env (plist-get entry :latex)))
        (if (not env)
            rows
          (concat "\\begin{" env "}"
                  (if (plist-get entry :spec)
                      (concat "{" (or spec my/tex-table-default-spec) "}")
                    "")
                  "\n" rows "\n\\end{" env "}"))))
     (t
      (let ((wrap (plist-get entry :optex)))
        (if (not wrap)
            rows
          (concat (car wrap) "\n" rows "\n" (cdr wrap))))))))

(defun my/tex-table--to-math (table type dialect &optional spec)
  "Render TABLE as maths rows of matrix TYPE in DIALECT."
  (my/tex-table--matrix-wrap
   (orgtbl-to-generic
    table
    (list :splice t
          :lstart "  "
          :lend (if (eq dialect 'latex) " \\\\" " \\cr")
          :sep " & "
          ;; A rule only means anything in LaTeX's array.
          :hline (and (eq dialect 'latex) (equal type "array") "  \\hline")))
   type dialect spec))

(defun my/tex-table--matrix-names ()
  "Every name that counts as an already-tabular construct."
  (delete-dups
   (append
    (delq nil (mapcar (lambda (e)
                        (and (not (plist-get (cdr e) :splice)) (car e)))
                      my/tex-table-matrix-types))
    (delq nil (mapcar (lambda (e) (plist-get (cdr e) :latex))
                      my/tex-table-matrix-types))
    (copy-sequence my/tex-table-math-macros))))

(defun my/tex-table--enclosing-env ()
  "Innermost unclosed LaTeX environment name before point, or nil."
  (save-excursion
    (let ((depth 0) (name nil))
      (while (and (not name)
                  (re-search-backward
                   "\\\\\\(begin\\|end\\)[ \t]*{\\([^}]+\\)}" nil t))
        (if (equal (match-string 1) "end")
            (setq depth (1+ depth))
          (if (> depth 0)
              (setq depth (1- depth))
            (setq name (match-string 2)))))
      name)))

(defun my/tex-table--enclosing-matrix (&optional dialect)
  "Name of the matrix construct point sits inside, or nil."
  (let* ((dialect (or dialect (my/tex-table--dialect)))
         (names (my/tex-table--matrix-names)))
    (or
     (and (eq dialect 'latex)
          (let ((env (if (fboundp 'LaTeX-current-environment)
                         (ignore-errors (LaTeX-current-environment))
                       (my/tex-table--enclosing-env))))
            (car (member env names))))
     ;; \pmatrix{ ...
     (when-let* ((open (nth 1 (syntax-ppss))))
       (save-excursion
         (goto-char open)
         (and (looking-back (concat "\\\\" (regexp-opt names t) "[ \t\n]*")
                            (max (point-min) (- open 64)))
              (match-string 1)))))))

(defun my/tex-table--render (table spec &optional type dialect context)
  "Render TABLE with alignment SPEC for the TeX at point."
  (let ((dialect (or dialect (my/tex-table--dialect)))
        (context (or context (my/tex-table--context))))
    (cond
     ((eq context 'math)
      (my/tex-table--to-math
       table
       (or type
           (if (my/tex-table--enclosing-matrix dialect)
               "none"
             my/tex-table-default-matrix-type))
       dialect spec))
     ((eq dialect 'latex) (my/tex-table--to-latex table spec))
     (t (my/tex-table--to-optex table spec)))))


;;;; The mode

(defvar-keymap my/tex-table-mode-map
  :doc "Active only while an org table is being typed in a TeX buffer."
  "C-c C-c" #'my/tex-table-finish)

(define-minor-mode my/tex-table-mode
  "Type a table as an org table, then convert it to TeX with \\[my/tex-table-finish]."
  :lighter " tbl"
  :keymap my/tex-table-mode-map
  (require 'org-table)
  (if my/tex-table-mode
      (progn
        (orgtbl-mode 1)
        (setq-local minor-mode-overriding-map-alist
                    (cons (cons 'my/tex-table-mode my/tex-table-mode-map)
                          minor-mode-overriding-map-alist)))
    (orgtbl-mode -1)
    (setq-local minor-mode-overriding-map-alist
                (assq-delete-all 'my/tex-table-mode
                                 minor-mode-overriding-map-alist))))


;;;; Commands

(defun my/tex-table--skeleton (rows cols &optional rule)
  "Insert ROWS by COLS empty org rows at point."
  (let ((row (concat "|" (mapconcat (lambda (_) "  |")
                                    (number-sequence 1 cols) ""))))
    (unless (bolp) (insert "\n"))
    (save-excursion
      (insert row "\n")
      (when rule (insert "|-\n"))
      (dotimes (_ (max 0 (1- rows))) (insert row "\n")))))

(defun my/tex-table--start (rows cols spec type dialect context)
  "Lay down a ROWS by COLS skeleton and start `my/tex-table-mode'."
  (require 'org-table)
  ;; A header rule outside maths, none inside it, where it is rarely wanted.
  (my/tex-table--skeleton rows cols (eq context 'text))
  (setq my/tex-table--spec spec
        my/tex-table--type type
        my/tex-table--dialect dialect
        my/tex-table--context context)
  (my/tex-table-mode 1)
  (org-table-align)
  (forward-char 2))

;;;###autoload
(defun my/tex-matrix (rows cols &optional type spec)
  "Start a ROWS by COLS org table to become a maths matrix of TYPE."
  (interactive
   (let* ((dialect (my/tex-table--dialect))
          (inside (my/tex-table--enclosing-matrix dialect))
          (type (if inside "none" (my/tex-table-read-matrix-type dialect)))
          (entry (my/tex-table--matrix-entry type))
          (spec (and (plist-get entry :spec) (my/tex-table-read-spec dialect)))
          (size (my/tex-table-read-size (plist-get entry :max-cols))))
     (list (car size) (cdr size) type spec)))
  (my/tex-table--start rows cols spec (or type "none")
                       (my/tex-table--dialect) 'math))

;;;###autoload
(defun my/tex-table (&optional spec rows)
  "Start an org table at point, to be converted to TeX by \\[my/tex-table-finish]."
  (interactive)
  (let ((dialect (my/tex-table--dialect))
        (context (my/tex-table--context)))
    (if (and (null spec) (eq context 'math))
        (call-interactively #'my/tex-matrix)
      (let* ((spec (or spec (my/tex-table-read-spec dialect)))
             (rows (or rows
                       (if current-prefix-arg
                           (prefix-numeric-value current-prefix-arg)
                         my/tex-table-rows))))
        (my/tex-table--start (max 1 rows)
                             (my/tex-table--count-columns spec dialect)
                             spec nil dialect 'text)))))

(defun my/tex-table-finish ()
  "Replace the org table at point with its TeX rendering."
  (interactive)
  (require 'org-table)
  (unless (org-at-table-p)
    (user-error "Point is not in a table"))
  (let* ((table (org-table-to-lisp))
         (spec (or my/tex-table--spec my/tex-table-default-spec))
         (tex (my/tex-table--render table spec
                                    my/tex-table--type
                                    my/tex-table--dialect
                                    my/tex-table--context))
         (start (org-table-begin)))
    (delete-region start (org-table-end))
    (goto-char start)
    (insert tex)
    (let ((indent-tabs-mode nil))
      (ignore-errors
        (align-regexp start (point) "\\(\\s-*\\)&" 1 1 t)))
    (setq my/tex-table--spec nil
          my/tex-table--type nil
          my/tex-table--dialect nil
          my/tex-table--context nil)
    (my/tex-table-mode -1)))


;;;; Keys

(with-eval-after-load 'tex
  (keymap-set TeX-mode-map "C-c t" #'my/tex-table)
  (keymap-set TeX-mode-map "C-c m" #'my/tex-matrix))

(with-eval-after-load 'tex-mode
  (keymap-set tex-mode-map "C-c t" #'my/tex-table)
  (keymap-set tex-mode-map "C-c m" #'my/tex-matrix))

;;; tbl.el ends here
