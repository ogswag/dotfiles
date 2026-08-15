;;; tex-symbols.el --- Math symbol picker and TeX surround -*- lexical-binding: t; -*-

;;; Commentary:
;; pick tex/optex math symbols (fzf) and wrap region helpers.

;;; Code:

(require 'cl-lib)
(require 'seq)

(use-package math-symbol-lists :ensure t :defer t)

(declare-function laas-identify-adjacent-tex-object "laas" (&optional point))

(defgroup my-tex-symbols nil
  "Math symbol picker and TeX surround."
  :group 'tex
  :prefix "my/tex-")


;;;; Symbol inventory

(defcustom my/tex-symbol-extra-file
  (expand-file-name "m/tex-symbols-extra.el" my-config-directory)
  "Editable file of extra symbols."
  :type 'file
  :group 'my-tex-symbols)

(defcustom my/tex-symbol-sources
  '(math-symbol-list-basic
    math-symbol-list-extended
    math-symbol-list-packages)
  "Symbol list variables from `math-symbol-lists' to load."
  :type '(repeat symbol)
  :group 'my-tex-symbols)

(defcustom my/tex-symbol-brace-categories '("accent" "constr")
  "Categories whose commands are inserted as \\cmd{} with point inside."
  :type '(repeat string)
  :group 'my-tex-symbols)

(defvar my/tex-symbol-history nil
  "Recently inserted symbol commands (with leading backslash).")

(defvar my/tex-symbol--cache nil
  "Cached list of (COMMAND UNICODE CATEGORY) triples.")

(defun my/tex-symbol--load-extras ()
  "Return the list of extra symbol entries from `my/tex-symbol-extra-file'."
  (when (file-readable-p my/tex-symbol-extra-file)
    (with-temp-buffer
      (insert-file-contents my/tex-symbol-extra-file)
      (goto-char (point-min))
      (let ((form (read (current-buffer))))
        (if (eq (car-safe form) 'quote)
            (cadr form)
          form)))))

(defun my/tex-symbol--unicode (x)
  "Return a character from X (char, codepoint, or one-char string), or nil."
  (cond
   ((and (characterp x) (not (eq x t))) x)
   ((and (integerp x) (> x 0)) x)
   ((and (stringp x) (= (length x) 1)) (aref x 0))
   (t nil)))

(defun my/tex-symbol--normalize-entry (entry)
  "Normalize ENTRY to (COMMAND UNICODE-OR-NIL CATEGORY) or nil."
  (let (cat cmd uni)
    (pcase entry
      ;; packages: (pkg cat cmd uni char)
      ((and `(,(pred stringp) ,(pred stringp) ,(pred stringp) ,u . ,rest)
            (guard (string-prefix-p "\\" (nth 2 entry))))
       (setq cat (nth 1 entry)
             cmd (nth 2 entry)
             uni (or (my/tex-symbol--unicode (car-safe rest))
                     (my/tex-symbol--unicode u))))
      ;; extended/basic with unicode: (cat cmd uni [char])
      ((and `(,(pred stringp) ,(pred stringp) ,u . ,rest)
            (guard (string-prefix-p "\\" (nth 1 entry))))
       (setq cat (nth 0 entry)
             cmd (nth 1 entry)
             uni (or (my/tex-symbol--unicode (car-safe rest))
                     (my/tex-symbol--unicode u))))
      ;; bare (cat cmd)
      (`(,(pred stringp) ,(pred stringp))
       (setq cat (nth 0 entry)
             cmd (nth 1 entry)
             uni nil)))
    (when (and cmd (string-prefix-p "\\" cmd))
      (list cmd uni (or cat "misc")))))

(defun my/tex-symbol--build-cache ()
  "Rebuild `my/tex-symbol--cache' from the package lists and extras."
  (require 'math-symbol-lists)
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (src (append my/tex-symbol-sources
                         (list (my/tex-symbol--load-extras))))
      (let ((entries (if (symbolp src)
                         (symbol-value src)
                       src)))
        (dolist (e entries)
          (when-let* ((n (my/tex-symbol--normalize-entry e))
                      (cmd (car n))
                      ((not (gethash cmd seen))))
            (puthash cmd t seen)
            (push n out)))))
    (setq my/tex-symbol--cache (nreverse out))))

(defun my/tex-symbol--cache ()
  "Return the symbol cache, building it if needed."
  (or my/tex-symbol--cache (my/tex-symbol--build-cache)))

;;;###autoload
(defun my/tex-symbol-rebuild ()
  "Reload symbol lists and extras, dropping the cache."
  (interactive)
  (setq my/tex-symbol--cache nil)
  (message "tex-symbols: %d symbols" (length (my/tex-symbol--cache))))

(defun my/tex-symbol--affix (cands)
  "Affixation function: unicode preview | command | category."
  (let ((by-cmd (make-hash-table :test #'equal)))
    (dolist (e (my/tex-symbol--cache))
      (puthash (car e) e by-cmd))
    (mapcar
     (lambda (cmd)
       (let* ((e (gethash cmd by-cmd))
              (uni (and e (nth 1 e)))
              (cat (and e (nth 2 e)))
              (prefix (if uni
                          (format "%c  " uni)
                        "   "))
              (suffix (if cat (format "  %s" cat) "")))
         (list cmd prefix (propertize suffix 'face 'completions-annotations))))
     cands)))

(defun my/tex-symbol--sort (cands)
  "Sort CANDS: recently used first, then alphabetically."
  (let ((rank (make-hash-table :test #'equal))
        (i 0))
    (dolist (h my/tex-symbol-history)
      (unless (gethash h rank)
        (puthash h i rank)
        (setq i (1+ i))))
    (sort (copy-sequence cands)
          (lambda (a b)
            (let ((ra (gethash a rank most-positive-fixnum))
                  (rb (gethash b rank most-positive-fixnum)))
              (if (/= ra rb) (< ra rb) (string-lessp a b)))))))

(defun my/tex-symbol--remember (cmd)
  "Push CMD onto `my/tex-symbol-history'."
  (when (and cmd (not (string-empty-p cmd)))
    (unless (string-prefix-p "\\" cmd)
      (setq cmd (concat "\\" cmd)))
    (setq my/tex-symbol-history
          (cons cmd (delete cmd my/tex-symbol-history)))
    (when (> (length my/tex-symbol-history) 80)
      (setq my/tex-symbol-history (seq-take my/tex-symbol-history 80)))))

(defun my/tex-symbol--insert (cmd)
  "Insert CMD at point, with braces for accent/constr categories."
  (let* ((e (cl-find cmd (my/tex-symbol--cache) :key #'car :test #'equal))
         (cat (and e (nth 2 e)))
         (brace (and cat (member cat my/tex-symbol-brace-categories))))
    (my/tex-symbol--remember cmd)
    (if brace
        (progn (insert cmd "{}") (backward-char 1))
      (insert cmd))))

;;;###autoload
(defun my/tex-insert-symbol ()
  "Pick a TeX/OpTeX math symbol (fzf-style) and insert it at point."
  (interactive)
  (let* ((cands (mapcar #'car (my/tex-symbol--cache)))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (category . my/tex-symbol)
                        (affixation-function . my/tex-symbol--affix)
                        (display-sort-function . my/tex-symbol--sort)
                        (cycle-sort-function . my/tex-symbol--sort))
                    (complete-with-action action cands string pred))))
         (choice (completing-read "Symbol: " table nil t nil
                                  'my/tex-symbol-history)))
    (when (and choice (not (string-empty-p choice)))
      (my/tex-symbol--insert choice))))

;;;###autoload
(defun my/tex-symbol-add (category command &optional unicode)
  "Add COMMAND under CATEGORY (optional UNICODE char) to the extras file."
  (interactive
   (list (read-string "Category: " "mine")
         (let ((c (read-string "Command (with \\): " "\\")))
           (if (string-prefix-p "\\" c) c (concat "\\" c)))
         (let ((u (read-string "Unicode char (empty to skip): ")))
           (and (not (string-empty-p u)) (aref u 0)))))
  (unless (string-prefix-p "\\" command)
    (setq command (concat "\\" command)))
  (let* ((entry (if unicode
                    (list category command unicode)
                  (list category command)))
         (existing (or (my/tex-symbol--load-extras) '()))
         (updated (append existing (list entry))))
    (with-temp-file my/tex-symbol-extra-file
      (insert ";;; tex-symbols-extra.el --- User TeX/OpTeX symbols -*- lexical-binding: t; -*-\n\n")
      (insert ";; Entries: (CATEGORY COMMAND &optional UNICODE)\n")
      (insert ";; Edited by `my/tex-symbol-add' or by hand.  Then M-x my/tex-symbol-rebuild.\n\n")
      (pp updated (current-buffer))
      (insert "\n"))
    (my/tex-symbol-rebuild)
    (message "Added %s" command)))

;;;###autoload
(defun my/tex-symbol-open-extras ()
  "Visit the editable extras symbol file."
  (interactive)
  (find-file my/tex-symbol-extra-file))


;;;; Surround

(defcustom my/tex-surround-alist
  '(("\\vec{...}"        . ("\\vec{" . "}"))
    ("\\hat{...}"        . ("\\hat{" . "}"))
    ("\\bar{...}"        . ("\\bar{" . "}"))
    ("\\tilde{...}"      . ("\\tilde{" . "}"))
    ("\\dot{...}"        . ("\\dot{" . "}"))
    ("\\ddot{...}"       . ("\\ddot{" . "}"))
    ("\\sqrt{...}"       . ("\\sqrt{" . "}"))
    ("\\overline{...}"   . ("\\overline{" . "}"))
    ("\\underline{...}"  . ("\\underline{" . "}"))
    ("\\mathbb{...}"     . ("\\mathbb{" . "}"))
    ("\\mathcal{...}"    . ("\\mathcal{" . "}"))
    ("\\mathrm{...}"     . ("\\mathrm{" . "}"))
    ("\\mathbf{...}"     . ("\\mathbf{" . "}"))
    ("\\mathit{...}"     . ("\\mathit{" . "}"))
    ("\\mathsf{...}"     . ("\\mathsf{" . "}"))
    ("\\mathtt{...}"     . ("\\mathtt{" . "}"))
    ("\\text{...}"       . ("\\text{" . "}"))
    ("\\textbf{...}"     . ("\\textbf{" . "}"))
    ("\\textit{...}"     . ("\\textit{" . "}"))
    ("{\\bf ...}"        . ("{\\bf " . "}"))
    ("{\\it ...}"        . ("{\\it " . "}"))
    ("{\\rm ...}"        . ("{\\rm " . "}"))
    ("{\\tt ...}"        . ("{\\tt " . "}"))
    ("{\\em ...}"        . ("{\\em " . "}"))
    ("\\жирный{...}"     . ("\\жирный{" . "}"))
    ("\\курсив{...}"     . ("\\курсив{" . "}"))
    ("\\выделение{...}"  . ("\\выделение{" . "}"))
    ("\\текст{...}"      . ("\\текст{" . "}"))
    ("\\mathbox{...}"    . ("\\mathbox{" . "}"))
    ("\\код{...}"        . ("\\код{" . "}"))
    ("\\рамка{...}"      . ("\\рамка{" . "}"))
    ("\\left(...\\right)" . ("\\left(" . "\\right)"))
    ("\\left[...\\right]" . ("\\left[" . "\\right]"))
    ("\\left\\{...\\right\\}" . ("\\left\\{" . "\\right\\}"))
    ("\\left|...\\right|" . ("\\left|" . "\\right|"))
    ("$...$"             . ("$" . "$"))
    ("$$...$$"           . ("$$" . "$$"))
    ;; OpTeX environments.
    ("\\теор ...\\конец"  . ("\\теор[] " . "\n\\конец\n"))
    ("\\лем ...\\конец"   . ("\\лем[] " . "\n\\конец\n"))
    ("\\опр ...\\конец"   . ("\\опр[] " . "\n\\конец\n"))
    ("\\прим ...\\конец"  . ("\\прим[] " . "\n\\конец\n"))
    ("\\предл ...\\конец" . ("\\предл[] " . "\n\\конец\n"))
    ("\\след ...\\конец"  . ("\\след[] " . "\n\\конец\n"))
    ("\\зам ...\\конец"   . ("\\зам[] " . "\n\\конец\n"))
    ("\\док ...\\конец"   . ("\\док\n" . "\n\\конец\n"))
    ("\\список \\стиль ...\\консписка" . my/tex-surround-optex-items)
    ("\\подпись/р ...\\отбивка" . ("\\подпись/р [] " . "\n\\отбивка\n"))
    ("\\подпись/т ...\\отбивка" . ("\\подпись/т [] " . "\n\\отбивка\n"))
    ("\\proof ...\\endthm" . ("\\proof\n" . "\n\\endthm\n"))
    ("\\begitems \\style ...\\enditems" . my/tex-surround-optex-begitems)
    ("\\caption/f ...\\cskip" . ("\\caption/f [] " . "\n\\cskip\n"))
    ("\\caption/t ...\\cskip" . ("\\caption/t [] " . "\n\\cskip\n")))
  "Wrappers offered by `my/tex-surround'. Each entry is (DISPLAY . WRAPPER): WRAPPER is (LEFT . RIGHT) or a niladic."
  :type '(alist :key-type string
                :value-type (choice (cons (string :tag "Left")
                                          (string :tag "Right"))
                                    (function :tag "Function returning (L . R)")))
  :group 'my-tex-symbols)

(defvar my/tex-surround-history nil
  "Recently used surround display labels.")

(declare-function optex-read-item-style "optex" ())

(defun my/tex-surround--item-style ()
  "Read an OpTeX list-item style letter."
  (if (fboundp 'optex-read-item-style)
      (or (optex-read-item-style) "o")
    (read-string "Style letter: " "o")))

(defun my/tex-surround-optex-items ()
  "Wrapper pair for a Russian OpTeX item list."
  (cons (format "\\список \\стиль %s\n* " (my/tex-surround--item-style))
        "\n\\консписка\n"))

(defun my/tex-surround-optex-begitems ()
  "Wrapper pair for an OpTeX item list."
  (cons (format "\\begitems \\style %s\n* " (my/tex-surround--item-style))
        "\n\\enditems\n"))

(defun my/tex-surround--apply (left right beg end)
  "Insert LEFT at BEG and RIGHT at END; leave point after RIGHT."
  (save-excursion
    (goto-char end)
    (insert right)
    (goto-char beg)
    (insert left))
  (goto-char (+ end (length left) (length right))))

(defun my/tex-surround--region-or-object ()
  "Return (BEG . END) for the active region or the previous TeX object."
  (cond
   ((use-region-p)
    (cons (region-beginning) (region-end)))
   ((require 'laas nil t)
    (when-let* ((start (ignore-errors (laas-identify-adjacent-tex-object))))
      (cons start (point))))
   (t nil)))

;;;###autoload
(defun my/tex-surround ()
  "Surround the region (or previous TeX token) with a chosen wrapper."
  (interactive)
  (let* ((table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . my/tex-surround))
                    (complete-with-action
                     action (mapcar #'car my/tex-surround-alist)
                     string pred))))
         (choice (completing-read "Surround: " table nil t nil
                                  'my/tex-surround-history))
         (entry (cdr (assoc choice my/tex-surround-alist))))
    (unless entry
      (user-error "Unknown wrapper: %s" choice))
    ;; Recorded before the wrapper gets to ask anything.
    (setq my/tex-surround-history
          (cons choice (delete choice my/tex-surround-history)))
    (pcase-let ((`(,left . ,right)
                 (if (functionp entry) (funcall entry) entry))
                (bounds (my/tex-surround--region-or-object)))
      (if bounds
          (progn
            (when (use-region-p) (deactivate-mark))
            (my/tex-surround--apply left right (car bounds) (cdr bounds)))
        (insert left right)
        (backward-char (length right))))))


;;;; Bindings

(defun my/tex-symbols-setup-keys ()
  "Bind symbol picker and surround on TeX keymaps."
  (when (boundp 'TeX-mode-map)
    (keymap-set TeX-mode-map "C-c s" #'my/tex-insert-symbol)
    (keymap-set TeX-mode-map "C-c w" #'my/tex-surround))
  (when (boundp 'tex-mode-map)
    (keymap-set tex-mode-map "C-c s" #'my/tex-insert-symbol)
    (keymap-set tex-mode-map "C-c w" #'my/tex-surround)))

(with-eval-after-load 'tex (my/tex-symbols-setup-keys))
(with-eval-after-load 'tex-mode (my/tex-symbols-setup-keys))

;;; tex-symbols.el ends here
