;;; neofile.el --- Untitled buffers, optionally typed -*- lexical-binding: t; -*-

;;; Commentary:
;; untitled buffers, optional type. nothing hits disk until you save.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)

(declare-function nerd-icons-icon-for-extension "nerd-icons" (ext &rest arg-overrides))
(defvar nerd-icons-extension-icon-alist)
(defvar nerd-icons-font-family)
(defvar vertico--index)

;;;; Customization

(defgroup neofile nil
  "Untitled buffers, optionally typed."
  :group 'files
  :prefix "neofile-")

(defcustom neofile-buffer-name-format "untitled %d"
  "Format for untitled buffer names.  Must contain a single %d."
  :type 'string
  :group 'neofile)

(defcustom neofile-icons 'auto
  "Whether to show nerd-icons in the file type picker."
  :type '(choice (const :tag "When renderable" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'neofile)

(defcustom neofile-tint t
  "Whether to tint the selected line of the picker with the type's icon color."
  :type 'boolean
  :group 'neofile)

(defcustom neofile-tint-saturation '(0.25 . 0.75)
  "Saturation clamp for the tint, as (MINIMUM ."
  :type '(cons (float :tag "Minimum") (float :tag "Maximum"))
  :group 'neofile)

(defcustom neofile-tint-min-contrast 2
  "Minimum contrast ratio between the tint and the default foreground."
  :type 'float
  :group 'neofile)

(defcustom neofile-type-names
  '(("el"    . "Emacs Lisp")
    ("tex"   . "LaTeX")
    ("sty"   . "LaTeX Style")
    ("cls"   . "LaTeX Class")
    ("bib"   . "BibTeX")
    ("h"     . "C/C++ Header")
    ("hpp"   . "C++ Header")
    ("hh"    . "C++ Header")
    ("cc"    . "C++")
    ("cpp"   . "C++")
    ("cxx"   . "C++")
    ("c"     . "C")
    ("m"     . "Objective-C")
    ("mm"    . "Objective-C++")
    ("rs"    . "Rust")
    ("go"    . "Go")
    ("py"    . "Python")
    ("rb"    . "Ruby")
    ("js"    . "JavaScript")
    ("mjs"   . "JavaScript (ESM)")
    ("cjs"   . "JavaScript (CommonJS)")
    ("jsx"   . "JavaScript (JSX)")
    ("ts"    . "TypeScript")
    ("tsx"   . "TypeScript (JSX)")
    ("sh"    . "Shell")
    ("bash"  . "Bash")
    ("zsh"   . "Zsh")
    ("fish"  . "Fish")
    ("md"    . "Markdown")
    ("org"   . "Org")
    ("txt"   . "Plain Text")
    ("json"  . "JSON")
    ("yml"   . "YAML")
    ("yaml"  . "YAML")
    ("toml"  . "TOML")
    ("ini"   . "INI")
    ("csv"   . "CSV")
    ("html"  . "HTML")
    ("css"   . "CSS")
    ("scss"  . "Sass (SCSS)")
    ("sql"   . "SQL")
    ("hs"    . "Haskell")
    ("ml"    . "OCaml")
    ("ex"    . "Elixir")
    ("cs"    . "C#")
    ("kt"    . "Kotlin")
    ("swift" . "Swift")
    ("zig"   . "Zig")
    ("lua"   . "Lua")
    ("php"   . "PHP")
    ("pl"    . "Perl")
    ("r"     . "R")
    ("jl"    . "Julia")
    ("nix"   . "Nix")
    ("dockerfile" . "Dockerfile"))
  "Display names for file types, as an alist of (EXTENSION ."
  :type '(alist :key-type (string :tag "Extension")
                :value-type (string :tag "Name"))
  :group 'neofile)

(defvar neofile-type-history nil
  "Minibuffer history for `neofile-new-file-with-type'.")

;;;; Buffer naming

(defun neofile--name-taken-p (n)
  "Non-nil if a live buffer is untitled N, with or without an extension."
  (let ((base (format neofile-buffer-name-format n)))
    (cl-some (lambda (buf)
               (let ((name (buffer-name buf)))
                 (and name
                      (string-prefix-p base name)
                      (or (= (length name) (length base))
                          (eq (aref name (length base)) ?.)))))
             (buffer-list))))

(defun neofile--next-number ()
  "Return the lowest untitled number not already in use."
  (let ((n 1))
    (while (neofile--name-taken-p n)
      (setq n (1+ n)))
    n))

(defun neofile--buffer-name (&optional ext)
  "Return the next free untitled buffer name, suffixed with EXT if given."
  (let ((base (format neofile-buffer-name-format (neofile--next-number))))
    (if (and ext (not (string-empty-p ext)))
        (concat base "." ext)
      base)))

;;;; Type table

(defconst neofile--extension-regexp
  "\\`\\\\\\.\\([[:alnum:]_+-]+\\)\\\\'\\'"
  "Matches an `auto-mode-alist' key of the plain \"\\\\.EXT\\\\='\" shape. Anything fancier -- character alternatives, embedded.")

(defvar neofile--nerd-icons 'unset
  "Cached result of loading nerd-icons: the feature symbol, or nil.")

(defun neofile--nerd-icons-p ()
  "Return non-nil if nerd-icons is installed, loading it on first call."
  (when (eq neofile--nerd-icons 'unset)
    (setq neofile--nerd-icons (require 'nerd-icons nil t)))
  neofile--nerd-icons)

(defun neofile--auto-mode-extensions ()
  "Return an alist of (EXTENSION . MODE) parsed from `auto-mode-alist'."
  (let (result)
    (dolist (entry auto-mode-alist)
      (let ((key (car entry))
            (val (cdr entry)))
        ;; A cons cdr is the (REGEXP FUNCTION NON-NIL) "keep looking" form.
        (when (and (stringp key) val (symbolp val)
                   (string-match neofile--extension-regexp key))
          (push (cons (downcase (match-string 1 key)) val) result))))
    (nreverse result)))

(defun neofile--mode-label (mode)
  "Return a display name derived from MODE, or nil."
  (when (and mode (symbolp mode))
    (let ((name (replace-regexp-in-string "-ts-mode\\'\\|-mode\\'" ""
                                          (symbol-name mode))))
      (unless (string-empty-p name)
        (capitalize (replace-regexp-in-string "-" " " name))))))

(defvar neofile--types nil
  "Cached alist of (EXTENSION . LABEL); see `neofile--types'.")

(defun neofile--types ()
  "Return the alist of (EXTENSION . LABEL) offered by the picker."
  (or neofile--types
      (setq neofile--types
            (let* ((modes (neofile--auto-mode-extensions))
                   (exts (delete-dups
                          (append
                           (mapcar #'car modes)
                           (and (neofile--nerd-icons-p)
                                (mapcar (lambda (cell) (downcase (car cell)))
                                        nerd-icons-extension-icon-alist))))))
              (sort (mapcar
                     (lambda (ext)
                       (cons ext
                             (or (cdr (assoc ext neofile-type-names))
                                 (neofile--mode-label (cdr (assoc ext modes)))
                                 (upcase ext))))
                     exts)
                    (lambda (a b) (string< (car a) (car b))))))))

;;;; Icons

(defun neofile--icon-family ()
  "Return the font family nerd-icons renders with, or nil."
  (or (and (boundp 'nerd-icons-font-family) nerd-icons-font-family)
      ;; m/font.el already picked the best nerd family present on this machine.
      (and (boundp 'my/nerd-mono-font) (symbol-value 'my/nerd-mono-font))))

(defun neofile--glyph-renderable-p ()
  "Non-nil if nerd-icons glyphs would actually show up on this display."
  (and (display-graphic-p)
       (let ((family (neofile--icon-family))
             (glyph (ignore-errors
                      (aref (nerd-icons-icon-for-extension "txt") 0))))
         (or (and family (member family (font-family-list)))
             (and glyph (char-displayable-p glyph))))))

(defun neofile--icons-p ()
  "Non-nil if the picker should show icons."
  (pcase neofile-icons
    ('nil nil)
    ('t (neofile--nerd-icons-p))
    (_ (and (neofile--nerd-icons-p) (neofile--glyph-renderable-p)))))

(defun neofile--icon (ext)
  "Return the propertized nerd-icons glyph for EXT, or nil."
  (when (neofile--nerd-icons-p)
    (ignore-errors (nerd-icons-icon-for-extension ext))))

(defun neofile--face-color (face)
  "Return the foreground color named by FACE, or nil."
  (let ((spec (cond
               ((null face) nil)
               ((and (symbolp face) (facep face)) face)
               ((and (consp face) (keywordp (car face))) (plist-get face :inherit))
               ((consp face) (seq-find #'facep face)))))
    (when (and spec (facep spec))
      (let ((color (face-attribute spec :foreground nil t)))
        (and (stringp color) color)))))

(defun neofile--icon-color (ext)
  "Return the foreground color of EXT's icon, or nil."
  (when-let* ((icon (neofile--icon ext))
              ((> (length icon) 0)))
    (neofile--face-color (get-text-property 0 'face icon))))

;;;; Tint

(defun neofile--rgb (color)
  "Return COLOR as an (R G B) list of floats, or nil."
  (and (stringp color) (color-name-to-rgb color)))

(defun neofile--attr-rgb (face attribute)
  "Return FACE's ATTRIBUTE as an (R G B) list of floats, or nil."
  (and (facep face)
       (neofile--rgb (face-attribute face attribute nil t))))

(defun neofile--selection-rgb ()
  "Return the theme's selection background as (R G B), or nil."
  (or (neofile--attr-rgb 'vertico-current :background)
      (neofile--attr-rgb 'highlight :background)
      (neofile--attr-rgb 'region :background)
      (neofile--attr-rgb 'default :background)))

(defun neofile--luminance (rgb)
  "Return the WCAG relative luminance of RGB."
  (let ((v (mapcar (lambda (x)
                     (if (<= x 0.03928)
                         (/ x 12.92)
                       (expt (/ (+ x 0.055) 1.055) 2.4)))
                   rgb)))
    (+ (* 0.2126 (nth 0 v)) (* 0.7152 (nth 1 v)) (* 0.0722 (nth 2 v)))))

(defun neofile--contrast (a b)
  "Return the WCAG contrast ratio between colors A and B, both (R G B)."
  (let ((la (neofile--luminance a))
        (lb (neofile--luminance b)))
    (/ (+ 0.05 (max la lb)) (+ 0.05 (min la lb)))))

(defun neofile--compute-tint (ext)
  "Return a hex tint for EXT's selected line, or nil if none is usable."
  (when-let* ((icon (neofile--rgb (neofile--icon-color ext)))
              (base (neofile--selection-rgb))
              (fg (neofile--attr-rgb 'default :foreground)))
    (let* ((hue (nth 0 (apply #'color-rgb-to-hsl icon)))
           (sat (min (cdr neofile-tint-saturation)
                     (max (car neofile-tint-saturation)
                          (nth 1 (apply #'color-rgb-to-hsl icon)))))
           ;; Start at the lightness the theme already picked for its own selection bar.
           (from (nth 2 (apply #'color-rgb-to-hsl base)))
           (to (or (and-let* ((bg (neofile--attr-rgb 'default :background)))
                     (nth 2 (apply #'color-rgb-to-hsl bg)))
                   from)))
      (cl-loop for i from 0 to 20
               for lum = (+ from (* (/ i 20.0) (- to from)))
               for rgb = (color-hsl-to-rgb hue sat lum)
               when (>= (neofile--contrast rgb fg) neofile-tint-min-contrast)
               return (apply #'color-rgb-to-hex (append rgb '(2)))))))

(defvar neofile--tint-cache (make-hash-table :test #'equal)
  "Extension to hex tint, rebuilt whenever the theme changes.")

(defun neofile--tint-p ()
  "Non-nil if tinting is on and this display has the colors for it."
  (and neofile-tint
       (display-graphic-p)
       (> (display-color-cells) 256)))

(defun neofile--tint-color (ext)
  "Return the cached tint for EXT, computing it on first use."
  (when (neofile--tint-p)
    (let ((hit (gethash ext neofile--tint-cache 'miss)))
      (if (eq hit 'miss)
          (puthash ext (neofile--compute-tint ext) neofile--tint-cache)
        hit))))

(defun neofile--tint-reset (&rest _)
  "Drop cached tints. On `enable-theme-functions': m/theme.el swaps the whole theme at runtime whenever macOS changes."
  (clrhash neofile--tint-cache))

(add-hook 'enable-theme-functions #'neofile--tint-reset)

;;;; Vertico glue

(defvar neofile--tint-active nil
  "Bound non-nil only while `neofile--read-type' is reading.")

(defun neofile--tint-advice (orig cand prefix suffix index start)
  "Tint the selected candidate."
  (let ((str (funcall orig cand prefix suffix index start))
        (current (bound-and-true-p vertico--index)))
    (when (and neofile--tint-active (integerp index) (eql index current))
      ;; Look the color up from the text rather than a text property: the string has been through.
      (when-let* ((bg (neofile--tint-color (substring-no-properties cand))))
        (add-face-text-property 0 (length str)
                                `(:background ,bg :extend t) nil str)))
    str))

(with-eval-after-load 'vertico
  (advice-add 'vertico--format-candidate :around #'neofile--tint-advice))

;;;; Completion

(defun neofile--affix (candidates)
  "Return CANDIDATES as (CANDIDATE ICON LABEL) triples."
  (let ((icons (neofile--icons-p))
        (types (neofile--types)))
    (mapcar
     (lambda (cand)
       (let* ((ext (substring-no-properties cand))
              (label (or (cdr (assoc ext types)) (upcase ext)))
              (icon (and icons (neofile--icon ext))))
         (list cand
               (if icon (concat icon " ") "")
               (concat "  " (propertize label 'face 'completions-annotations)))))
     candidates)))

(defun neofile--read-type ()
  "Read a file type and return its extension, or nil."
  (let* ((cands (mapcar #'car (neofile--types)))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (category . neofile-type)
                        (affixation-function . neofile--affix))
                    (complete-with-action action cands string pred))))
         (neofile--tint-active t)
         (choice (completing-read (format "File type (%d): " (length cands))
                                  table nil nil nil 'neofile-type-history)))
    (let ((ext (string-trim (substring-no-properties (or choice "")))))
      (unless (string-empty-p ext)
        (string-remove-prefix "." (downcase ext))))))

;;;; Commands

(defun neofile--set-mode (name)
  "Set the major mode `auto-mode-alist' would pick for NAME."
  (when-let* (((string-match-p "\\." name))
              (mode (assoc-default name auto-mode-alist #'string-match))
              (mode (if (consp mode) (car mode) mode))
              ((and (symbolp mode) (fboundp mode))))
    (condition-case err
        (funcall mode)
      (error
       (message "neofile: %s failed: %s" mode (error-message-string err))))))

(defun neofile--create (ext)
  "Create and switch to a new untitled buffer, typed EXT if non-nil."
  (let* ((dir default-directory)
         (name (neofile--buffer-name ext))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq default-directory dir)
      (neofile--set-mode name)
      (setq buffer-offer-save t))
    (switch-to-buffer buffer)
    buffer))

;;;###autoload
(defun neofile-new-file-fast ()
  "Create and switch to a new untitled buffer."
  (interactive)
  (neofile--create nil))

;;;###autoload
(defun neofile-new-file-with-type ()
  "Ask for a file type, then create and switch to a new untitled buffer."
  (interactive)
  (neofile--create (neofile--read-type)))

(provide 'neofile)
;;; neofile.el ends here
