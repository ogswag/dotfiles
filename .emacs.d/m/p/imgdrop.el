;;; imgdrop.el --- Drop and pick images into markup buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; drop/pick an image into a markup buffer; files it next to the doc and inserts the macro.

;;; Code:

(require 'dnd)
(require 'image)
(require 'image-file)
(require 'seq)
(require 'subr-x)

;; AUCTeX, markdown-mode and vertico are all optional here: each is used only
(declare-function TeX-master-directory "tex" ())
(declare-function TeX-master-file "tex" (&optional extension nondirectory ask))
(declare-function markdown-insert-inline-image "markdown-mode"
                  (text url &optional title))
(declare-function vertico--candidate "vertico" (&optional hl))
(defvar optex--russian)

;;;; Customization

(defgroup imgdrop nil
  "Drop and pick images into markup buffers."
  :group 'files
  :prefix "imgdrop-")

(defcustom imgdrop-image-extensions
  (append image-file-name-extensions '("pdf" "eps" "heic" "avif"))
  "File name extensions treated as images, without the leading dot."
  :type '(repeat string)
  :group 'imgdrop)

(defcustom imgdrop-file-action 'copy
  "What to do with the picked or dropped file."
  :type '(choice (const :tag "Copy beside the document" copy)
                 (const :tag "Move beside the document" move)
                 (const :tag "Leave in place" link))
  :group 'imgdrop)

(defcustom imgdrop-image-directory "img/"
  "Subdirectory of the document's directory that images are put in."
  :type '(choice (const :tag "Beside the document" nil) string)
  :group 'imgdrop)

(defcustom imgdrop-file-name-function #'imgdrop-file-name-keep
  "Function returning the name to save an image under."
  :type '(choice (const :tag "Keep the original name" imgdrop-file-name-keep)
                 (const :tag "Timestamp" imgdrop-file-name-timestamp)
                 (const :tag "Ask" imgdrop-file-name-read)
                 function)
  :group 'imgdrop)

(defcustom imgdrop-insert-functions
  '((LaTeX-mode     . imgdrop-insert-latex)
    (latex-mode     . imgdrop-insert-latex)
    (plain-TeX-mode . imgdrop-insert-optex)
    (plain-tex-mode . imgdrop-insert-optex)
    (TeX-mode       . imgdrop-insert-latex)
    (tex-mode       . imgdrop-insert-latex)
    (markdown-mode  . imgdrop-insert-markdown)
    (org-mode       . imgdrop-insert-org)
    (html-mode      . imgdrop-insert-html)
    (text-mode      . imgdrop-insert-path))
  "How each major mode spells an image, as (MODE ."
  :type '(alist :key-type symbol :value-type function)
  :group 'imgdrop)

(defcustom imgdrop-tex-centered t
  "Whether TeX and LaTeX markup is wrapped in \\centerline{...}."
  :type 'boolean
  :group 'imgdrop)

(defcustom imgdrop-tex-width ".8\\hsize"
  "Width given to OpTeX's \\picw."
  :type 'string
  :group 'imgdrop)

(defcustom imgdrop-latex-width "0.8\\linewidth"
  "Width given to \\includegraphics."
  :type 'string
  :group 'imgdrop)

(defcustom imgdrop-optex-russian 'auto
  "Whether OpTeX markup uses the Cyrillic macro names from rutex.tex."
  :type '(choice (const :tag "Follow the buffer" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'imgdrop)

(defcustom imgdrop-preview t
  "Whether `imgdrop-insert-image' previews the highlighted candidate."
  :type 'boolean
  :group 'imgdrop)

(defcustom imgdrop-preview-delay 0.15
  "Idle seconds before the picker's preview catches up with point."
  :type 'number
  :group 'imgdrop)

(defcustom imgdrop-preview-width 0.4
  "Width of the preview side window, as a fraction of the frame."
  :type 'number
  :group 'imgdrop)

;;;; Files

(defun imgdrop-image-file-p (file)
  "Return non-nil when FILE's extension is in `imgdrop-image-extensions'."
  (and (stringp file)
       (let ((ext (file-name-extension file)))
         (and ext (seq-contains-p imgdrop-image-extensions ext #'string-equal-ignore-case)))))

(defun imgdrop--hash (file)
  "Return a hash of FILE's contents."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun imgdrop--same-file-p (a b)
  "Return non-nil when files A and B hold the same bytes."
  (and (file-regular-p a) (file-regular-p b)
       (= (file-attribute-size (file-attributes a))
          (file-attribute-size (file-attributes b)))
       (string= (imgdrop--hash a) (imgdrop--hash b))))

(defun imgdrop--unique (name directory source)
  "Return NAME, made unique within DIRECTORY, for saving SOURCE."
  (let ((target (expand-file-name name directory)))
    (if (or (not (file-exists-p target)) (imgdrop--same-file-p target source))
        name
      (let* ((base (file-name-sans-extension name))
             (ext (file-name-extension name))
             (n 1)
             candidate)
        (while (progn
                 (setq candidate (if ext (format "%s-%d.%s" base n ext)
                                   (format "%s-%d" base n)))
                 (setq target (expand-file-name candidate directory))
                 (and (file-exists-p target)
                      (not (imgdrop--same-file-p target source))))
          (setq n (1+ n)))
        candidate))))

(defun imgdrop-file-name-keep (source directory)
  "Return SOURCE's own base name, made unique within DIRECTORY."
  (imgdrop--unique (file-name-nondirectory source) directory source))

(defun imgdrop-file-name-timestamp (source directory)
  "Return a timestamped name for SOURCE, made unique within DIRECTORY."
  (let ((ext (file-name-extension source)))
    (imgdrop--unique (concat (format-time-string "%Y%m%dT%H%M%S")
                             (if ext (concat "." ext) ""))
                     directory source)))

(defun imgdrop-file-name-read (source directory)
  "Ask for the name to save SOURCE under in DIRECTORY."
  (imgdrop--unique (read-string "Save image as: " (file-name-nondirectory source))
                   directory source))

;;;; Where the document keeps its pictures

(defun imgdrop--tex-p ()
  "Return non-nil in a buffer whose major mode is some flavour of TeX."
  (derived-mode-p 'TeX-mode 'tex-mode))

(defun imgdrop--document-directory ()
  "Return the directory the document's paths are relative to."
  (or (and (imgdrop--tex-p)
           (fboundp 'TeX-master-directory)
           (ignore-errors (TeX-master-directory)))
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun imgdrop--master-file ()
  "Return the file that governs this document, or nil."
  (or (and (imgdrop--tex-p)
           (fboundp 'TeX-master-file)
           (ignore-errors
             (when-let* ((name (TeX-master-file "tex" t)))
               (expand-file-name name (imgdrop--document-directory)))))
      buffer-file-name))

(defun imgdrop--tex-commented-p (pos)
  "Return non-nil when POS is after an unescaped % on its own line."
  (save-excursion
    (goto-char pos)
    (let ((limit (point))
          (commented nil))
      (goto-char (line-beginning-position))
      (while (and (not commented) (search-forward "%" limit t))
        (unless (eq (char-before (1- (point))) ?\\)
          (setq commented t)))
      commented)))

(defun imgdrop--picdir ()
  "Return the document's \\picdir value, or nil."
  (when-let* ((master (imgdrop--master-file))
              ((file-readable-p master)))
    (with-temp-buffer
      (insert-file-contents master)
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "\\\\picdir[ \t]*=[ \t]*{\\([^}]*\\)}" nil t)
          (let ((value (match-string 1)))
            (when (and (not (imgdrop--tex-commented-p (match-beginning 0)))
                       (string-suffix-p "/" value))
              (throw 'found value))))
        nil))))

(defun imgdrop--destination (source)
  "Return (TARGET . LINK) for SOURCE."
  (let ((docdir (imgdrop--document-directory)))
    (if (eq imgdrop-file-action 'link)
        (cons source (file-relative-name source docdir))
      (let* ((picdir (and (imgdrop--tex-p) (imgdrop--picdir)))
             (subdir (or picdir imgdrop-image-directory ""))
             (dir (file-name-as-directory (expand-file-name subdir docdir)))
             (name (funcall imgdrop-file-name-function source dir)))
        ;; With \picdir in force the path is relative to *it*, not to the document.
        (cons (expand-file-name name dir)
              (if picdir name (file-relative-name (expand-file-name name dir) docdir)))))))

;;;; Markup

(defun imgdrop--optex-russian-p ()
  "Return non-nil when OpTeX markup should use the Cyrillic macro names."
  (if (eq imgdrop-optex-russian 'auto)
      (bound-and-true-p optex--russian)
    imgdrop-optex-russian))

(defun imgdrop--tex-wrap (body)
  "Return BODY centred, or as it stands, per `imgdrop-tex-centered'."
  (if imgdrop-tex-centered (format "\\centerline{%s}" body) body))

(defun imgdrop-insert-optex (link)
  "Insert OpTeX markup for LINK at point."
  (let ((russian (imgdrop--optex-russian-p)))
    (insert (imgdrop--tex-wrap
             (format "%s=%s %s{%s}"
                     (if russian "\\ширинакартинки" "\\picw")
                     imgdrop-tex-width
                     (if russian "\\картинка" "\\inspic")
                     link)))))

(defun imgdrop-insert-latex (link)
  "Insert LaTeX markup for LINK at point."
  (insert (imgdrop--tex-wrap
           (format "\\includegraphics[width=%s]{%s}" imgdrop-latex-width link))))

(defun imgdrop-insert-markdown (link)
  "Insert Markdown markup for LINK at point."
  (if (fboundp 'markdown-insert-inline-image)
      (markdown-insert-inline-image "" link)
    (insert (format "![](%s)" link))))

(defun imgdrop-insert-org (link)
  "Insert an Org link to LINK at point."
  (insert (format "[[file:%s]]" link)))

(defun imgdrop-insert-html (link)
  "Insert an HTML image element for LINK at point."
  (insert (format "<img src=\"%s\" alt=\"\" />" link)))

(defun imgdrop-insert-path (link)
  "Insert LINK at point, bare.  The fallback for modes with no markup."
  (insert link))

(defun imgdrop--inserter ()
  "Return the insertion function for this buffer's major mode."
  (or (seq-some (lambda (cell)
                  (and (derived-mode-p (car cell)) (cdr cell)))
                imgdrop-insert-functions)
      #'imgdrop-insert-path))

;;;; Placing

(defun imgdrop--place (source)
  "Put SOURCE where the document keeps its pictures and mark it up at point."
  (unless (and (stringp source) (file-readable-p source))
    (user-error "Cannot read %s" (or source "nothing")))
  (pcase-let ((`(,target . ,link) (imgdrop--destination source)))
    (unless (equal (file-truename target) (file-truename source))
      (make-directory (file-name-directory target) t)
      (pcase imgdrop-file-action
        ('copy (copy-file source target t))
        ('move (rename-file source target t))))
    (funcall (imgdrop--inserter) link)
    link))

;;;; Drag and drop

(defconst imgdrop--url-regexp "\\`file:"
  "URLs `imgdrop-mode' looks at. Deliberately broad: the extension test lives in `imgdrop--dnd-handler' instead, because a regexp would have to match extensions in whatever case the drop happened to arrive in, under whatever `case-fold-search' `dnd-handle-multiple-urls' was called with.")

(defvar-local imgdrop--prior-dnd-alist nil
  "The effective `dnd-protocol-alist' from before `imgdrop-mode'.")

(defvar-local imgdrop--dnd-was-local nil
  "Whether `dnd-protocol-alist' was already buffer-local before the mode.")

(defvar-local imgdrop--indicate-was-local nil
  "Whether `dnd-indicate-insertion-point' was already buffer-local.")

(defun imgdrop--dnd-delegate (url action)
  "Hand URL and ACTION to whatever would have handled it before the mode."
  (let ((handler (seq-some (lambda (cell)
                             (and (string-match-p (car cell) url) (cdr cell)))
                           imgdrop--prior-dnd-alist)))
    (cond
     ((null handler) (dnd-open-local-file url action))
     ((and (symbolp handler) (get handler 'dnd-multiple-handler))
      (funcall handler (list url) action))
     (t (funcall handler url action)))))

(defun imgdrop--dnd-handler (url action)
  "Insert markup for a dropped image URL, or delegate."
  (let ((file (dnd-get-local-file-name url t)))
    (if (imgdrop-image-file-p file)
        (progn (imgdrop--place file) 'private)
      (imgdrop--dnd-delegate url action))))

;;;###autoload
(define-minor-mode imgdrop-mode
  "Insert dropped images as markup instead of opening them."
  :lighter nil
  :group 'imgdrop
  (if imgdrop-mode
      ;; Idempotent: `mhtml-mode' runs `html-mode-hook' as well as its own.
      (unless (eq (cdar dnd-protocol-alist) #'imgdrop--dnd-handler)
        (setq imgdrop--prior-dnd-alist dnd-protocol-alist
              imgdrop--dnd-was-local (local-variable-p 'dnd-protocol-alist)
              imgdrop--indicate-was-local
              (local-variable-p 'dnd-indicate-insertion-point))
        (setq-local dnd-protocol-alist
                    (cons (cons imgdrop--url-regexp #'imgdrop--dnd-handler)
                          dnd-protocol-alist))
        (setq-local dnd-indicate-insertion-point t))
    (when (eq (cdar dnd-protocol-alist) #'imgdrop--dnd-handler)
      (if imgdrop--dnd-was-local
          (setq-local dnd-protocol-alist imgdrop--prior-dnd-alist)
        (kill-local-variable 'dnd-protocol-alist))
      (unless imgdrop--indicate-was-local
        (kill-local-variable 'dnd-indicate-insertion-point))
      (setq imgdrop--prior-dnd-alist nil))))

(defun imgdrop--turn-on ()
  "Enable `imgdrop-mode' when this buffer's mode has known markup."
  (when (seq-some (lambda (cell) (derived-mode-p (car cell)))
                  imgdrop-insert-functions)
    (imgdrop-mode 1)))

;;;###autoload
(define-globalized-minor-mode imgdrop-global-mode imgdrop-mode imgdrop--turn-on
  :group 'imgdrop)

;;;; Preview

(defconst imgdrop--preview-buffer " *imgdrop preview*"
  "Buffer the picker previews into.")

(defvar imgdrop--preview-timer nil
  "Idle timer debouncing preview updates, or nil.")

(defvar imgdrop--preview-shown nil
  "File the preview window is currently showing, or nil.")

(defun imgdrop--preview-candidate ()
  "Return the file the minibuffer is currently pointing at, or nil."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (let ((cand (or (and (fboundp 'vertico--candidate)
                           (ignore-errors (vertico--candidate)))
                      (minibuffer-contents-no-properties))))
        (when (and (stringp cand) (not (string-empty-p cand)))
          (ignore-errors
            (expand-file-name (substitute-in-file-name cand))))))))

(defun imgdrop--preview-show (file)
  "Show FILE in the preview side window."
  (let* ((buffer (get-buffer-create imgdrop--preview-buffer))
         (window (display-buffer
                  buffer
                  `(display-buffer-in-side-window
                    (side . right)
                    (slot . 0)
                    (window-width . ,imgdrop-preview-width)
                    (window-parameters . ((no-other-window . t)
                                          (mode-line-format . none)))))))
    (when (window-live-p window)
      (with-current-buffer buffer
        (setq-local cursor-type nil)
        (erase-buffer)
        (cond
         ((file-directory-p file) (insert "Directory"))
         ((not (file-regular-p file)) (insert "No preview"))
         (t (condition-case nil
                (insert-image
                 (create-image file nil nil
                               :max-width (window-body-width window t)
                               :max-height (window-body-height window t)))
              (error (insert "No preview")))))))))

(defun imgdrop--preview-update ()
  "Bring the preview into line with the highlighted candidate."
  (with-demoted-errors "imgdrop: %S"
    (when (active-minibuffer-window)
      (let ((file (imgdrop--preview-candidate)))
        (when (and file (not (equal file imgdrop--preview-shown)))
          (setq imgdrop--preview-shown file)
          (imgdrop--preview-show file))))))

(defun imgdrop--preview-schedule ()
  "Queue a preview update, replacing any update not yet run."
  (when (timerp imgdrop--preview-timer) (cancel-timer imgdrop--preview-timer))
  (setq imgdrop--preview-timer
        (run-with-idle-timer imgdrop-preview-delay nil #'imgdrop--preview-update)))

(defun imgdrop--preview-setup ()
  "Arm the preview for this minibuffer session."
  (setq imgdrop--preview-shown nil)
  (add-hook 'post-command-hook #'imgdrop--preview-schedule nil t)
  (add-hook 'minibuffer-exit-hook #'imgdrop--preview-teardown nil t))

(defun imgdrop--preview-teardown ()
  "Cancel the preview and take its window down."
  (when (timerp imgdrop--preview-timer) (cancel-timer imgdrop--preview-timer))
  (setq imgdrop--preview-timer nil
        imgdrop--preview-shown nil)
  (when-let* ((buffer (get-buffer imgdrop--preview-buffer)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (when (window-live-p window)
        (ignore-errors (delete-window window))))
    (kill-buffer buffer)))

;;;; Commands

(defun imgdrop--read-file-name ()
  "Read the name of an image file, offering directories to descend into."
  (read-file-name "Insert image: " (imgdrop--document-directory) nil t nil
                  (lambda (name)
                    (or (file-directory-p name) (imgdrop-image-file-p name)))))

(defun imgdrop--read-file ()
  "Read an image file name, previewing the highlighted candidate."
  (let ((file (if imgdrop-preview
                  (unwind-protect
                      (minibuffer-with-setup-hook #'imgdrop--preview-setup
                        (imgdrop--read-file-name))
                    (imgdrop--preview-teardown))
                (imgdrop--read-file-name))))
    (unless (and file (file-regular-p file))
      (user-error "Not a file: %s" (or file "")))
    file))

;;;###autoload
(defun imgdrop-insert-image ()
  "Pick an image in the minibuffer and insert markup for it at point."
  (interactive)
  (imgdrop--place (imgdrop--read-file)))

;;;###autoload
(defun imgdrop-insert-image-gui ()
  "Pick an image with the system file panel and insert markup for it at point."
  (interactive)
  (if (and (display-graphic-p) (fboundp 'x-file-dialog))
      (let ((file (x-file-dialog "Insert image: "
                                 (imgdrop--document-directory) nil t)))
        (unless (and file (file-regular-p file))
          (user-error "Not a file: %s" (or file "")))
        (imgdrop--place file))
    (imgdrop-insert-image)))

(provide 'imgdrop)
;;; imgdrop.el ends here
