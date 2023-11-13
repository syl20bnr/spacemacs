;;; pdf-virtual.el --- Virtual PDF documents         -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Politz

;; Author: Andreas Politz <politza@hochschule-trier.de>
;; Keywords: multimedia, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A virtual PDF is a collection of pages, or parts thereof, of
;; arbitrary documents in one particular order.  This library acts as
;; an intermediate between pdf-info.el and all other packages, in
;; order to transparently make this collection appear as one single
;; document.
;;
;; The trickiest part is to make these intermediate functions behave
;; like the pdf-info-* equivalents in both the synchronous and
;; asynchronous case.

;;; Code:
(require 'let-alist)
(require 'pdf-info)
(require 'pdf-util)

(declare-function pdf-view-mode "pdf-view.el")

;; * ================================================================== *
;; * Variables
;; * ================================================================== *

(defconst pdf-virtual-magic-mode-regexp "^ *;+ *%VPDF\\_>"
  "A regexp matching the first line in a vpdf file.")

(defvar-local pdf-virtual-document nil
  "A list representing the virtual document.")

(put 'pdf-virtual-document 'permanent-local t)

(defvar pdf-virtual-adapter-alist nil
  "Alist of server functions.

Each element looks like \(PDF-VIRTUAL-FN . PDF-INFO-FN\).  This
list is filled by the macro `pdf-virtual-define-adapter' and used
to enable/disable the corresponding advices.")


;; * ================================================================== *
;; * VPDF datastructure
;; * ================================================================== *

(defun pdf-virtual-pagespec-normalize (page-spec &optional filename)
  "Normalize PAGE-SPEC using FILENAME.

PAGE-SPEC should be as described in
`pdf-virtual-document-create'.  FILENAME is used to determine the
last page number, if needed.  The `current-buffer', if it is nil.

Returns a list \(\(FIRST . LAST\) . REGION\)\)."

  (let ((page-spec (cond
                    ((natnump page-spec)
                     (list (cons page-spec page-spec)))
                    ((null (car page-spec))
                     (let ((npages (pdf-info-number-of-pages filename)))
                       (cons (cons 1 npages)
                             (cdr page-spec))))
                    ((natnump (car page-spec))
                     (cond
                      ((natnump (cdr page-spec))
                       (list page-spec))
                      (t
                       (cons (cons (car page-spec)
                                   (car page-spec))
                             (cdr page-spec)))))
                    (t page-spec))))
    (when (equal (cdr page-spec)
                 '(0 0 1 1))
      (setq page-spec `((,(caar page-spec) . ,(cdar page-spec)))))
    page-spec))

(cl-defstruct pdf-virtual-range
  ;; The PDF's filename.
  filename
  ;; First page in this range.
  first
  ;; Last page.
  last
  ;; The edges selected for these pages.
  region
  ;; The page-index corresponding to the first page in this range.
  index-start)

(cl-defstruct pdf-virtual-document
  ;; Array of shared pdf-virtual-range structs, one element for each
  ;; page.
  page-array
  ;; An alist mapping filenames to a list of pages.
  file-map)

(defun pdf-virtual-range-length (page)
  "Return the number of pages in PAGE."
  (1+ (- (pdf-virtual-range-last page)
         (pdf-virtual-range-first page))))

(defun pdf-virtual-document-create (list &optional directory
                                         file-error-handler)
  "Create a virtual PDF from LIST using DIRECTORY.

LIST should be a list of elements \(FILENAME . PAGE-SPECS\),
where FILENAME is a PDF document and PAGE-SPECS is a list of
PAGE-RANGE and/or \(PAGE-RANGE . EDGES\).  In the later case,
EDGES should be a list of relative coordinates \(LEFT TOP RIGHT
BOT\) selecting a region of the page(s) in PAGE-RANGE.  Giving no
PAGE-SPECs at all is equivalent to all pages of FILENAME.

See `pdf-info-normalize-page-range' for the valid formats of
PAGE-RANGE.
"

  (unless (cl-every 'consp list)
    (error "Every element should be a cons: %s" list))
  (unless (cl-every 'stringp (mapcar 'car list))
    (error "The car of every element should be a filename."))
  (unless (cl-every (lambda (elt)
                      (cl-every (lambda (page)
                                  (or (pdf-info-valid-page-spec-p page)
                                      (and (consp page)
                                           (pdf-info-valid-page-spec-p (car page))
                                           (pdf-util-edges-p (cdr page) 'relative))))
                                elt))
                    (mapcar 'cdr list))
    (error
     "The cdr of every element should be a list of page-specs"))
  (let* ((doc (pdf-virtual-document--normalize
               list (or directory default-directory)
               file-error-handler))
         (npages 0)
         document file-map)
    (while doc
      (let* ((elt (pop doc))
             (filename (car elt))
             (mapelt (assoc filename file-map))
             (page-specs (cdr elt)))
        (if mapelt
            (setcdr mapelt (cons (1+ npages) (cdr mapelt)))
          (push (list filename (1+ npages)) file-map))
        (while page-specs
          (let* ((ps (pop page-specs))
                 (first (caar ps))
                 (last (cdar ps))
                 (region (cdr ps))
                 (clx (make-pdf-virtual-range
                       :filename filename
                       :first first
                       :last last
                       :region region
                       :index-start npages)))
            (cl-incf npages (1+ (- last first)))
            (push (make-vector (1+ (- last first)) clx)
                  document)))))
    (make-pdf-virtual-document
     :page-array (apply 'vconcat (nreverse document))
     :file-map (nreverse
                (mapcar (lambda (f)
                          (setcdr f (nreverse (cdr f)))
                          f)
                        file-map)))))

(defun pdf-virtual-document--normalize (list &optional directory
                                            file-error-handler)
  (unless file-error-handler
    (setq file-error-handler
          (lambda (filename err)
            (signal (car err)
                    (append (cdr err) (list filename))))))
  (let ((default-directory
          (or directory default-directory)))
    (setq list (cl-remove-if-not
                (lambda (filename)
                  (condition-case err
                      (progn
                        (unless (file-readable-p filename)
                          (signal 'file-error
                                  (list "File not readable: " filename)))
                        (pdf-info-open filename)
                        t)
                    (error
                     (funcall file-error-handler filename err)
                     nil)))
                list
                :key 'car))
    (let* ((file-attributes (make-hash-table :test 'equal))
           (file-equal-p (lambda (f1 f2)
                           (let ((a1 (gethash f1 file-attributes))
                                 (a2 (gethash f2 file-attributes)))
                             (if (and a1 a2)
                                 (equal a1 a2)
                               (file-equal-p f1 f2)))))
           files normalized)
      ;; Optimize file-equal-p by caching file-attributes, which is slow
      ;; and would be called quadratic times otherwise.  (We don't want
      ;; the same file under different names.)
      (dolist (f (mapcar 'car list))
        (unless (find-file-name-handler f 'file-equal-p)
          (puthash f (file-attributes f) file-attributes)))
      (dolist (elt list)
        (let ((file (cl-find (car elt) files :test file-equal-p)))
          (unless file
            (push (car elt) files)
            (setq file (car elt)))
          (let ((pages (mapcar (lambda (p)
                                 (pdf-virtual-pagespec-normalize p file))
                               (or (cdr elt) '(nil))))
                newpages)
            (while pages
              (let* ((spec (pop pages))
                     (first (caar spec))
                     (last (cdar spec))
                     (region (cdr spec)))
                (while (and pages
                            (eq (1+ last)
                                (caar (car pages)))
                            (equal region (cdr (car pages))))
                  (setq last (cdar (pop pages))))
                (push `((,first . ,last) . ,region) newpages)))
            (push (cons file (nreverse newpages))
                  normalized))))
      (nreverse normalized))))

(defmacro pdf-virtual-document-defun (name args &optional documentation &rest body)
  "Define a PDF Document function.

Args are just like for `defun'.  This macro will ensure, that the
DOCUMENT argument, which should be last, is setup properly in
case it is nil, i.e. check that the buffer passes
`pdf-virtual-buffer-assert-p' and use the variable
`pdf-virtual-document'."

  (declare (doc-string 3) (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           def-body)))
  (unless (stringp documentation)
    (push documentation body)
    (setq documentation nil))
  (unless (memq '&optional args)
    (setq args (append (butlast args)
                       (list '&optional)
                       (last args))))
  (when (memq '&rest args)
    (error "&rest argument not supported"))
  (let ((doc-arg (car (last args)))
        (fn (intern (format "pdf-virtual-document-%s" name))))
    `(progn
       (put ',fn 'definition-name ',name)
       (defun ,fn
           ,args ,documentation
           (setq ,doc-arg
                 (or ,doc-arg
                     (progn (pdf-virtual-buffer-assert-p)
                            pdf-virtual-document)))
           (cl-check-type ,doc-arg pdf-virtual-document)
           ,@body))))

(pdf-virtual-document-defun filenames (doc)
  "Return the list of filenames in DOC."
  (mapcar 'car (pdf-virtual-document-file-map doc)))

(pdf-virtual-document-defun normalize-pages (pages doc)
  "Normalize PAGES using DOC.

Like `pdf-info-normalize-page-range', except 0 is replaced by
DOC's last page."

  (setq pages (pdf-info-normalize-page-range pages))
  (if (eq 0 (cdr pages))
      `(,(car pages) . ,(pdf-virtual-document-number-of-pages doc))
    pages))

(pdf-virtual-document-defun page (page doc)
  "Get PAGE of DOC.

Returns a list \(FILENAME FILE-PAGE REGION\)."
  (let ((page (car (pdf-virtual-document-pages (cons page page) doc))))
    (when page
      (cl-destructuring-bind (filename first-last region)
          page
        (list filename (car first-last) region)))))

(pdf-virtual-document-defun pages (pages doc)
  "Get PAGES of DOC.

PAGES should be a cons \(FIRST . LAST\).  Return a list of
ranges corresponding to PAGES. Each element has the form

     \(FILENAME \(FILE-FIRT-PAGE . FILE-LAST-PAGE\) REGION\)
.
"

  (let ((begin (car pages))
        (end (cdr pages)))
    (unless (<= begin end)
      (error "begin should not exceed end: %s" (cons begin end)))
    (let ((arr (pdf-virtual-document-page-array doc))
          result)
      (when (or (< begin 1)
                (> end (length arr)))
        (signal 'args-out-of-range (list 'pages pages)))
      (while (<= begin end)
        (let* ((page (aref arr (1- begin)))
               (filename (pdf-virtual-range-filename page))
               (offset (- (1- begin)
                          (pdf-virtual-range-index-start page)))
               (first (+ (pdf-virtual-range-first page)
                         offset))
               (last (min (+ first (- end begin))
                          (pdf-virtual-range-last page)))
               (region (pdf-virtual-range-region page)))
          (push `(,filename (,first . ,last) ,region) result)
          (cl-incf begin (1+ (- last first)))))
      (nreverse result))))

(pdf-virtual-document-defun number-of-pages (doc)
  "Return the number of pages in DOC."
  (length (pdf-virtual-document-page-array doc)))

(pdf-virtual-document-defun page-of (filename &optional file-page limit doc)
  "Return a page number displaying FILENAME's page FILE-PAGE in DOC.

If FILE-PAGE is nil, return the first page displaying FILENAME.
If LIMIT is non-nil, it should be a range \(FIRST . LAST\) in
which the returned page should fall. This is useful if there are
more than one page displaying FILE-PAGE. LIMIT is ignored, if
FILE-PAGE is nil.

Return nil if there is no matching page."

  (if (null file-page)
      (cadr (assoc filename (pdf-virtual-document-file-map doc)))
    (let ((pages (pdf-virtual-document-page-array doc)))
      (catch 'found
        (mapc
         (lambda (pn)
           (while (and (<= pn (length pages))
                       (equal (pdf-virtual-range-filename (aref pages (1- pn)))
                              filename))
             (let* ((page (aref pages (1- pn)))
                    (first (pdf-virtual-range-first page))
                    (last (pdf-virtual-range-last page)))
               (when (and (>= file-page first)
                          (<= file-page last))
                 (let ((r (+ (pdf-virtual-range-index-start page)
                             (- file-page (pdf-virtual-range-first page))
                             1)))
                   (when (or (null limit)
                             (and (>= r (car limit))
                                  (<= r (cdr limit))))
                     (throw 'found r))))
               (cl-incf pn (1+ (- last first))))))
         (cdr (assoc filename (pdf-virtual-document-file-map doc))))
        nil))))

(pdf-virtual-document-defun find-matching-page (page predicate
                                                     &optional
                                                     backward-p doc)
  (unless (and (>= page 1)
               (<= page (length (pdf-virtual-document-page-array doc))))
    (signal 'args-out-of-range (list 'page page)))
  (let* ((pages (pdf-virtual-document-page-array doc))
         (i (1- page))
         (this (aref pages i))
         other)
    (while (and (< i (length pages))
                (>= i 0)
                (null other))
      (setq i
            (if backward-p
                (1- (pdf-virtual-range-index-start this))
              (+ (pdf-virtual-range-length this)
                 (pdf-virtual-range-index-start this))))
      (when (and (< i (length pages))
                 (>= i 0))
        (setq other (aref pages i))
        (unless (funcall predicate this other)
          (setq other nil))))
    other))

(pdf-virtual-document-defun next-matching-page (page predicate doc)
  (pdf-virtual-document-find-matching-page page predicate nil doc))

(pdf-virtual-document-defun previous-matching-page (page predicate doc)
  (declare (indent 1))
  (pdf-virtual-document-find-matching-page page predicate t doc))

(pdf-virtual-document-defun next-file (page doc)
  "Return the next page displaying a different file than PAGE.

PAGE should be a page-number."
  (let ((page (pdf-virtual-document-next-matching-page
               page
               (lambda (this other)
                 (not (equal (pdf-virtual-range-filename this)
                             (pdf-virtual-range-filename other)))))))
    (when page
      (1+ (pdf-virtual-range-index-start page)))))

(pdf-virtual-document-defun previous-file (page doc)
  "Return the previous page displaying a different file than PAGE.

PAGE should be a page-number."
  (let ((page (pdf-virtual-document-previous-matching-page
               page
               (lambda (this other)
                 (not (equal (pdf-virtual-range-filename this)
                             (pdf-virtual-range-filename other)))))))
    (when page
      (1+ (pdf-virtual-range-index-start page)))))


;; * ================================================================== *
;; * Modes
;; * ================================================================== *

(defvar pdf-virtual-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emacs-lisp-mode-map)
    (define-key map (kbd "C-c C-c") 'pdf-virtual-view-mode)
    map))


;;;###autoload
(define-derived-mode pdf-virtual-edit-mode emacs-lisp-mode "VPDF-Edit"
  "Major mode when editing a virtual PDF buffer."
  (buffer-enable-undo)
  (setq-local buffer-read-only nil)
  (unless noninteractive
    (message (substitute-command-keys "Press \\[pdf-virtual-view-mode] to view."))))

;; FIXME: Provide filename/region from-windows-gathering functions.
(defvar pdf-virtual-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pdf-view-mode-map)
    (define-key map (kbd "C-c C-c") 'pdf-virtual-edit-mode)
    (define-key map [remap backward-paragraph] 'pdf-virtual-buffer-backward-file)
    (define-key map [remap forward-paragraph] 'pdf-virtual-buffer-forward-file)
    (define-key map (kbd "C-c C-c") 'pdf-virtual-edit-mode)
    map))

;;;###autoload
(define-derived-mode pdf-virtual-view-mode pdf-view-mode "VPDF-View"
  "Major mode in virtual PDF buffers."
  (setq-local write-contents-functions nil)
  (remove-hook 'kill-buffer-hook 'pdf-view-close-document t)
  (setq-local header-line-format
              `(:eval (pdf-virtual-buffer-current-file)))
  (unless noninteractive
    (message (substitute-command-keys "Press \\[pdf-virtual-edit-mode] to edit."))))

;;;###autoload
(define-minor-mode pdf-virtual-global-minor-mode
  "Enable recognition and handling of VPDF files."
  :global t
  :group 'pdf-tools
  (let ((elt `(,pdf-virtual-magic-mode-regexp . pdf-virtual-view-mode)))
    (cond
     (pdf-virtual-global-minor-mode
      (add-to-list 'magic-mode-alist elt))
     (t
      (setq magic-mode-alist
            (remove elt magic-mode-alist))))
    (dolist (elt pdf-virtual-adapter-alist)
      (let ((fn (car elt))
            (orig (cdr elt)))
        (advice-remove orig fn)
        (when pdf-virtual-global-minor-mode
          (advice-add orig :around fn))))))

(advice-add 'pdf-virtual-view-mode
            :around 'pdf-virtual-view-mode-prepare)

;; This needs to run before pdf-view-mode does its thing.
(defun pdf-virtual-view-mode-prepare (fn)
  (let (list unreadable)
    (save-excursion
      (goto-char 1)
      (unless (looking-at pdf-virtual-magic-mode-regexp)
        (pdf-virtual-buffer-assert-p))
      (setq list (read (current-buffer))))
    (setq pdf-virtual-document
          (pdf-virtual-document-create
           list
           nil
           (lambda (filename _error)
             (push filename unreadable))))
    (when unreadable
      (display-warning
       'pdf-virtual
       (format "Some documents could not be opened:\n%s"
               (mapconcat (lambda (f)
                            (concat " " f))
                          unreadable "\n"))))
    (if (= (pdf-virtual-document-number-of-pages) 0)
        (error "Document is empty.")
      (unless pdf-virtual-global-minor-mode
        (pdf-virtual-global-minor-mode 1))
      (funcall fn))))


;; * ================================================================== *
;; * Buffer handling
;; * ================================================================== *

;;;###autoload
(defun pdf-virtual-buffer-create (&optional filenames buffer-name display-p)
  (interactive
   (list (directory-files default-directory nil "\\.pdf\\'")
         (read-string
          "Buffer name (default: all.vpdf): " nil nil "all.vpdf") t))
  (with-current-buffer (generate-new-buffer buffer-name)
    (insert ";; %VPDF 1.0\n\n")
    (insert ";; File Format
;;
;; FORMAT    ::= ( FILES* )
;; FILES     ::= ( FILE . PAGE-SPEC* )
;; PAGE-SPEC ::= PAGE | ( PAGE . REGION )
;; PAGE      ::= NUMBER | ( FIRST . LAST )
;; REGION    ::= ( LEFT TOP RIGHT BOT )
;;
;; 0 <= X <= 1, forall X in REGION .

")
    (if (null filenames)
        (insert "nil\n")
      (insert "(")
      (dolist (f filenames)
        (insert (format "(%S)\n " f)))
      (delete-char -2)
      (insert ")\n"))
    (pdf-virtual-edit-mode)
    (when display-p
      (pop-to-buffer (current-buffer)))
    (current-buffer)))

(defun pdf-virtual-buffer-p (&optional buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (or (derived-mode-p 'pdf-virtual-view-mode 'pdf-virtual-edit-mode)
        pdf-virtual-document)))

(defun pdf-virtual-view-window-p (&optional window)
  (save-selected-window
    (when window (select-window window 'norecord))
    (derived-mode-p 'pdf-virtual-view-mode)))

(defun pdf-virtual-filename-p (filename)
  (and (stringp filename)
       (file-exists-p filename)
       (with-temp-buffer
         (save-excursion (insert-file-contents filename nil 0 128))
         (looking-at pdf-virtual-magic-mode-regexp))))

(defun pdf-virtual-buffer-assert-p (&optional buffer)
  (unless (pdf-virtual-buffer-p buffer)
    (error "Buffer is not a virtual PDF buffer")))

(defun pdf-virtual-view-window-assert-p (&optional window)
  (unless (pdf-virtual-view-window-p window)
    (error "Window's buffer is not in `pdf-virtual-view-mode'.")))

(defun pdf-virtual-buffer-current-file (&optional window)
  (pdf-virtual-view-window-assert-p window)
  (pdf-virtual-range-filename
   (aref (pdf-virtual-document-page-array
          pdf-virtual-document)
         (1- (pdf-view-current-page window)))))

(defun pdf-virtual-buffer-forward-file (&optional n interactive-p)
  (interactive "p\np")
  (pdf-virtual-view-window-assert-p)
  (let* ((pn (pdf-view-current-page))
         (pages (pdf-virtual-document-page-array
                 pdf-virtual-document))
         (page (aref pages (1- pn)))
         (first-filepage (1+ (pdf-virtual-range-index-start page))))

    (when (and (< n 0)
               (not (= first-filepage pn)))
      (cl-incf n))
    (setq pn first-filepage)

    (let (next)
      (while (and (> n 0)
                  (setq next (pdf-virtual-document-next-file pn)))
        (setq pn next)
        (cl-decf n)))
    (let (previous)
      (while (and (< n 0)
                  (setq previous (pdf-virtual-document-previous-file pn)))
        (setq pn previous)
        (cl-incf n)))
    (when interactive-p
      (when (< n 0)
        (message "First file."))
      (when (> n 0)
        (message "Last file.")))
    (pdf-view-goto-page pn)
    n))

(defun pdf-virtual-buffer-backward-file (&optional n interactive-p)
  (interactive "p\np")
  (pdf-virtual-buffer-forward-file (- (or n 1)) interactive-p))


;; * ================================================================== *
;; * Helper functions
;; * ================================================================== *


(defmacro pdf-virtual-dopages (bindings pages &rest body)
  (declare (indent 2) (debug (sexp form &rest form)))
  (let ((page (make-symbol "page")))
    `(dolist (,page ,pages)
       (cl-destructuring-bind ,bindings
           ,page
         ,@body))))

(defun pdf-virtual--perform-search (string pages &optional regexp-p no-error)
  (let* ((pages (pdf-virtual-document-normalize-pages pages))
         (file-pages (pdf-virtual-document-pages pages)))
    (pdf-info-compose-queries
        ((responses
          (pdf-virtual-dopages (filename pages _region)
              file-pages
            (if regexp-p
                (pdf-info-search-string string pages filename)
              ;; FIXME: no-error won't work with synchronous calls.
              (pdf-info-search-regexp string pages no-error filename)))))
      (let (result)
        (pdf-virtual-dopages (filename _ region)
          file-pages
          (let ((matches (pop responses)))
            (when region
              (setq matches
                    (mapcar
                     (lambda (m)
                       (let-alist m
                         `((edges . ,(pdf-util-edges-transform region .edges t))
                           ,@m)))
                     (pdf-virtual--filter-edges
                      region matches
                      (apply-partially 'alist-get 'edges)))))
            (dolist (m matches)
              (push `((page . ,(pdf-virtual-document-page-of
                                filename (alist-get 'page m)
                                pages))
                      ,@m)
                    result))))
        (nreverse result)))))

(defun pdf-virtual--filter-edges (region elts &optional edges-key-fn)
  (if (null region)
      elts
    (cl-remove-if-not
     (lambda (edges)
       (or (null edges)
           (if (consp (car edges))
               (cl-some (apply-partially 'pdf-util-edges-intersection region) edges)
             (pdf-util-edges-intersection region edges))))
     elts
     :key edges-key-fn)))

(defun pdf-virtual--transform-goto-dest (link filename region)
  (let-alist link
    (let ((local-page (pdf-virtual-document-page-of
                       filename .page)))
      (if local-page
          `((type . ,'goto-dest)
            (title . , .title)
            (page . ,local-page)
            (top . ,(car (pdf-util-edges-transform
                          region (cons .top .top) t))))
        `((type . ,'goto-remote)
          (title . , .title)
          (filename . ,filename)
          (page . , .page)
          (top . , .top))))))


;; * ================================================================== *
;; * Server adapter
;; * ================================================================== *

(defmacro pdf-virtual-define-adapter (name arglist &optional doc &rest body)
  ;; FIXME: Handle &optional + &rest argument.
  (declare (doc-string 3) (indent 2)
           (debug (&define name lambda-list
                           [&optional stringp]
                           def-body)))
  (unless (stringp doc)
    (push doc body)
    (setq doc nil))
  (let ((fn (intern (format "pdf-virtual-%s" name)))
        (base-fn (intern (format "pdf-info-%s" name)))
        (base-fn-arg (make-symbol "fn"))
        (true-file-or-buffer (make-symbol "true-file-or-buffer"))
        (args (cl-remove-if (lambda (elt)
                              (memq elt '(&optional &rest)))
                            arglist)))
    (unless (fboundp base-fn)
      (error "Base function is undefined: %s" base-fn))
    (unless (memq 'file-or-buffer arglist)
      (error "Argument list is missing a `file-or-buffer' argument: %s" arglist))
    `(progn
       (put ',fn 'definition-name ',name)
       (add-to-list 'pdf-virtual-adapter-alist ',(cons fn base-fn))
       (defun ,fn ,(cons base-fn-arg arglist)
         ,(format "%sPDF virtual adapter to `%s'.

This function delegates to `%s',
unless the FILE-OR-BUFFER argument denotes a VPDF document."
                  (if doc (concat doc "\n\n") "")
                  base-fn
                  base-fn)
         (let ((,true-file-or-buffer
                (cond
                 ((or (bufferp file-or-buffer)
                      (stringp file-or-buffer)) file-or-buffer)
                 ((or (null file-or-buffer)
                      ,(not (null (memq '&rest arglist))))
                  (current-buffer)))))
           (if (cond
                ((null ,true-file-or-buffer) t)
                ((bufferp ,true-file-or-buffer)
                 (not (pdf-virtual-buffer-p ,true-file-or-buffer)))
                ((stringp ,true-file-or-buffer)
                 (not (pdf-virtual-filename-p ,true-file-or-buffer))))
               (,(if (memq '&rest arglist) 'apply 'funcall) ,base-fn-arg ,@args)
             (when (stringp ,true-file-or-buffer)
               (setq ,true-file-or-buffer
                     (find-file-noselect ,true-file-or-buffer)))
             (save-current-buffer
               (when (bufferp ,true-file-or-buffer)
                 (set-buffer ,true-file-or-buffer))
               ,@body)))))))

(define-error 'pdf-virtual-unsupported-operation
  "Operation not supported in VPDF buffer")

(pdf-virtual-define-adapter open (&optional file-or-buffer password)
  (mapc (lambda (file)
          (pdf-info-open file password))
        (pdf-virtual-document-filenames)))

(pdf-virtual-define-adapter close (&optional file-or-buffer)
  (let ((files (cl-remove-if 'find-buffer-visiting
                             (pdf-virtual-document-filenames))))
    (pdf-info-compose-queries
        ((results (mapc 'pdf-info-close files)))
      (cl-some 'identity results))))

(pdf-virtual-define-adapter metadata (&optional file-or-buffer)
  (pdf-info-compose-queries
      ((md (mapc 'pdf-info-metadata (pdf-virtual-document-filenames))))
    (apply 'cl-mapcar (lambda (&rest elts)
                        (cons (caar elts)
                              (cl-mapcar 'cdr elts)))
           md)))

(pdf-virtual-define-adapter search-string (string &optional pages file-or-buffer)
  (pdf-virtual--perform-search
   string (pdf-virtual-document-normalize-pages pages)))

(pdf-virtual-define-adapter search-regexp (pcre &optional
                                                pages no-error file-or-buffer)
  (pdf-virtual--perform-search
   pcre (pdf-virtual-document-normalize-pages pages) 'regexp no-error))

(pdf-virtual-define-adapter pagelinks (page &optional file-or-buffer)
  (cl-destructuring-bind (filename ext-page region)
      (pdf-virtual-document-page page)
    (pdf-info-compose-queries
        ((links (pdf-info-pagelinks ext-page filename)))
      (mapcar
       (lambda (link)
         (let-alist link
           (if (not (eq .type 'goto-dest))
               link
             `((edges .  ,(pdf-util-edges-transform region .edges t))
               ,@(pdf-virtual--transform-goto-dest link filename region)))))
       (pdf-virtual--filter-edges region (car links) 'car)))))

(pdf-virtual-define-adapter number-of-pages (&optional file-or-buffer)
  (pdf-info-compose-queries nil (pdf-virtual-document-number-of-pages)))

(pdf-virtual-define-adapter outline (&optional file-or-buffer)
  (let ((files (pdf-virtual-document-filenames)))
    (pdf-info-compose-queries
        ((outlines (mapc 'pdf-info-outline files)))
      (cl-mapcan
       (lambda (outline filename)
         `(((depth . 1)
            (type . goto-dest)
            (title . ,filename)
            (page . ,(pdf-virtual-document-page-of filename))
            (top . 0))
           ,@(delq
              nil
              (mapcar
               (lambda (item)
                 (let-alist item
                   (if (not (eq .type 'goto-dest))
                       `((depth . ,(1+ .depth))
                         ,@item)
                     (cl-check-type filename string)
                     (let ((page (pdf-virtual-document-page-of
                                  filename .page)))
                       (when page
                         `((depth . ,(1+ .depth))
                           ,@(pdf-virtual--transform-goto-dest
                              item filename
                              (nth 2 (pdf-virtual-document-page page)))))))))
               outline))))
       outlines files))))

(pdf-virtual-define-adapter gettext (page edges &optional
                                          selection-style file-or-buffer)
  (cl-destructuring-bind (filename file-page region)
      (pdf-virtual-document-page page)
    (let ((edges (pdf-util-edges-transform region edges)))
      (pdf-info-gettext file-page edges selection-style filename))))

(pdf-virtual-define-adapter getselection (page edges &optional
                                               selection-style file-or-buffer)
  (cl-destructuring-bind (filename file-page region)
      (pdf-virtual-document-page page)
    (let ((edges (pdf-util-edges-transform region edges)))
      (pdf-info-compose-queries
          ((results (pdf-info-getselection file-page edges selection-style filename)))
        (pdf-util-edges-transform
         region
         (pdf-virtual--filter-edges region (car results)) t)))))

(pdf-virtual-define-adapter charlayout (page &optional edges-or-pos file-or-buffer)
  (cl-destructuring-bind (filename file-page region)
      (pdf-virtual-document-page page)
    (let ((edges-or-pos (pdf-util-edges-transform region edges-or-pos)))
      (pdf-info-compose-queries
          ((results (pdf-info-charlayout file-page edges-or-pos filename)))
        (mapcar (lambda (elt)
                  `(,(car elt)
                    . ,(pdf-util-edges-transform region (cdr elt) t)))
                (pdf-virtual--filter-edges region (car results) 'cadr))))))

(pdf-virtual-define-adapter pagesize (page &optional file-or-buffer)
  (cl-destructuring-bind (filename file-page region)
      (pdf-virtual-document-page page)
    (pdf-info-compose-queries
        ((result (pdf-info-pagesize file-page filename)))
      (if (null region)
          (car result)
        (pdf-util-with-edges (region)
          (pdf-util-scale
           (car result) (cons region-width region-height)))))))

(pdf-virtual-define-adapter getannots (&optional pages file-or-buffer)
  (let* ((pages (pdf-virtual-document-normalize-pages pages))
         (file-pages (pdf-virtual-document-pages pages)))
    (pdf-info-compose-queries
        ((annotations
          (pdf-virtual-dopages (filename file-pages _region)
              file-pages
            (pdf-info-getannots file-pages filename))))
      (let ((page (car pages))
            result)
        (pdf-virtual-dopages (_filename file-pages region)
            file-pages
          (dolist (a (pop annotations))
            (let ((edges (delq nil `(,(cdr (assq 'edges a))
                                     ,@(cdr (assq 'markup-edges a))))))
              (when (pdf-virtual--filter-edges region edges)
                (let-alist a
                  (setcdr (assq 'page a)
                          (+ page (- .page (car file-pages))))
                  (setcdr (assq 'id a)
                          (intern (format "%s/%d" .id (cdr (assq 'page a)))))
                  (when region
                    (when .edges
                      (setcdr (assq 'edges a)
                              (pdf-util-edges-transform region .edges t)))
                    (when .markup-edges
                      (setcdr (assq 'markup-edges a)
                              (pdf-util-edges-transform region .markup-edges t))))
                  (push a result)))))
          (cl-incf page (1+ (- (cdr file-pages) (car file-pages)))))
        (nreverse result)))))

(pdf-virtual-define-adapter getannot (id &optional file-or-buffer)
  (let ((name (symbol-name id))
        page)
    (save-match-data
      (when (string-match "\\(.*\\)/\\([0-9]+\\)\\'" name)
        (setq id  (intern (match-string 1 name))
              page (string-to-number (match-string 2 name)))))
    (if page
        (cl-destructuring-bind (filename _ _)
            (pdf-virtual-document-page page)
          (pdf-info-compose-queries
              ((result (pdf-info-getannot id filename)))
            (let ((a (car result)))
              (cl-destructuring-bind (_ _ region)
                  (pdf-virtual-document-page page)
                (setcdr (assq 'page a) page)
                (let-alist a
                  (setcdr (assq 'id a)
                          (intern (format "%s/%d" .id (cdr (assq 'page a)))))
                  (when region
                    (when .edges
                      (setcdr (assq 'edges a)
                              (pdf-util-edges-transform region .edges t)))
                    (when .markup-edges
                      (setcdr (assq 'markup-edges a)
                              (pdf-util-edges-transform region .markup-edges t))))))
              a)))
      (pdf-info-compose-queries nil
        (error "No such annotation: %s" id)))))

(pdf-virtual-define-adapter addannot (page edges type &optional
                                           file-or-buffer &rest markup-edges)
  (signal 'pdf-virtual-unsupported-operation (list 'addannot)))

(pdf-virtual-define-adapter delannot (id &optional file-or-buffer)
  (signal 'pdf-virtual-unsupported-operation (list 'delannot)))

(pdf-virtual-define-adapter mvannot (id edges &optional file-or-buffer)
  (signal 'pdf-virtual-unsupported-operation (list 'mvannot)))

(pdf-virtual-define-adapter editannot (id modifications &optional file-or-buffer)
  (signal 'pdf-virtual-unsupported-operation (list 'editannot)))

(pdf-virtual-define-adapter save (&optional file-or-buffer)
  (signal 'pdf-virtual-unsupported-operation (list 'save)))

;;(defvar-local pdf-virtual-annotation-mapping nil)

(pdf-virtual-define-adapter getattachment-from-annot
    (id &optional do-save file-or-buffer)
  (let ((name (symbol-name id))
        page)
    (save-match-data
      (when (string-match "\\(.*\\)/\\([0-9]+\\)\\'" name)
        (setq id  (intern (match-string 1 name))
              page (string-to-number (match-string 2 name)))))
    (if page
        (cl-destructuring-bind (filename _ _)
            (pdf-virtual-document-page page)
          (pdf-info-getattachment-from-annot id do-save filename))
      (pdf-info-compose-queries nil
        (error "No such annotation: %s" id)))))

(pdf-virtual-define-adapter getattachments (&optional do-save file-or-buffer)
  (pdf-info-compose-queries
      ((results (mapc
                 (lambda (f)
                   (pdf-info-getattachments do-save f))
                 (pdf-virtual-document-filenames))))
    (apply 'append results)))

(pdf-virtual-define-adapter synctex-forward-search
    (source &optional line column file-or-buffer)
  (signal 'pdf-virtual-unsupported-operation (list 'synctex-forward-search)))

(pdf-virtual-define-adapter synctex-backward-search (page &optional x y file-or-buffer)
  (cl-destructuring-bind (filename file-page region)
      (pdf-virtual-document-page page)
    (cl-destructuring-bind (x &rest y)
        (pdf-util-edges-transform region (cons x y))
      (pdf-info-synctex-backward-search file-page x y filename))))

(pdf-virtual-define-adapter renderpage (page width &optional file-or-buffer
                                             &rest commands)
  (when (keywordp file-or-buffer)
    (push file-or-buffer commands)
    (setq file-or-buffer nil))
  (cl-destructuring-bind (filename file-page region)
      (pdf-virtual-document-page page)
    (when region
      (setq commands (append (list :crop-to region) commands)
            width (pdf-util-with-edges (region)
                    (round (* width (max 1 (/ 1.0 (max 1e-6 region-width))))))))
    (apply 'pdf-info-renderpage file-page width filename commands)))

(pdf-virtual-define-adapter boundingbox (page &optional file-or-buffer)
  (cl-destructuring-bind (filename file-page region)
      (pdf-virtual-document-page page)
    (pdf-info-compose-queries
        ((results (unless region (pdf-info-boundingbox file-page filename))))
      (if region
          (list 0 0 1 1)
        (car results)))))

(pdf-virtual-define-adapter pagelabels (&optional file-or-buffer)
  (signal 'pdf-virtual-unsupported-operation (list 'pagelabels)))

(pdf-virtual-define-adapter setoptions (&optional file-or-buffer &rest options)
  (when (keywordp file-or-buffer)
    (push file-or-buffer options)
    (setq file-or-buffer nil))
  (pdf-info-compose-queries
      ((_ (dolist (f (pdf-virtual-document-filenames))
            (apply 'pdf-info-setoptions f options))))
    nil))

(pdf-virtual-define-adapter getoptions (&optional file-or-buffer)
  (signal 'pdf-virtual-unsupported-operation (list 'getoptions)))

(pdf-virtual-define-adapter encrypted-p (&optional file-or-buffer)
  nil)

(provide 'pdf-virtual)
;;; pdf-virtual.el ends here
