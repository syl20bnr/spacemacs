;; -*- lexical-binding: t; -*-
(require 'sly)
(require 'sly-parse "lib/sly-parse")

(define-sly-contrib sly-package-fu
  "Exporting/Unexporting symbols at point."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slynk-dependencies slynk/package-fu)
  (:on-load 
   (define-key sly-mode-map "\C-cx"  'sly-export-symbol-at-point)
   (define-key sly-mode-map "\C-ci"  'sly-import-symbol-at-point))
  (:on-unload
   ;; FIXME: To properly support unloading, this contrib should be
   ;; made a minor mode with it's own keymap. The minor mode
   ;; activation function should be added to the proper sly-* hooks.
   ;; 
   ))

(defvar sly-package-file-candidates
  (mapcar #'file-name-nondirectory
	  '("package.lisp" "packages.lisp" "pkgdcl.lisp"
            "defpackage.lisp")))

(defvar sly-export-symbol-representation-function
  #'(lambda (n) (format "#:%s" n)))

(defvar sly-import-symbol-package-transform-function
  'identity
  "String transformation used by `sly-import-symbol-at-point'.

This function is applied to a package name before it is inserted
into the defpackage form. By default, it is `identity' but you
may wish redefine it to do some tranformations, for example, to
replace dots with slashes to conform to a package-inferred ASDF
system-definition style.")

(defvar sly-export-symbol-representation-auto t
  "Determine automatically which style is used for symbols, #: or :
If it's mixed or no symbols are exported so far,
use `sly-export-symbol-representation-function'.")

(define-obsolete-variable-alias 'sly-export-save-file
  'sly-package-fu-save-file "1.0.0-beta-3")

(defvar sly-package-fu-save-file nil
  "Save the package file after each automatic modification")

(defvar sly-defpackage-regexp
  "^(\\(cl:\\|common-lisp:\\|uiop:\\|\\uiop/package:\\)?\\(defpackage\\|define-package\\)\\>[ \t']*")

(put 'uiop:define-package 'sly-common-lisp-indent-function '(as defpackage))

(defun sly-find-package-definition-rpc (package)
  (sly-eval `(slynk:find-definition-for-thing
                (slynk::guess-package ,package))))

(defun sly-find-package-definition-regexp (package)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (cl-block nil
	(while (re-search-forward sly-defpackage-regexp nil t)
	  (when (sly-package-equal package (sly-sexp-at-point))
            (backward-sexp)
	    (cl-return (make-sly-file-location (buffer-file-name)
                                                 (1- (point))))))))))

(defun sly-package-equal (designator1 designator2)
  ;; First try to be lucky and compare the strings themselves (for the
  ;; case when one of the designated packages isn't loaded in the
  ;; image.) Then try to do it properly using the inferior Lisp which
  ;; will also resolve nicknames for us &c.
  (or (cl-equalp (sly-cl-symbol-name designator1)
                 (sly-cl-symbol-name designator2))
      (sly-eval `(slynk:package= ,designator1 ,designator2))))

(defun sly-export-symbol (symbol package)
  "Unexport `symbol' from `package' in the Lisp image."
  (sly-eval `(slynk:export-symbol-for-emacs ,symbol ,package)))

(defun sly-unexport-symbol (symbol package)
  "Export `symbol' from `package' in the Lisp image."
  (sly-eval `(slynk:unexport-symbol-for-emacs ,symbol ,package)))


(defun sly-find-possible-package-file (buffer-file-name)
  (cl-labels ((file-name-subdirectory (dirname)
                                      (expand-file-name
                                       (concat (file-name-as-directory (sly-to-lisp-filename dirname))
                                               (file-name-as-directory ".."))))
              (try (dirname)
                   (cl-dolist (package-file-name sly-package-file-candidates)
                     (let ((f (sly-to-lisp-filename
                               (concat dirname package-file-name))))
                       (when (file-readable-p f)
                         (cl-return f))))))
    (when buffer-file-name
      (let ((buffer-cwd (file-name-directory buffer-file-name)))
	(or (try buffer-cwd)
	    (try (file-name-subdirectory buffer-cwd))
	    (try (file-name-subdirectory
                  (file-name-subdirectory buffer-cwd))))))))

(defun sly-goto-package-source-definition (package)
  "Tries to find the DEFPACKAGE form of `package'. If found,
places the cursor at the start of the DEFPACKAGE form."
  (cl-labels ((try (location)
                   (when (sly-location-p location)
                     (sly-move-to-source-location location)
                     t)))
    (or (try (sly-find-package-definition-rpc package))
	(try (sly-find-package-definition-regexp package))
	(try (sly--when-let
                 (package-file (sly-find-possible-package-file
                                (buffer-file-name)))
	       (with-current-buffer (find-file-noselect package-file t)
		 (sly-find-package-definition-regexp package))))
	(sly-error "Couldn't find source definition of package: %s" package))))

(defun sly-at-expression-p (pattern)
  (when (ignore-errors
          ;; at a list?
          (= (point) (progn (down-list 1)
                            (backward-up-list 1)
                            (point))))
    (save-excursion
      (down-list 1)
      (sly-in-expression-p pattern))))

(defun sly-goto-next-export-clause ()
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (save-excursion
      (cl-block nil
	(while (ignore-errors (sly-forward-sexp) t)
          (skip-chars-forward " \n\t")
	  (when (sly-at-expression-p '(:export *))
	    (setq point (point))
	    (cl-return)))))
    (if point
	(goto-char point)
      (error "No next (:export ...) clause found"))))

(defun sly-search-exports-in-defpackage (symbol-name)
  "Look if `symbol-name' is mentioned in one of the :EXPORT clauses."
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (cl-labels ((target-symbol-p (symbol)
                               (string-match-p (format "^\\(\\(#:\\)\\|:\\)?%s$"
                                                       (regexp-quote symbol-name))
                                               symbol)))
    (save-excursion
      (cl-block nil
        (while (ignore-errors (sly-goto-next-export-clause) t)
          (let ((clause-end (save-excursion (forward-sexp) (point))))
            (save-excursion
              (while (search-forward symbol-name clause-end t)
                (when (target-symbol-p (sly-symbol-at-point))
                  (cl-return (if (sly-inside-string-p)
                                 ;; Include the following "
                                 (1+ (point))
                               (point))))))))))))


(defun sly-package-fu--read-symbols ()
  "Reads sexps as strings from the point to end of sexp.

For example, in this situation.

   (for<point> bar minor (again 123))

this will return (\"bar\" \"minor\" \"(again 123)\")"
  (cl-labels ((read-sexp ()
                         (ignore-errors
                           (forward-comment (point-max))
                           (buffer-substring-no-properties
                            (point) (progn (forward-sexp) (point))))))
    (save-excursion
      (cl-loop for sexp = (read-sexp) while sexp collect sexp))))

(defun sly-package-fu--normalize-name (name)
  (if (string-prefix-p "\"" name)
      (read name)
    (replace-regexp-in-string "^\\(\\(#:\\)\\|:\\)"
                              "" name)))

(defun sly-defpackage-exports ()
  "Return a list of symbols inside :export clause of a defpackage."
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (save-excursion
    (mapcar #'sly-package-fu--normalize-name
            (cl-loop while (ignore-errors (sly-goto-next-export-clause) t)
                     do (down-list) (forward-sexp)
                     append (sly-package-fu--read-symbols)
                     do (up-list) (backward-sexp)))))

(defun sly-symbol-exported-p (name symbols)
  (cl-member name symbols :test 'cl-equalp))

(defun sly-frob-defpackage-form (current-package do-what symbols)
  "Adds/removes `symbol' from the DEFPACKAGE form of `current-package'
depending on the value of `do-what' which can either be `:export',
or `:unexport'.

Returns t if the symbol was added/removed. Nil if the symbol was
already exported/unexported."
  (save-excursion
    (sly-goto-package-source-definition current-package)
    (down-list 1)			; enter DEFPACKAGE form
    (forward-sexp)			; skip DEFPACKAGE symbol
    ;; Don't or will fail if (:export ...) is immediately following
    ;; (forward-sexp)			; skip package name
    (let ((exported-symbols (sly-defpackage-exports))
          (symbols (if (consp symbols)
                       symbols
                     (list symbols)))
          (number-of-actions 0))
      (cl-ecase do-what
        (:export
         (sly-add-export)
         (dolist (symbol symbols)
           (let ((symbol-name (sly-cl-symbol-name symbol)))
             (unless (sly-symbol-exported-p symbol-name exported-symbols)
               (cl-incf number-of-actions)
               (sly-package-fu--insert-symbol symbol-name)))))
        (:unexport
         (dolist (symbol symbols)
           (let ((symbol-name (sly-cl-symbol-name symbol)))
             (when (sly-symbol-exported-p symbol-name exported-symbols)
               (sly-remove-export symbol-name)
               (cl-incf number-of-actions))))))
      (when sly-package-fu-save-file
        (save-buffer))
      (cons number-of-actions
            (current-buffer)))))

(defun sly-add-export ()
  (let (point)
    (save-excursion
      (while (ignore-errors (sly-goto-next-export-clause) t)
        (setq point (point))))
    (cond (point
           (goto-char point)
           (down-list)
           (sly-end-of-list))
          (t
           (sly-end-of-list)
           (unless (looking-back "^\\s-*" (line-beginning-position) nil)
             (newline-and-indent))
           (insert "(:export ")
           (save-excursion (insert ")"))))))

(defun sly-determine-symbol-style ()
  ;; Assumes we're inside :export
  (save-excursion
    (sly-beginning-of-list)
    (sly-forward-sexp)
    (let ((symbols (sly-package-fu--read-symbols)))
      (cond ((null symbols)
             sly-export-symbol-representation-function)
            ((cl-every (lambda (x)
                         (string-match "^:" x))
                       symbols)
             (lambda (n) (format ":%s" n)))
            ((cl-every (lambda (x)
                         (string-match "^#:" x))
                       symbols)
             (lambda (n) (format "#:%s" n)))
            ((cl-every (lambda (x)
                         (string-prefix-p "\"" x))
                       symbols)
             (lambda (n) (prin1-to-string (upcase (substring-no-properties n)))))
            (t
             sly-export-symbol-representation-function)))))

(defun sly-format-symbol-for-defpackage (symbol-name)
  (funcall (if sly-export-symbol-representation-auto
               (sly-determine-symbol-style)
             sly-export-symbol-representation-function)
           symbol-name))

(defun sly-package-fu--insert-symbol (symbol-name)
  ;; Assumes we're at the inside :export or :import-from form
  ;; after the last symbol
  (let ((symbol-name (sly-format-symbol-for-defpackage symbol-name)))
    (unless (looking-back "^\\s-*" (line-beginning-position) nil)
      (newline-and-indent))
    (insert symbol-name)
    (when (looking-at "\\s_") (insert " "))))

(defun sly-remove-export (symbol-name)
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (while (setq point (sly-search-exports-in-defpackage symbol-name))
      (save-excursion
	(goto-char point)
	(backward-sexp)
	(delete-region (point) point)
	(beginning-of-line)
	(when (looking-at "^\\s-*$")
          (join-line)
          (delete-trailing-whitespace (point) (line-end-position)))))))

(defun sly-export-symbol-at-point ()
  "Add the symbol at point to the defpackage source definition
belonging to the current buffer-package. With prefix-arg, remove
the symbol again. Additionally performs an EXPORT/UNEXPORT of the
symbol in the Lisp image if possible."
  (interactive)
  (let* ((symbol (sly-symbol-at-point))
         (package (or (and (string-match "^\\([^:]+\\):.*" symbol)
                           (match-string 1 symbol))
                      (sly-current-package))))
    (unless symbol (error "No symbol at point."))
    (cond (current-prefix-arg
           (let* ((attempt (sly-frob-defpackage-form package :unexport symbol))
                  (howmany (car attempt))
                  (where (buffer-file-name (cdr attempt))))
             (if (cl-plusp howmany)
                 (sly-message "Symbol `%s' no longer exported from `%s' in %s"
                              symbol package where)
               (sly-message "Symbol `%s' is not exported from `%s' in %s"
                            symbol package where)))
	   (sly-unexport-symbol symbol package))
	  (t
           (let* ((attempt (sly-frob-defpackage-form package :export symbol))
                  (howmany (car attempt))
                  (where (buffer-file-name (cdr attempt))))
             (if (cl-plusp howmany)
                 (sly-message "Symbol `%s' now exported from `%s' in %s"
                              symbol package where)
               (sly-message "Symbol `%s' already exported from `%s' in %s"
                            symbol package where)))
	   (sly-export-symbol symbol package)))))

(defun sly-export-class (name)
  "Export acessors, constructors, etc. associated with a structure or a class"
  (interactive (list (sly-read-from-minibuffer "Export structure named: "
                                                 (sly-symbol-at-point))))
  (let* ((package (sly-current-package))
         (symbols (sly-eval `(slynk:export-structure ,name ,package))))
    (sly-message "%s symbols exported from `%s'"
             (car (sly-frob-defpackage-form package :export symbols))
             package)))

(defalias 'sly-export-structure 'sly-export-class)

;; 
;; Dealing with import-from
;;

(defun sly-package-fu--search-import-from (package)
  (let* ((normalized-package (sly-package-fu--normalize-name package))
         (regexp (format "(:import-from[ \t']*\\(:\\|#:\\)?%s"
                         (regexp-quote normalized-package))))
    (re-search-forward regexp nil t)))


(defun sly-package-fu--create-new-import-from (package symbol)
  "Add new :IMPORT-FROM subform for PACKAGE.  Add SYMBOL.
Assumes point just before start of DEFPACKAGE form"
  (forward-sexp)
  ;; Now, search last :import-from or :use form
  (cond
   ((or (re-search-backward "(:\\(use\\|import-from\\)" nil t)
        (and (re-search-backward "def[[:alnum:]]*package" nil t)
             (progn (forward-sexp) t)))
    ;; Skip found expression
    (forward-sexp)
    ;; and insert a new (:import-from <package> <symbol>) form.
    (newline-and-indent)
    (let ((symbol-name (sly-format-symbol-for-defpackage symbol))
          (package-name (sly-format-symbol-for-defpackage package)))
      (insert "(:import-from )")
      (backward-char)
      (insert package-name)
      (newline-and-indent)
      (insert symbol-name)))
    (t (error "Can't find suitable place for :import-from defpackage form."))))


(defun sly-package-fu--add-or-update-import-from-form (symbol)
  "Do the heavy-lifting for `sly-import-symbol-at-point'.

Accept a string or a symbol like \"alexandria:with-gensyms\",
and add it to existing (import-from #:alexandria ...) form, or
create a new one. Return name of the given symbol inside of its
package.  For example above, return \"with-gensyms\"."
  (let* ((package (or (funcall sly-import-symbol-package-transform-function
                               (sly-cl-symbol-package symbol))
                      ;; We only process symbols in fully qualified form like
                      ;; weblocks/request:get-parameter
                      (user-error "`%s' is not a package-qualified symbol."
                                  symbol)))
         (simple-symbol (sly-cl-symbol-name symbol)))
    (save-excursion
      ;; First go to just before relevant DEFPACKAGE form
      ;;
      (sly-goto-package-source-definition (sly-current-package))

      ;; Ask CL to actually import the symbol (a synchronized eval
      ;; makes sure an error aborts the rest of the command)
      ;;
      (sly-eval `(slynk:import-symbol-for-emacs ,symbol
                                                ,(sly-current-package)
                                                ,package))
      (if (sly-package-fu--search-import-from package)
          ;; If specific (:IMPORT-FROM PACKAGE... ) subform exists,
          ;; attempt to insert package-less SYMBOL there.
          (let ((imported-symbols (mapcar #'sly-package-fu--normalize-name
                                          (sly-package-fu--read-symbols))))
            (unless (cl-member simple-symbol
                               imported-symbols
                               :test 'cl-equalp)
              (sly-package-fu--insert-symbol simple-symbol)
              (when sly-package-fu-save-file (save-buffer))))
        ;; Else, point is unmoved.  Add a new (:IMPORT-FROM PACKAGE)
        ;; subform after any other existing :IMPORT-FROM or :USE
        ;; subforms.
        (sly-package-fu--create-new-import-from package
                                                simple-symbol)
        (when sly-package-fu-save-file (save-buffer)))
      ;; Always return symbol-without-package, because it is useful
      ;; to replace symbol at point and change it from fully qualified
      ;; form to a simple-form
      simple-symbol)))


(defun sly-import-symbol-at-point ()
  "Add a qualified symbol to package's :import-from subclause.

Takes a package-qualified symbol at point, adds it to the current
package's defpackage form (under its :import-form subclause) and
replaces with a symbol name without the package designator."
  (interactive)
  (let* ((bounds (sly-bounds-of-symbol-at-point))
         (beg (set-marker (make-marker) (car bounds)))
         (end (set-marker (make-marker) (cdr bounds))))
    (when bounds
      (let ((non-qualified-name
             (sly-package-fu--add-or-update-import-from-form
              (buffer-substring-no-properties beg end))))
        (when non-qualified-name
          (delete-region beg end)
          (insert non-qualified-name))))))


(provide 'sly-package-fu)
