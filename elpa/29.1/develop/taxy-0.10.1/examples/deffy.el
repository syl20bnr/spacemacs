;;; deffy.el --- Show definitions in an Elisp project/buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience, lisp
;; Package-Requires: ((emacs "27.2") (taxy "0.7"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides commands that show top-level forms and
;; definitions found in an Emacs Lisp project or buffer, organized by
;; file and type.

;;; Code:

(require 'map)
(require 'project)

(require 'taxy)
(require 'taxy-magit-section)

(cl-defstruct deffy-def
  ;; Okay, the name of this struct is silly, but at least it's concise.
  file pos form name type docstring)

(defgroup deffy nil
  "Show an overview of definitions in an Emacs Lisp project or buffer."
  :group 'emacs-lisp-mode)

;;;; Variables

(defvar deffy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'deffy-RET)
    (define-key map [mouse-1] #'deffy-mouse-1)
    map))

(defvar-local deffy-directory nil
  "Directory relative to which filenames should be expanded.")

(defvar-local deffy-files nil
  "Files shown in the current Deffy buffer.")

(defvar-local deffy-display-buffer-action nil
  "Last-used display-buffer-action in the current Deffy buffer.")

;;;; Keys

(cl-eval-when (compile load eval)
  ;; I don't understand why using `cl-eval-when' is necessary, but it
  ;; seems to be.
  (taxy-define-key-definer deffy-define-key deffy-keys "deffy-key"
    ;; FIXME: Docstring.
    "")

  (deffy-define-key file ()
    (file-relative-name (deffy-def-file item) deffy-directory))

  (deffy-define-key type ()
    (pcase-let* (((cl-struct deffy-def form) item)
	         (type (pcase form
		         (`(,(or 'defun 'cl-defun 'defalias) . ,_)
			  (if (cl-find-if (lambda (form)
					    (pcase form
					      (`(interactive . ,_) t)))
					  form)
			      'command
			    'function))
		         (`(,(or 'defmacro 'cl-defmacro) . ,_)
			  'macro)
                         (`(,(or 'cl-defstruct) . ,_)
			  'struct)
                         (`(,(or 'defclass) . ,_)
			  'class)
                         (`(,(or 'cl-defmethod 'defmethod) . ,_)
			  'method)
                         (`(,(or 'define-error) . ,_)
			  'error)
                         (`(,(or 'defconst 'defcustom 'defgroup 'defvar 'defvar-local) . ,_)
                          'variable)
                         (`(,(or 'define-hash-table-test) . ,_)
                          'hash-table-test)
                         (`(,(or 'defface) . ,_)
                          'face)
                         (`(,(or 'provide 'require) . ,_)
                          'feature)
                         ;; Top-level forms that don't usually correspond to definitions,
                         ;; so we ignore them.
                         (`(,(or 'cl-eval-when 'eval-and-compile 'eval-when-compile 'with-eval-after-load) . ,_)
                          nil)
                         (`(,(or 'declare-function) . ,_)
                          nil)
                         (`(,(or 'unless 'when 'if-let 'if-let* 'when-let 'when-let* '-if-let '-if-let* '-when-let '-when-let* 'setf 'setq) . ,_)
                          nil)
                         ;; Top-level forms that are macro calls (e.g. custom defining macros).
                         ((and `(,car . ,_) (guard (macrop car))) car)
                         ;; Anything else: ignored.
		         (`(,car . ,_) nil))))
      ;; FIXME: Returning nil for these ignored types only works when the form is in the
      ;; top-level file, i.e. when the file's relative name is nil, so these ignored types
      ;; still show up when they're in other files.  This isn't really the right way to
      ;; discard uninteresting items.

      ;; FIXME: Also, when a project Lisp file is not loaded into Emacs, some symbols may
      ;; not be correctly classified, e.g. defining macros.  It's probably not feasible to
      ;; solve that completely correctly, so some options or workarounds may be needed.
      (when type
        (format "%s" type)))))

(defvar deffy-taxy-default-keys
  '(type file))

;;;; Columns

(cl-eval-when (compile load eval)
  ;; I don't understand why using `cl-eval-when' is necessary, but it
  ;; seems to be.
  (taxy-magit-section-define-column-definer "deffy"))

(deffy-define-column "Definition" (:max-width 45 :face font-lock-function-name-face)
  (format "%s" (deffy-def-name item)))

(deffy-define-column "Type" (:max-width 25 :face font-lock-type-face)
  (format "%s" (deffy-def-type item)))

(deffy-define-column "Docstring" (:max-width nil :face font-lock-doc-face)
  (deffy-def-docstring item))

(unless deffy-columns
  ;; TODO: Automate this or document it
  (setq-default deffy-columns
		(get 'deffy-columns 'standard-value)))

;;;; Options

(defcustom deffy-side-window-action
  '(display-buffer-in-side-window
    (side . right)
    (window-parameters
     (dedicated . t)
     (window-side . right)
     (no-delete-other-windows . t)))
  "`display-buffer' action used when displaying Deffy buffer in a side window.
See Info node `(elisp)Displaying Buffers in Side Windows'."
  :type 'sexp)

;;;; Commands

;;;###autoload
(cl-defun deffy
    (&key (project (or (project-current)
		       (cons 'transient default-directory)))
	  (keys deffy-taxy-default-keys)
	  (files deffy-files)
	  (buffer-name (format "*Deffy: %s*"
			       (if files
				   (string-join (mapcar #'file-name-nondirectory files) ", ")
				 (file-name-nondirectory
				  (directory-file-name (project-root project))))))
	  visibility-fn display-buffer-action)
  "Show definitions defined in PROJECT or FILES.
Interactively, with PREFIX, show only definitions in current
buffer."
  (interactive (list :files (when current-prefix-arg
			      (list (buffer-file-name)))
		     :keys (if current-prefix-arg
			       (remove 'file deffy-taxy-default-keys)
			     deffy-taxy-default-keys)))
  (let (format-table column-sizes)
    (cl-labels (;; (heading-face
		;;  (depth) (list :inherit (list 'bufler-group (bufler-level-face depth))))
		(elisp-file-p (file) (string-match-p (rx ".el" (optional ".gz") eos) file))
		(file-visible-p
		 (file) (not (string-match-p (rx bos ".") (file-name-nondirectory file))))
		(format-item (item) (gethash item format-table))
		(make-fn (&rest args)
			 (apply #'make-taxy-magit-section
				:make #'make-fn
				:format-fn #'format-item
				:level-indent deffy-level-indent
				:visibility-fn visibility-fn
				;; :heading-face-fn #'heading-face
				args))
		(def-name (def) (format "%s" (cl-second (deffy-def-form def)))))
      ;; (when (get-buffer buffer-name)
      ;;   (kill-buffer buffer-name))
      (setf files (cl-reduce #'cl-remove-if-not (list #'file-exists-p #'elisp-file-p #'file-visible-p)
			     :initial-value (or files (project-files project))
			     :from-end t))
      (unless files
        (user-error "No files to show"))
      (with-current-buffer (get-buffer-create buffer-name)
	(deffy-mode)
	(setq-local deffy-taxy-default-keys keys
		    deffy-directory (project-root project)
		    deffy-files files
		    deffy-display-buffer-action display-buffer-action
		    default-directory deffy-directory)
	(let* ((forms (apply #'append (mapcar #'deffy--file-forms files)))
	       (taxy (thread-last
		       (make-fn
			:name "Deffy"
			:description
                        (format "Definitions in %s:"
				(if files
				    (string-join (mapcar #'file-relative-name files) ", ")
				  (file-name-nondirectory
				   (directory-file-name (project-root project)))))
			:take (taxy-make-take-function keys deffy-keys))
		       (taxy-fill forms)
		       (taxy-sort* #'string< #'taxy-name)
		       (taxy-sort #'string< #'def-name)))
	       (taxy-magit-section-insert-indent-items nil)
	       (inhibit-read-only t)
	       (format-cons (taxy-magit-section-format-items
			     deffy-columns deffy-column-formatters taxy)))
	  (setf format-table (car format-cons)
		column-sizes (cdr format-cons)
		header-line-format (taxy-magit-section-format-header
				    column-sizes deffy-column-formatters))
          (delete-all-overlays)
          (erase-buffer)
	  (save-excursion
	    (taxy-magit-section-insert taxy :items 'last
	      ;; :blank-between-depth bufler-taxy-blank-between-depth
	      :initial-depth 0))))
      (pop-to-buffer buffer-name display-buffer-action))))

;;;###autoload
(cl-defun deffy-buffer
    (&optional (buffer (current-buffer))
	       &key display-buffer-action)
  "Show an Deffy view for BUFFER.
Interactively, with prefix, display in dedicated side window."
  (interactive
   (list (current-buffer)
	 :display-buffer-action (when current-prefix-arg
				  deffy-side-window-action)))
  (unless (buffer-file-name buffer)
    (user-error "Buffer is not file-backed: %S.  See command `deffy-project'"
		buffer))
  (deffy :files (list (buffer-file-name buffer))
    :keys (remove 'file deffy-taxy-default-keys)
    :display-buffer-action display-buffer-action))

(cl-defun deffy-project (&optional project &key display-buffer-action)
  "Show an Deffy view for PROJECT.
Interactively, with prefix, display in dedicated side window."
  (interactive
   (list nil :display-buffer-action (when current-prefix-arg
				      deffy-side-window-action)))
  (deffy :project (or project
		      (project-current)
		      (cons 'transient default-directory))
    :display-buffer-action display-buffer-action))

(defun deffy-revert (_ignore-auto _noconfirm)
  "Revert current Deffy buffer."
  (interactive)
  (deffy :display-buffer-action (or deffy-display-buffer-action
				    '((display-buffer-same-window)))))

(defun deffy-jump (def)
  "Jump to definition DEF.
Interactively, read DEF from current buffer with completion; with
universal prefix, from project buffers; with two universal
prefixes, from all `deffy-mode' buffers."
  (interactive
   (list (deffy--read-def
           (pcase current-prefix-arg
             (`nil (deffy--buffer-for (current-buffer)))
             ('(4) (save-window-excursion
	             (deffy-project)
	             (list (current-buffer))))
             (_ (deffy--all-buffers))))))
  (pcase-let (((cl-struct deffy-def file pos) def)
              (action (if (eq 'deffy-mode major-mode)
                          `(display-buffer-in-previous-window
                            (previous-window . ,(get-mru-window nil nil 'not-selected)))
                        '(display-buffer-same-window))))
    (pop-to-buffer
     (or (find-buffer-visiting file)
	 (find-file-noselect file))
     action)
    (goto-char pos)
    (backward-sexp 1)))

(defun deffy-mouse-1 (event)
  "Call `deffy-RET' with point at EVENT's position."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'deffy-RET))

(defun deffy-RET ()
  "Go to form at point, or expand section at point."
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (deffy-def (deffy-jump (oref (magit-current-section) value)))
    (taxy-magit-section (call-interactively #'magit-section-cycle))
    (null nil)))

(define-derived-mode deffy-mode magit-section-mode "Deffy"
  :global nil
  (setq-local bookmark-make-record-function #'deffy-bookmark-make-record
	      revert-buffer-function #'deffy-revert))

;;;; Functions

(cl-defun deffy--read-def
    (deffy-buffers &key
      affixation-fn
      (annotate-fn (lambda (def)
		     (concat (deffy-def-type def) " " (deffy-def-docstring def))))
      (group-fn #'deffy-def-file))
  "Read form selected from Deffy BUFFERS with completion."
  (unless deffy-buffers
    (user-error "No Deffy buffers to find in"))
  (cl-labels ((disambiguate (string)
                            (format "%s (%s)"
                                    string (deffy-def-type (get-text-property 0 :def string))))
              (def-cons
		(def) (cons (propertize
			     (format "%s" (deffy-def-name def))
			     :annotation (funcall annotate-fn def)
			     :group (funcall group-fn def)
			     :def def)
			    def))
	      (buffer-taxy
	       (buffer) (with-current-buffer buffer
			  (save-excursion
			    (goto-char (point-min))
			    (oref (magit-current-section) value))))
	      (annotate
	       (candidate)
	       (concat (propertize " " 'display '(space :align-to center))
		       (get-text-property 0 :annotation candidate)))
	      (group
	       (candidate transform)
	       (pcase transform
		 (`nil (get-text-property 0 :group candidate))
		 (_ candidate)))
	      (affix (candidates)
		     (cl-loop for candidate in candidates collect
		              (list (propertize candidate
					        'face 'font-lock-function-name-face)
			            (concat (propertize
                                             (symbol-name
                                              (deffy-def-type
					        (get-text-property 0 :def candidate)))
					     'face 'font-lock-type-face)
				            "  ")
			            (concat (propertize " "
                                                        'display '(space :align-to center))
				            (get-text-property 0 :annotation candidate))))))
    (pcase (length deffy-buffers)
      (1 (setf annotate-fn #'deffy-def-docstring
	       group-fn #'deffy-key-type))
      (_ (setf annotate-fn #'deffy-def-docstring
               affixation-fn #'affix)))
    (let* ((taxys (mapcar #'buffer-taxy deffy-buffers))
	   (items (mapcan #'taxy-flatten taxys))
	   (alist (mapcar #'def-cons items))
           ;; Unfortunately, `completing-read' always discards text properties, which
           ;; means that they can't be used to disambiguate items with the same name
           ;; (e.g. `(defthis foo)' in one form and `(defthat foo)' in another).  So we
           ;; have to check for items with duplicate names, then replace the string with
           ;; one that disambiguates them.
           (duplicates (cl-loop for item in alist
                                when (> (cl-count (car item) alist :key #'car :test #'equal) 1)
                                collect item))
           (_ (when duplicates
                (dolist (dupe duplicates)
                  (setf alist (remove dupe alist)
                        dupe (cons (disambiguate (car dupe)) (cdr dupe)))
                  (push dupe alist))))
	   (metadata (list 'metadata (cons 'group-function #'group)))
	   (dynamic-fn (lambda (str pred flag)
			 (pcase flag
			   ('metadata metadata)
			   (_ (complete-with-action flag alist str pred)))))
	   (completion-extra-properties (list :annotation-function #'annotate
					      :affixation-function affixation-fn))
	   (selected (completing-read "Definition: " dynamic-fn nil t)))
      (alist-get selected alist nil nil #'equal))))

(cl-defun deffy--file-forms (file)
  "Return forms defined in FILE."
  (with-temp-buffer
    (save-excursion
      (insert-file-contents file))
    (cl-loop for form = (ignore-errors
			  (read (current-buffer)))
	     while form
	     when (listp form)
	     collect (make-deffy-def
                      :file file :pos (point) :form form
                      :name (pcase-exhaustive (cadr form)
			      ((and (pred atom) it) it)
			      (`(quote ,it) it)
			      (`(,it . ,_) it))
                      :type (car form)
                      :docstring
                      (replace-regexp-in-string
                       "\n" "  "
                       (pcase form
		         (`(,(or 'defun 'cl-defun 'defmacro 'cl-defmacro) ,_name ,_args
		            ,(and (pred stringp) docstring) . ,_)
		          docstring)
		         (`(,(or 'defvar 'defvar-local 'defcustom) ,_name ,_value
		            ,(and (pred stringp) docstring) . ,_)
		          docstring)
		         (_ ;; Use the first string found, if any.
		          (or (cl-find-if #'stringp form)
                              ""))))))))

(defun deffy--all-buffers ()
  "Return list of all `deffy-mode' buffers."
  (cl-loop for buffer in (buffer-list)
	   when (eq 'deffy-mode (buffer-local-value 'major-mode buffer))
	   collect buffer))

(defun deffy--buffer-for (buffer)
  "Return `deffy-mode' buffer having definitions for BUFFER.
Return value is actually a one-element list."
  (or (cl-loop for other-buffer in (buffer-list)
	       when (and (eq 'deffy-mode (buffer-local-value 'major-mode other-buffer))
		         (or (member (buffer-file-name buffer)
                                     (buffer-local-value 'deffy-files other-buffer))
                             (equal default-directory (buffer-local-value 'deffy-directory other-buffer))))
	       return (list other-buffer))
      ;; Make a new deffy buffer for BUFFER.
      (condition-case nil
	  (save-window-excursion
	    (deffy-buffer)
	    (list (current-buffer)))
	(error (cl-loop for window in (window-list)
                        when (eq 'deffy-mode
                                 (buffer-local-value 'major-mode (window-buffer window)))
                        return (list (window-buffer window)))))))

;;;;; Bookmark support

;; FIXME: If a project gains a file after a Deffy bookmark is made, the restored Deffy
;; buffer won't show the new file.

(defvar bookmark-make-record-function)

(defun deffy-bookmark-make-record ()
  "Return a bookmark record for current Deffy buffer."
  (list (concat "Deffy: %s" deffy-directory)
	(cons 'directory deffy-directory)
	(cons 'files deffy-files)
	(cons 'handler #'deffy-bookmark-handler)))

;;;###autoload
(defun deffy-bookmark-handler (record)
  "Show Deffy buffer for bookmark RECORD."
  (pcase-let* ((`(,_ . ,(map directory files)) record))
    (deffy :files files :project (project-current nil directory))
    (current-buffer)))

(provide 'deffy)

;;; deffy.el ends here
