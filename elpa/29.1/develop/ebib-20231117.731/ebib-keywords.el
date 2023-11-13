;;; ebib-keywords.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2023 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This file is part of Ebib, a BibTeX database manager for Emacs.  It contains
;; the keywords code.

;;; Code:

(require 'cl-lib)
(require 'ebib-utils)
(require 'ebib-db)

(defgroup ebib-keywords nil "Keyword settings for Ebib." :group 'ebib)

(defcustom ebib-keywords-separator ", "
  "String for separating keywords in the `keyword' field.
This separator is also used to separate multiple identical
fields, since those are most likely keyword fields."
  :group 'ebib
  :type '(string :tag "Keyword separator:"))

(defcustom ebib-keywords nil
  "If set, keep a list of canonical keywords.
This option can be a list of keywords or the name of a file to
which the keywords are saved."
  :group 'ebib-keywords
  :type '(choice (const :tag "Read keywords from the .bib file." nil)
                 (repeat :tag "Specify keywords" (string :tag "Keyword"))
                 (file :tag "Use a keyword file")))

(make-obsolete-variable 'ebib-keywords-list 'ebib-keywords "Ebib 2.28")
(make-obsolete-variable 'ebib-keywords-file 'ebib-keywords "Ebib 2.28")
(make-obsolete-variable 'ebib-keywords-file-save-on-exit 'ebib-keywords-save-on-exit "Ebib 2.28")
(make-obsolete-variable 'ebib-keywords-use-only-file "The variable `ebib-keywords-use-only-file' is no longer used." "Ebib 2.28")

(defcustom ebib-keywords-add-new-to-canonical t
  "Add new keywords to the canonical list.
if this option is set, a keyword that is added to the
\"Keywords\" field that is not on the canonical list is added to
it.  Note that this option only takes effect if `ebib-keywords'
is set."
  :group 'ebib-keywords
  :type '(choice (const :tag "Add new keywords to the canonical list" t)
                 (const :tag "Do not add new keywords to the canonical list" nil)))

(defcustom ebib-keywords-save-on-exit 'ask
  "Whether to save new keywords automatically or not.
This option only takes effect if `ebib-keywords' is set."
  :group 'ebib-keywords
  :type '(choice (const :tag "Ask whether to save" ask)
                 (const :tag "Always save on exit" always)
                 (const :tag "Do not save on exit" nil)))

(defcustom ebib-keywords-field-keep-sorted nil
  "Keep the keywords field sorted in alphabetical order.
Also automatically remove duplicates."
  :group 'ebib-keywords
  :type '(choice (const :tag "Sort keywords field" t)
                 (const :tag "Do not sort keywords field" nil)))

(defvar ebib--keywords-completion-list nil "List of keywords offered for completion.")

(defsubst ebib--keywords-to-list (str)
  "Convert STR to a list of keywords.
STR should be a string containing keywords separated by
`ebib-keywords-separator'."
  (split-string str (regexp-quote (string-trim ebib-keywords-separator))
		t "[[:space:]]*"))

(defun ebib--keywords-sort (keywords)
  "Sort the KEYWORDS string, remove duplicates, and return it as a string.
Note: KEYWORDS should be unbraced."
  (mapconcat 'identity
             (sort (delete-dups (ebib--keywords-to-list keywords))
                   'string<)
             ebib-keywords-separator))

(defun ebib--keywords-add-to-completion-list (keyword)
  "Add KEYWORD to the list of keywords offered for completion.
Mark the keyword list as modified, so that if a canonical set of
keywords is used (i.e., if `ebib-keywords' is set), it can be
saved when Ebib is exited.  If KEYWORD is already in the list, it
is not added again and the list is not marked as modified."
  (unless (member-ignore-case keyword ebib--keywords-completion-list)
    (push keyword ebib--keywords-completion-list)
    (put 'ebib--keywords-completion-list :modified t)))

(defun ebib--keywords-load-canonical-list ()
  "Load the canonical keywords list.
The list is either copied from `ebib-keywords' or read form the
file `ebib-keywords' points to.  Return value is the list of
keywords."
  (setq ebib--keywords-completion-list (cond
                                        ((listp ebib-keywords) (copy-sequence ebib-keywords))
                                        ((stringp ebib-keywords)
                                         (if (file-readable-p ebib-keywords)
                                             (prog1
                                                 (ebib--read-file-to-list ebib-keywords)
                                               (ebib--log 'log "Using keywords file `%s'." ebib-keywords))
                                           (ebib--log 'warning "Keywords file `%s' not readable." ebib-keywords)))
                                        (t nil))))

(defun ebib--keywords-save-canonical-list ()
  "Save the list of keywords.
Depending on the value of `ebib-keywords', the list of keywords
is saved as a user option to the user's `custom-file' or to a
file."
  (cond
   ((listp ebib-keywords)
    (customize-save-variable 'ebib-keywords ebib--keywords-completion-list))
   ((and (stringp ebib-keywords)
         (file-writable-p ebib-keywords))
    (ebib--keywords-save-to-file ebib-keywords ebib--keywords-completion-list))))

(defun ebib--keywords-save-to-file (file keywords)
  "Save KEYWORDS to FILE."
  (if (file-writable-p file)
      (with-temp-buffer
        (mapc (lambda (keyword)
                (insert (format "%s\n" keyword)))
              keywords)
        (write-region (point-min) (point-max) file))
    (ebib--log 'warning "Could not write to keywords file `%s'" file)))

(provide 'ebib-keywords)

;;; ebib-keywords.el ends here
