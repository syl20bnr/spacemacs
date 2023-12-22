;;; ebib-reading-list.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

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
;; the code for managing the reading list.

;;; Code:

(require 'ebib-utils)

(declare-function org-get-todo-state "org.el")
(declare-function org-todo "org.el")

(defgroup ebib-reading-list nil "Settings for the reading list." :group 'ebib)

(defcustom ebib-reading-list-symbol "R"
  "Symbol used to indicate that the current entry is on the reading list.
If the entry is on the reading list, this symbol is displayed in
the mode line of the entry buffer after the entry key."
  :group 'ebib-reading-list
  :type '(string :tag "Reading list symbol"))

(defcustom ebib-reading-list-file nil
  "File for storing the reading list."
  :group 'ebib-reading-list
  :type '(choice (const :tag "No reading list")
                 (file :tag "Reading list file")))

(defcustom ebib-reading-list-template "* %M %T\n:PROPERTIES:\n%K\n:END:\n%F\n"
  "Template for an entry in the reading list.
New entries are created on the basis of this template.  The
template can contain format specifiers consisting of a percent
sign and a character.  These specifiers are defined by
`ebib-notes-template-specifiers'.  Note that the `%K' specifier
must be present in the template and should be replaced by an
identifier that is unique for the entry.  This identifier is used
to retrieve the item.  Without it, Ebib is not able to determine
whether an entry is on the reading list or not."
  :group 'ebib-reading-list
  :type '(string "Reading list item template"))

(defcustom ebib-reading-list-template-specifiers '((?K . ebib-reading-list-create-org-identifier)
                                                   (?T . ebib-create-org-title)
                                                   (?M . ebib-reading-list-todo-marker)
                                                   (?L . ebib-create-org-link)
                                                   (?F . ebib-create-org-file-link)
                                                   (?D . ebib-create-org-doi-link)
                                                   (?U . ebib-create-org-url-link))
  "Specifiers used in `ebib-reading-list-template'.
Each specifier consists of a character (which is preceded by a
percent sign in `ebib-reading-list-template') and a symbol, which
either names a function to be executed or a variable, which
should hold a string.  If a function, it should take two
arguments, the entry key and the database, and should return a
string that is substituted for the specifier in the template."
  :group 'ebib-notes
  :type '(repeat (cons :tag "Specifier"
                       (character :tag "Character")
                       (symbol :tag "Function or variable"))))

(defun ebib-reading-list-create-org-identifier (key _)
  "Create a unique identifier for KEY for use in a reading list file.
The prefix \"reading_\" is added to the key to create an
identifier that differs from the identifier used in notes files.
Furthermore, the string \"Custom_id:\" is prepended, so that it
can be used in an org :PROPERTIES: block."
  (format ":Custom_id: reading_%s" key))

(defcustom ebib-reading-list-todo-marker "TODO"
  "Marker for reading list items that are still open."
  :group 'ebib-reading-list
  :type '(string :tag "Todo marker"))

(defcustom ebib-reading-list-done-marker "DONE"
  "Marker for reading list items that are done."
  :group 'ebib-reading-list
  :type '(string :tag "Done marker"))

(defcustom ebib-reading-list-add-item-function 'ebib-reading-list-move-point-default
  "Function to run when adding an item to the reading list.
This function is run with point at the beginning of the buffer
and should move point to the correct position in the buffer.

The default function simply moves point to the end of the buffer."
  :group 'ebib-reading-list
  :type 'function)

(defun ebib-reading-list-move-point-default ()
  "Default value for `ebib-reading-list-add-item-function'.
This function just moves point to the end of the buffer."
  (goto-char (point-max)))

(defcustom ebib-reading-list-remove-item-function 'ebib-reading-list-mark-item-as-done
  "Function to run when removing an item from the reading list.
This function is run with point positioned after the item's
identifier.  The default value removes the current orgmode
subtree, but if your reading list is not an org file, you may
want to set another function here."
  :group 'ebib-reading-list
  :type 'function)

(defun ebib-reading-list-mark-item-as-done ()
  "Mark the current reading list item as done."
  (org-todo ebib-reading-list-done-marker))

(defcustom ebib-reading-list-item-active-function 'ebib-reading-list-item-org-active-p
  "Function to determine whether a reading list item is active.
This function is called with point inside the item, at the end of
the item's identifier.  It should return non-nil when the item is
still pending, nil if it is done."
  :group 'ebib-reading-list
  :type 'function)

(defun ebib-reading-list-item-org-active-p ()
  "Return t if point is in a reading list item that is still active."
  (string= (org-get-todo-state) ebib-reading-list-todo-marker))

(defcustom ebib-reading-list-new-item-hook nil
  "Hook run when a new reading list item is created.
The functions in this hook can use the variable `ebib--cur-db' to
access the current database, the function `ebib--get-key-at-point'
to obtain the key of the current entry, and the database
functions, especially `ebib-get-field-value' and
`ebib-db-get-entry', to access the current entry's data
fields."
  :group 'ebib-reading-list
  :type 'hook)

(defcustom ebib-reading-list-remove-item-hook nil
  "Hook run when an item is removed from the reading list.
The functions in this hook can use the variable `ebib--cur-db' to
access the current database, the function `ebib--get-key-at-point'
to obtain the key of the current entry, and the database
functions, especially `ebib-get-field-value' and
`ebib-db-get-entry', to access the current entry's data fields."
  :group 'ebib-reading-list
  :type 'hook)

(defun ebib--reading-list-buffer ()
  "Return the buffer containing the reading list.
If the file has not been opened yet, open it, creating it if
necessary.  An error is raised if the location for the reading
list file is not accessible to the user."
  (unless ebib-reading-list-file
    (error "[Ebib] No reading list file defined"))
  (unless (file-writable-p ebib-reading-list-file)
    (error "[Ebib] Cannot read or create reading list file"))
  (find-file-noselect ebib-reading-list-file))

(defun ebib--reading-list-item-p (key)
  "Return t if KEY is on the reading list."
  (if (and ebib-reading-list-file
           (file-writable-p ebib-reading-list-file))
      (with-current-buffer (ebib--reading-list-buffer)
        (save-excursion
          (let ((loc (ebib--reading-list-locate-item key)))
            (when loc
              (goto-char loc)
              (funcall ebib-reading-list-item-active-function)))))))

(defun ebib--reading-list-locate-item (key)
  "Return the location of the reading list item for KEY.
Specifically, the location of the final character of the
identifier is returned.  If there is no item for KEY, the return
value is nil.  Note that this function searches in the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward (funcall (cdr (assoc ?K ebib-reading-list-template-specifiers)) key nil) nil t)))

(defun ebib--reading-list-new-item (key db)
  "Add a reading list item for KEY in DB.
Return KEY.  If there is already an item for KEY, do nothing and
return nil."
  (with-current-buffer (ebib--reading-list-buffer)
    (unless (ebib--reading-list-locate-item key)
      (funcall ebib-reading-list-add-item-function)
      (insert (ebib--reading-list-fill-template key db))
      (run-hooks 'ebib-reading-list-new-item-hook)
      (save-buffer)
      key)))

(defun ebib--reading-list-remove-item (key)
  "Remove the reading list item for KEY.
Return KEY if the item was removed.  If there is no item for KEY,
do nothing and return nil."
  (with-current-buffer (ebib--reading-list-buffer)
    (let ((location (ebib--reading-list-locate-item key)))
      (when location
        (goto-char location)
        (funcall ebib-reading-list-remove-item-function)
        (run-hooks 'ebib-reading-list-remove-item-hook)
        (save-buffer)
        key))))

(defun ebib--reading-list-fill-template (key db)
  "Create the text for a reading list item for KEY in DB."
  (ebib-format-template ebib-reading-list-template ebib-reading-list-template-specifiers key db))

(provide 'ebib-reading-list)

;;; ebib-reading-list.el ends here
