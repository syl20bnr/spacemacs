;;; org-vcard.el --- org-mode support for vCard export and import.

;; Copyright (C) 2014-2019,2021-2022 Alexis <flexibeast@gmail.com>, Will Dey <will123dey@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>, Will Dey <will123dey@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2014-07-31
;; URL: https://github.com/flexibeast/org-vcard
;; Keywords: outlines, org, vcard

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; *This project is currently unmaintained, and needs a new
;; maintainer. If you wish to take maintainership, please email
;; Alexis.*

;; `org-vcard' is a package for exporting and importing
;; [vCards](https://en.wikipedia.org/wiki/Vcard) from within [GNU
;; Emacs](https://www.gnu.org/software/emacs/)' [Org
;; mode](http://orgmode.org/).

;; ## Table of Contents

;; - [Features](#features)
;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [TODO](#todo)
;; - [Issues](#issues)
;; - [Testing](#testing)
;; - [License](#license)

;; ## Features

;; * _Backwards-compatible with org-contacts.el._

;; `org-vcard' comes with a built-in contacts style called `flat',
;; which adheres to org-contacts' method of structuring contacts and
;; contact information. It not only supports the properties specified
;; in org-contacts.el, but many other properties as well.

;; * _Basic support for vCard 4.0, 3.0 and 2.1._

;; `org-vcard' is working towards full compliance with the vCard 4.0
;; ([RFC 6350](https://tools.ietf.org/html/rfc6350)), 3.0 ([RFC
;; 2426](https://tools.ietf.org/html/rfc2426) and [RFC
;; 4770](https://tools.ietf.org/html/rfc4770)) and
;; [2.1](https://web.archive.org/web/20120501162958/http://www.imc.org/pdi/vcard-21.doc)
;; specifications.

;; * _New contacts style: `tree'._

;; `org-vcard' introduces a new style for Org contacts, called
;; `tree'. More details [below](#tree).

;; * _Highly customisable, via Emacs' `customize' interface._

;; Modify existing contact styles; change the labels used to map
;; contact details in `org-mode' to various vCard properties/types, or
;; add new ones. Create completely new contact styles by plugging in
;; your own code to handle export and import.

;; ## Installation

;; Install `org-vcard' from [MELPA](http://melpa.org/#/org-vcard),
;; [MELPA Stable](http://stable.melpa.org/#/org-vcard), or put the
;; `org-vcard' folder in your load-path and do a `(require
;; 'org-vcard)'.

;; ## Usage

;; The main user commands are `org-vcard-export' and
;; `org-vcard-import', which are intended to be called interactively;
;; you can press TAB at many of the minibuffer prompts to get a list
;; of the available options for a prompt.

;; Both `org-vcard-export' and `org-vcard-import' are wrappers around
;; the `org-vcard-transfer-helper'
;; function. `org-vcard-transfer-helper' can be used to export and
;; import programmatically (i.e. via Emacs Lisp).

;; Enabling `org-vcard-mode' will add an "Org-vCard" menu to the menu
;; bar, from which one can access the various export, import and
;; customisation options.

;; **Note!** When exporting to vCard using the source `buffer',
;; narrowing is respected. If you wish to export the entire buffer
;; without restriction, remove any narrowing in effect.

;; For a list of the properties available by default for each contacts
;; style and related vCard versions, visit the "Org Vcard Styles
;; Languages Mappings" setting in the Org Vcard customize group, or
;; examine the value of the `org-vcard-styles-languages-mappings'
;; variable.

;; **Note!** The default mappings might need to be tweaked for
;; particular use-cases. For example, some systems create vCards
;; with a bare `TEL' property, whereas others use `TEL;TYPE=voice';
;; but both are mapped to the Org `PHONE' property (for `flat'
;; style) or `phone' FIELDTYPE (for `tree' style). In this case, the
;; `customize' interface could be used to delete whichever of the
;; two mappings is unwanted.

;; The value of the `org-vcard-include-import-unknowns' (boolean)
;; variable specifies whether the import process should include vCard
;; properties not listed in the mapping being used.

;; The value of the `org-vcard-append-to-existing-import-buffer' and
;; `org-vcard-append-to-existing-export-buffer' (boolean) variables
;; specify whether the import/export process should append to any
;; existing import/export buffer. If not, a new import/export buffer
;; is created for each import/export.

;; <a name="tree"></a>

;; ### The `tree' contacts style

;; The structure of the `tree' contacts style is:

;;     * [Contact name]
;;     :PROPERTIES:
;;     :KIND: individual
;;     :FIELDTYPE: name
;;     :END:
;;     ** [Information type]
;;     *** [Information value]
;;     :PROPERTIES:
;;     :FIELDTYPE: [Field type]
;;     [:PREFERRED:]
;;     :END:

;; Here's an example:

;;     * Joan Smith
;;     :PROPERTIES:
;;     :KIND: individual
;;     :FIELDTYPE: name
;;     :END:
;;     ** Mobile
;;     *** 0000 999 999
;;     :PROPERTIES:
;;     :FIELDTYPE: cell
;;     :END:
;;     ** Email
;;     *** Work
;;     **** address1@hidden
;;     :PROPERTIES:
;;     :FIELDTYPE: email-work
;;     :PREFERRED:
;;     :END:
;;     *** Home
;;     **** address2@hidden
;;     :PROPERTIES:
;;     :FIELDTYPE: email-home
;;     :END:

;; As the `tree' style uses a heading's FIELDTYPE property to
;; associate fields with their data, the above hierarchy is only one
;; way to structure contacts; equivalently, one could do:

;;     * People
;;     ** Joan Smith
;;     :PROPERTIES:
;;     :KIND: individual
;;     :FIELDTYPE: name
;;     :END:
;;     *** Cell
;;     **** 0000 999 999
;;     :PROPERTIES:
;;     :FIELDTYPE: cell
;;     :END:
;;     *** Email
;;     **** address1@hidden
;;     :PROPERTIES:
;;     :FIELDTYPE: email-work
;;     :PREFERRED:
;;     :END:
;;     **** address2@hidden
;;     :PROPERTIES:
;;     :FIELDTYPE: email-home
;;     :END:

;; ## TODO

;; * Add support for one-vCard-per-file export.

;; * Add support for line folding when exporting.

;; * Add support for multiple instances of EMAIL property.

;; * Add support for vCard PREF for style `flat'.

;; * Add support for vCard KINDs `group' and `org'.

;; * Add support for per-card version handling on import.

;; * Improve compliance with each version of the vCard specification.

;; * Extend test coverage.

;; <a name="issues"></a>

;; ## Issues / bugs

;; If you discover an issue or bug in `org-vcard' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on
;; GitHub](https://github.com/flexibeast/org-vcard/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `org-vcard'.

;; ## Testing

;; A basic test suite is located in the repository `tests' directory,
;; in `org-vcard-tests.el`. To run the suite:

;; 1. Ensure `org-vcard.el' has been loaded, e.g. `(load "~/org-vcard/org-vcard.el")'.
;; 2. Load the test suite: e.g. `(load "~/org-vcard/tests/org-vcard-tests.el")'.
;; 3. Run the tests by evaluating `(ert '(tag org-vcard))'.

;; ## License

;; [GNU General Public License version
;; 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any
;; later version.

;;; Code:

;;
;; This package is working towards full compliance with the
;; vCard specifications:

;; vCard 4.0: https://tools.ietf.org/html/rfc6350
;; vCard 3.0: https://tools.ietf.org/html/rfc2426
;; vCard 2.1: http://www.imc.org/pdi/vcard-21.txt

;; If you find any apparent instances of non-compliance that aren't
;; already noted in the TODO section of the org-vcard README.md
;; document, please let the maintainers know.

;; Differences between 4.0 and 3.0 can be found in Appendix A of
;; RFC6350: https://tools.ietf.org/html/rfc6350#page-73
;; Note that vCard 3.0 'types' became vCard 4.0 'properties'.

;; Differences between 3.0 and 2.1 can be found in Section 5 of
;; RFC2426: https://tools.ietf.org/html/rfc2426#page-37

;; Point of amusement:
;; In section 7 of RFC2426, the authors of the standard don't
;; include the 'N' type in their supposed-version-3.0 vCards.

;; Please refer to the TODO section of the org-vcard README.md
;; document for known limitations and/or issues.
;; 


(require 'org)
(require 'org-element)
(require 'subr-x)

(defconst org-vcard-mode-keymap (make-sparse-keymap))


;;
;; Directory configuration.
;;

(defconst org-vcard--elisp-dir (file-name-directory load-file-name)
  "Absolute path of the directory of org-vcard.el.")

(defcustom org-vcard-custom-styles-dir nil
  "Directory containing custom styles."
  :type 'directory
  :group 'org-vcard)

(defun org-vcard--styles-dirs ()
  "List of directories containing org-vcard styles."
  (let ((default (file-name-as-directory
                  (concat org-vcard--elisp-dir "styles"))))
    (if org-vcard-custom-styles-dir
        (list default org-vcard-custom-styles-dir)
      (list default))))


;;
;; Internal functions and variables.
;;

(defun org-vcard--canonicalise-adr-property (property-name)
  "Canonicalise a vCard ADR property.

Intended for use by `org-vcard--canonicalise-property-name'.

PROPERTY-NAME must be a string containing a vCard property name."
  (let ((property-canonicalised "ADR")
        (property-type-data '())
        (case-fold-search t))
    (if (string-match "HOME" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("home"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("home"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";HOME"))))))
    (if (string-match "WORK" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("work"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("work"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";WORK"))))))
    `(,property-canonicalised ,property-type-data)))


(defun org-vcard--canonicalise-email-property (property-name)
  "Canonicalise a vCard EMAIL property.

Intended for use by `org-vcard--canonicalise-property-name'.

PROPERTY-NAME must be a string containing a vCard property name."
  (let ((property-canonicalised "EMAIL")
        (property-type-data '())
        (case-fold-search t))
    (if (string-match "HOME" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("home"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("home"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";HOME"))))))
    (if (string-match "WORK" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("work"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("work"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";WORK"))))))
    `(,property-canonicalised ,property-type-data)))

(defun org-vcard--canonicalise-property-name (property-name)
  "Canonicalise a vCard property name.

Canonicalisation of a property enable its lookup in an org-vcard mapping.

PROPERTY-NAME must be a string containing the vCard property name."
  (if (not (string-match ";" property-name))
      ;; No need to do anything, return property-name unchanged.
      property-name
    ;; Property has qualifiers.
    (if (or
         (and
          (not (string-match "^ADR" property-name))
          (not (string-match "^EMAIL" property-name))
          (not (string-match "^TEL" property-name)))
         (and
          (string-match "^TEL" property-name)
          (string-match "PAGER" property-name)))
        ;; We currently only canonicalise the ADR, EMAIL and TEL
        ;; properties, and don't handle the PAGER type within the
        ;; latter, so return property-name unchanged when not dealing
        ;; with ADR, EMAIL or TEL, or when dealing with PAGER.
        property-name
      ;; Canonicalise.
      (let* ((property-canonicalised "")
             (property-type-data '())
             (retval '())
             (case-fold-search t)
             (preferred
              (if (string-match "PREF" property-name)
                  t
                nil)))
        (cond
         ((string-match "^ADR" property-name)
          (progn 
            (setq retval (org-vcard--canonicalise-adr-property property-name))
            (setq property-canonicalised (car retval))
            (setq property-type-data (cadr retval))))
         ((string-match "^EMAIL" property-name)
          (progn 
            (setq retval (org-vcard--canonicalise-email-property property-name))
            (setq property-canonicalised (car retval))
            (setq property-type-data (cadr retval))))
         ((string-match "^TEL" property-name)
          (progn 
            (setq retval (org-vcard--canonicalise-tel-property property-name))
            (setq property-canonicalised (car retval))
            (setq property-type-data (cadr retval)))))
        (cond
         ((string= "4.0" org-vcard-active-version)
          (progn
            (if property-type-data
                (progn
                  (setq property-canonicalised
                        (concat property-canonicalised ";TYPE=\""))
                  (let ((processed-one nil))
                    (dolist (type property-type-data)
                      (if processed-one
                          (setq property-canonicalised
                                (concat property-canonicalised "," type))
                        (progn
                          (setq property-canonicalised
                                (concat property-canonicalised type))
                          (setq processed-one t)))))
                  (setq property-canonicalised
                        (concat property-canonicalised "\""))))
            (if preferred
                (setq property-canonicalised
                      (concat property-canonicalised ";PREF=1")))))
         ((string= "3.0" org-vcard-active-version)
          (progn
            (if property-type-data
                (progn
                  (setq property-canonicalised
                        (concat property-canonicalised ";TYPE="))
                  (let ((processed-one nil))
                    (dolist (type property-type-data)
                      (if processed-one
                          (setq property-canonicalised
                                (concat property-canonicalised "," type))
                        (progn
                          (setq property-canonicalised
                                (concat property-canonicalised type))
                          (setq processed-one t)))))))
            (if preferred
                (if property-type-data
                    (setq property-canonicalised
                          (concat property-canonicalised ",pref"))
                  (setq property-canonicalised
                        (concat property-canonicalised ";TYPE=pref"))))))
         ((string= "2.1" org-vcard-active-version)
          (progn
            (dolist (type property-type-data)
              (setq property-canonicalised
                    (concat property-canonicalised type)))
            (if preferred
                (setq property-canonicalised
                      (concat property-canonicalised ";PREF"))))))
        property-canonicalised))))

(defun org-vcard--canonicalise-tel-property (property-name)
  "Canonicalise a vCard TEL property.

Intended for use by `org-vcard--canonicalise-property-name'.

PROPERTY-NAME must be a string containing a vCard property name."
  (let ((property-canonicalised "TEL")
        (property-type-data '())
        (case-fold-search t))
    (if (string-match "CELL" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("cell"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("cell"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";CELL"))))))
    (if (string-match "FAX" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("fax"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("fax"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";FAX"))))))
    ;; Assume the TEL is for VOICE if other qualifiers
    ;; don't specify otherwise.
    (if (and (not (string-match "CELL" property-name))
             (not (string-match "FAX" property-name))
             (not (string-match "MSG" property-name)))
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("voice"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("voice"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";VOICE"))))))
    (if (string-match "HOME" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("home"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("home"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";HOME"))))))
    (if (string-match "WORK" property-name)
        (cond
         ((string= "4.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("work"))))
         ((string= "3.0" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '("work"))))
         ((string= "2.1" org-vcard-active-version)
          (setq property-type-data
                (append property-type-data '(";WORK"))))))
    `(,property-canonicalised ,property-type-data)))

(defun org-vcard--check-contacts-styles ()
  "Check integrity of `org-vcard-contacts-styles'."
  (let ((styles '()))
    (dolist (style org-vcard--styles-functions)
      (if (not (member (car style) styles))
          (setq styles (append styles `(,(car style))))
        (error
         (concat
          "Style '"
          (cadr style)
          "' appears more than once in org-vcards-contacts-styles")))
      (if (not (functionp (nth 0 (cadr style))))
          (error
           (concat
            "Style '"
            (car style)
            "' has an invalid export function")))
      (if (not (functionp (nth 1 (cadr style))))
          (error
           (concat
            "Style '"
            (car style)
            "' has an invalid import function"))))))

(defun org-vcard--create-styles-functions ()
  "Create a data structure for use by `org-vcard-styles-functions'."
  (let ((the-list) '())
    (dolist (style-dir (org-vcard--styles-dirs))
      (dolist (style (directory-files style-dir))
        (if (and
             (not (string= "." (file-name-nondirectory style)))
             (not (string= ".." (file-name-nondirectory style))))
            (progn
              (load
               (concat
                (file-name-as-directory (concat style-dir style))
                "functions.el"))
              (add-to-list
               'the-list
               `(,(file-name-nondirectory style)
                 ,(list
                   (intern
                    (concat
                     "org-vcard-export-from-"
                     (file-name-nondirectory style)))
                   (intern
                    (concat
                     "org-vcard-import-to-"
                     (file-name-nondirectory style))))))))))
    (sort the-list
          #'(lambda (a b)
              (if (string< (car a) (car b))
                  t
                nil)))))

(defun org-vcard--create-styles-languages-mappings ()
  "Create a data structure for use by `org-vcard-styles-languages-mappings'."
  (let ((style-mappings '()))
    (dolist (style-dir (org-vcard--styles-dirs))
      (dolist (style
               ;; Reverse the list so the repeated calls to
               ;; add-to-list will produce a lexicographically-sorted
               ;; list.
               (sort (directory-files style-dir)
                     #'(lambda (a b)
                         (if (not (string< a b))
                             t
                           nil))))
        (if (and
             (not (string= "." style))
             (not (string= ".." style)))
            (progn
              (let ((language-mapping '()))
                (dolist
                    (mapping
                     ;; Reverse the list so the repeated calls to add-to-list
                     ;; will produce a lexicographically-sorted list.
                     (sort (directory-files
                            (file-name-as-directory
                             (concat
                              (file-name-as-directory
                               (concat style-dir style))
                              "mappings"))
                            t)
                           #'(lambda (a b)
                               (if (not (string< a b))
                                   t
                                 nil))))
                  (if (and
                       (not
                        (string=
                         "."
                         (file-name-nondirectory mapping)))
                       (not
                        (string=
                         ".."
                         (file-name-nondirectory mapping))))
                      (progn
                        (add-to-list
                         'language-mapping
                         `(,(file-name-nondirectory mapping)
                           ,@(list
                              (car
                               (read-from-string
                                (with-temp-buffer
                                  (insert-file-contents-literally mapping)
                                  (buffer-string))))))))))
                (setq language-mapping (list language-mapping))
                (add-to-list
                 'style-mappings
                 `(,style
                   ,@language-mapping)))))))
    style-mappings))

(defun org-vcard--escape-value-string (characters value)
  "Escape each instance of each character in CHARACTERS.

CHARACTERS must be a list of strings. VALUE is the string to be
escaped."
  (if (member "\134" characters)
      ;; Process backslashes first.
      (setq value
            (replace-regexp-in-string
             "\134\134"
             "\134\134"
             value nil t)))
  (dolist (char characters)
    (if (not (string= "\134" char))
        ;; We're escaping a non-backslash character.
        (setq value
              (replace-regexp-in-string
               char
               (concat "\134" char)
               value nil t))))
  value)

(defun org-vcard--export-line (property value &optional noseparator)
  "Export a line as appropriate for each vCard version.

PROPERTY is the vCard property/type to output, VALUE its value.
If NOSEPARATOR is non-nil, don't output colon to separate PROPERTY
from VALUE."
  (let ((separator ":")
        (property-name
         (progn
           (string-match "^[^;:]+" property)
           (match-string 0 property))))
    (if noseparator
        (setq separator ""))
    (cond
     ((string= org-vcard-active-version "4.0")
      ;; In values, escape commas, semicolons and backslashes.
      ;; End line with U+000D U+000A.
      ;; Output must be UTF-8.
      (encode-coding-string
       (concat
        property
        separator
        (cond
         ((member property-name org-vcard-comma-separated-properties)
          (org-vcard--escape-value-string '(";" "\134") value))
         ((member property-name org-vcard-compound-properties)
          (org-vcard--escape-value-string '("," "\134") value))
         (t
          (org-vcard--escape-value-string '("," ";" "\134") value)))
        "\u000D\u000A")
       'utf-8))
     ((string= org-vcard-active-version "3.0")
      ;; In values, escape commas and semicolons.
      ;; End line with CRLF.
      ;; RFC2426 doesn't seem to mandate an encoding, so output UTF-8.
      (encode-coding-string
       (concat
        property
        separator
        (cond
         ((member property-name org-vcard-comma-separated-properties)
          (org-vcard--escape-value-string '(";" "\134") value))
         ((member property-name org-vcard-compound-properties)
          (org-vcard--escape-value-string '("," "\134") value))
         (t
          (org-vcard--escape-value-string '("," ";" "\134") value)))
        "\015\012")
       'utf-8))
     ((string= org-vcard-active-version "2.1")
      ;; In values, escape semicolons.
      ;; End line with CRLF.
      ;; Output ASCII.
      (concat
       (encode-coding-string property 'us-ascii)
       (unless (or (string= "BEGIN" property)
                   (string= "VERSION" property)
                   (string= "END" property))
         (encode-coding-string
          (concat
           ";CHARSET="
           (car
            (rassoc
             org-vcard-default-vcard-21-character-set
             org-vcard-character-set-mapping)))
          'us-ascii))
       (encode-coding-string separator 'us-ascii)
       (if (not (member property-name org-vcard-compound-properties))
           (encode-coding-string
            (org-vcard--escape-value-string '(";") value)
            org-vcard-default-vcard-21-character-set)
         (encode-coding-string
          value
          org-vcard-default-vcard-21-character-set))
       (encode-coding-string "\015\012" 'us-ascii))))))

(defun org-vcard--set-active-settings ()
  "Set active settings to values of last in-buffer settings.

Fall back to value of custom variables."
  (save-excursion
    (goto-char (point-min))
    (let* ((valid-styles (mapcar 'car org-vcard--styles-functions))
           (valid-languages '("en" "en_AU" "en_US"))
           (valid-versions '("4.0" "3.0" "2.1"))
           (found-keywords '()))
      (while (not (eobp))
        (if (looking-at "^#+")
            (let ((this-line (org-element-keyword-parser nil nil)))
              (when (eq 'keyword (car this-line))
                (cond
                 ((string=
                   "CONTACTS_STYLE"
                   (plist-get (cadr this-line) :key))
                  (if (member (plist-get (cadr this-line) :value) valid-styles)
                      (progn
                        (setq org-vcard-active-style
                              (plist-get (cadr this-line) :value))
                        (setq found-keywords
                              (append found-keywords '("CONTACTS_STYLE"))))
                    (error "Invalid in-buffer setting for CONTACTS_STYLE")))
                 ((string=
                   "CONTACTS_LANGUAGE"
                   (plist-get (cadr this-line) :key))
                  (if (member (plist-get (cadr this-line) :value) valid-languages)
                      (progn
                        (setq org-vcard-active-language
                              (plist-get (cadr this-line) :value))
                        (setq found-keywords
                              (append found-keywords '("CONTACTS_LANGUAGE"))))
                    (error "Invalid in-buffer setting for CONTACTS_LANGUAGE")))
                 ((string=
                   "VCARD_VERSION"
                   (plist-get (cadr this-line) :key))
                  (if (member (plist-get (cadr this-line) :value) valid-versions)
                      (progn
                        (setq org-vcard-active-version
                              (plist-get (cadr this-line) :value))
                        (setq found-keywords
                              (append found-keywords '("VCARD_VERSION"))))
                    (error "Invalid in-buffer setting for VCARD_VERSION")))))))
        (forward-line))
      (cond
       ((not (member "CONTACTS_STYLE" found-keywords))
        (setq org-vcard-active-style org-vcard-default-style))
       ((not (member "CONTACTS_LANGUAGE" found-keywords))
        (setq org-vcard-active-language org-vcard-default-language))
       ((not (member "VCARD_VERSION" found-keywords))
        (setq org-vcard-active-version org-vcard-default-version))))))

(defun org-vcard--transfer-write (direction content destination &optional filename)
  "During import, write CONTENT to DESTINATION.

DIRECTION must be either 'import or 'export. CONTENT must be a string.
DESTINATION must be either \"buffer\" or \"file\"."
  (if (not
       (or
        (eq 'import direction)
        (eq 'export direction)))
      (error "DIRECTION must be either 'import or 'export"))
  (if (not (stringp content))
      (error "Received non-string as CONTENT"))
  (let ((the-buffer nil)
        (direction-string
         (cond
          ((eq 'import direction)
           "Imported")
          ((eq 'export direction)
           "Exported"))))
    (cond
     ((string= "buffer" destination)
      (progn
        (cond
         ((eq 'import direction)
          (if org-vcard-append-to-existing-import-buffer
              (setq the-buffer
                    (get-buffer-create "*org-vcard-import*"))
            (setq the-buffer
                  (generate-new-buffer "*org-vcard-import*"))))
         ((eq 'export direction)
          (if org-vcard-append-to-existing-export-buffer
              (setq the-buffer
                    (get-buffer-create "*org-vcard-export*"))
            (setq the-buffer
                  (generate-new-buffer "*org-vcard-export*")))))
        (set-buffer the-buffer)
        (insert (string-as-multibyte content))
        (message
         (concat
          direction-string
          " contacts data to buffer '"
          (buffer-name the-buffer)
          "'."))))
     ((string= "file" destination)
      (let ((filename
             (if filename
                 filename
               (read-file-name
                "Destination filename? "
                default-directory
                (cond
                 ((eq 'import direction)
                  org-vcard-default-import-file)
                 ((eq 'export direction)
                  org-vcard-default-export-file))
                nil))))
        (with-temp-buffer
          (insert (string-as-multibyte content))
          (when (file-writable-p filename)
            (write-region
             (point-min)
             (point-max)
             filename)))
        (message
         (concat
          direction-string
          " contacts data to file '"
          filename
          "'."))))
     (t
      (error "Invalid DESTINATION type")))))


;;
;; org-vcard-mode menu setup.
;;

(defvar org-vcard--styles-functions (org-vcard--create-styles-functions)
  "Available styles and associated import/export functions.")

(defcustom org-vcard-styles-languages-mappings
  (org-vcard--create-styles-languages-mappings)
  "Details of the available styles and their associated mappings."
  :type '(repeat
          (list string
                (repeat
                 (list string
                       (repeat
                        (list string
                              (repeat (cons string string))))))))
  :group 'org-vcard)

(defun org-vcard--create-org-vcard-mode-menu ()
  "Create or recreate the `org-vcard-mode' menu."
  (easy-menu-define org-vcard-menu org-vcard-mode-keymap
    "Menu bar entry for org-vcard"
    `("Org-vCard"
      ,(let ((export '("Export")))
         (let ((style-list '()))
           (dolist (style
                    (sort (mapcar
                           'car
                           org-vcard-styles-languages-mappings)
                          'string<))
             (setq style-list
                   (list (concat "from " style)))
             (let ((language-list '()))
               (dolist (language
                        (sort (mapcar
                               'car
                               (cadr
                                (assoc
                                 style
                                 org-vcard-styles-languages-mappings)))
                              'string<))
                 (setq language-list (list language))
                 (let ((version-list '()))
                   (dolist (version
                            (sort (mapcar
                                   'car
                                   (cadr
                                    (assoc
                                     language
                                     (cadr
                                      (assoc
                                       style
                                       org-vcard-styles-languages-mappings)))))
                                  'string<))
                     (setq version-list
                           (append
                            version-list
                            (list
                             (vector
                              (concat "to vCard " version)
                              `(org-vcard-export-via-menu
                                ,style
                                ,language
                                ,version)
                              t)))))
                   (setq language-list
                         (append language-list version-list)))
                 (setq style-list
                       (append style-list `(,language-list)))))
             (setq export
                   (append export `(,style-list)))))
         export)
      ,(let ((import '("Import")))
         (let ((style-list '()))
           (dolist (style
                    (sort (mapcar
                           'car
                           org-vcard-styles-languages-mappings)
                          'string<))
             (setq style-list
                   (list (concat "to " style)))
             (let ((language-list '()))
               (dolist (language
                        (sort (mapcar
                               'car
                               (cadr
                                (assoc
                                 style
                                 org-vcard-styles-languages-mappings)))
                              'string<))
                 (setq language-list (list language))
                 (let ((version-list '()))
                   (dolist (version
                            (sort (mapcar
                                   'car
                                   (cadr
                                    (assoc
                                     language
                                     (cadr
                                      (assoc
                                       style
                                       org-vcard-styles-languages-mappings)))))
                                  'string<))
                     (setq version-list
                           (append version-list
                                   (list
                                    (vector
                                     (concat "from vCard " version)
                                     `(org-vcard-import-via-menu
                                       ,style
                                       ,language
                                       ,version)
                                     t)))))
                   (setq language-list (append language-list version-list)))
                 (setq style-list (append style-list `(,language-list)))))
             (setq import (append import `(,style-list)))))
         import)
      ["Customize" (customize-group 'org-vcard) t])))
(org-vcard--create-org-vcard-mode-menu)


;;
;; Additional user-customisable variables.
;;

(defgroup org-vcard nil
  "vCard support for Org mode."
  :group 'org
  :prefix "org-vcard-")

(defcustom org-vcard-append-to-existing-export-buffer t
  "Whether export should append to an existing export buffer.

If not, create a new export buffer per export."
  :type 'boolean
  :group 'org-vcard)

(defcustom org-vcard-append-to-existing-import-buffer t
  "Whether import should append to an existing import buffer.

If not, create a new import buffer per import."
  :type 'boolean
  :group 'org-vcard)

(defcustom org-vcard-character-set-mapping
  '(("Big5" . big5)
    ("EUC-JP" . euc-jp)
    ("EUC-KR" . euc-kr)
    ("GB2312" . gb2312)
    ("ISO-2022-JP" . iso-2022-jp)
    ("ISO-2022-JP-2" . iso-2022-jp-2)
    ("ISO-2022-KR" . iso-2022-kr)
    ("ISO-8859-1" . iso-8859-1)
    ("ISO-8859-2" . iso-8859-2)
    ("ISO-8859-3" . iso-8859-3)
    ("ISO-8859-4" . iso-8859-4)
    ("ISO-8859-5" . iso-8859-5)
    ("ISO-8859-6" . iso-8859-6)
    ("ISO-8859-6-E" . iso-8859-6-e)
    ("ISO-8859-6-I" . iso-8859-6-i)
    ("ISO-8859-7" . iso-8859-7)
    ("ISO-8859-8" . iso-8859-8)
    ("ISO-8859-8-E" . iso-8859-8-e)
    ("ISO-8859-8-I" . iso-8859-8-i)
    ("ISO-8859-9" . iso-8859-9)
    ("ISO-8859-10" . iso-8859-10)
    ("KOI8-R" . koi8-r)
    ("Shift_JIS" . shift_jis)
    ("US-ASCII" . us-ascii)
    ("UTF-8" . utf-8)
    ("UTF-16" . utf-16))
  "Alist mapping IANA MIME names for character sets to Emacs coding systems.

Derived from:
http://www.iana.org/assignments/character-sets/character-sets.xhtml"
  :type '(repeat (cons string symbol))
  :group 'org-vcard)

(defcustom org-vcard-default-export-file "~/org-vcard-export.vcf"
  "Default file to which to export."
  :type 'file
  :group 'org-vcard)

(defcustom org-vcard-default-import-file "~/org-vcard-import.vcf"
  "Default file from which to import."
  :type 'file
  :group 'org-vcard)

;; The in-buffer setting #+CONTACT_LANGUAGE.
(defcustom org-vcard-default-language "en"
  "Default language to use.

Initially set to \"en\"."
  :type 'string
  :group 'org-vcard)

(defcustom org-vcard-default-property-for-heading "FN"
  "vCard property whose value to use for a contact's Org heading."
  :type '(radio (const :tag "FN" "FN")
                (const :tag "N" "N"))
  :group 'org-vcard)

;; The in-buffer setting #+CONTACT_STYLE.
(defcustom org-vcard-default-style "flat"
  "Default contact style to use.

Initially set to \"flat\"."
  :type 'string
  :group 'org-vcard)

(defcustom org-vcard-default-vcard-21-character-set 'us-ascii
  "CHARSET modifier for all vCard properties when exporting to vCard 2.1."
  :type `(radio ,@(mapcar #'(lambda (entry)
                              `(const :tag ,(car entry) ,(cdr entry)))
                          org-vcard-character-set-mapping))
  :group 'org-vcard)

;; The in-buffer setting #+VCARD_VERSION;
;; can be "4.0", "3.0" or "2.1".
(defcustom org-vcard-default-version "4.0"
  "Default version of the vCard standard to use.

Initially set to 4.0."
  :type '(radio (const "4.0") (const "3.0") (const "2.1"))
  :group 'org-vcard)

(defcustom org-vcard-include-import-unknowns nil
  "Whether to import vCard properties not listed in the mapping being used."
  :type 'boolean
  :group 'org-vcard)

(defcustom org-vcard-remove-external-semicolons nil
  "Whether import should remove leading/trailing semicolons from compound values.

NB! Since the components of compound values are positional,
removing such semicolons will change the meaning of the value
if/when it is subsequently exported to vCard. If in doubt, leave
this set to nil."
  :type 'boolean
  :group 'org-vcard)

(defvar org-vcard-active-style ""
  "The currently-active contacts style.")

(defvar org-vcard-active-language ""
  "The currently-active language.")

(defvar org-vcard-active-version ""
  "The currently-active version of vCard.")

(defvar org-vcard-comma-separated-properties '("CATEGORIES" "NICKNAME")
  "List of non-compound vCard properties.

Such properties can contain one or more values, separated by commas.")

(defvar org-vcard-compound-properties '("ADR" "N" "ORG")
  "List of compound vCard properties.

Such properties contain a value with multiple components,
each component separated by a semicolon.")


;;
;; User-facing commands for export and import.
;;

;;;###autoload
(defun org-vcard-export (arg)
  "User command to export to vCard. Intended only for interactive use.

With no prefix argument, use the values of `org-vcard-default-version',
`org-vcard-default-language' and `org-vcard-default-style'. With prefix
argument of:

1 : prompt for version;
2 : prompt for language;
3 : prompt for style;
4 : prompt for version, language and style."
  (interactive "P")
  (let ((source "")
        (destination "")
        (version org-vcard-default-version)
        (language org-vcard-default-language)
        (style org-vcard-default-style))
    (setq source
          (completing-read
           "Source: "
           '("buffer" "region" "subtree")))
    (setq destination
          (completing-read
           "Destination: "
           '("file" "buffer")))
    (cond
     ((eq nil arg)
      t)
     ((= 1 arg)
      (setq version
            (completing-read
             "Version: "
             (mapcar
              'car
              (cadr
               (assoc
                language
                (cadr
                 (assoc
                  style
                  org-vcard-styles-languages-mappings)))))
             nil t org-vcard-default-version)))
     ((= 2 arg)
      (setq language
            (completing-read
             "Language: "
             (mapcar
              'car
              (cadr
               (assoc
                style
                org-vcard-styles-languages-mappings)))
             nil t org-vcard-default-language)))
     ((= 3 arg)
      (setq style
            (completing-read
             "Style: "
             (mapcar
              'car
              org-vcard--styles-functions)
             nil t org-vcard-default-style)))
     ((= 4 arg)
      (progn
        (setq version
              (completing-read
               "Version: "
               (mapcar
                'car
                (cadr
                 (assoc
                  language
                  (cadr
                   (assoc
                    style
                    org-vcard-styles-languages-mappings)))))
               nil t org-vcard-default-version))
        (setq language
              (completing-read
               "Language: "
               (mapcar
                'car
                (cadr
                 (assoc
                  style
                  org-vcard-styles-languages-mappings)))
               nil t org-vcard-default-language))
        (setq style
              (completing-read
               "Style: "
               (mapcar
                'car
                org-vcard--styles-functions)
               nil t org-vcard-default-style)))))
    (org-vcard-transfer-helper
     source destination style language version 'export)))

;;;###autoload
(defun org-vcard-export-via-menu (style language version)
  "User command for exporting to vCard via Emacs' menu bar."
  (let ((source nil)
        (destination nil))
    (setq source
          (completing-read
           "Source: "
           '("buffer" "region" "subtree")))
    (setq destination
          (completing-read
           "Destination: "
           '("file" "buffer")))
    (org-vcard-transfer-helper
     source destination style language version 'export)))

;;;###autoload
(defun org-vcard-import (arg)
  "User command to import from vCard. Intended only for interactive use.

With no prefix argument, use the values of `org-vcard-default-version',
`org-vcard-default-language' and `org-vcard-default-style'. With prefix
argument of:

1 : prompt for version;
2 : prompt for language;
3 : prompt for style;
4 : prompt for version, language and style."
  (interactive "P")
  (let ((source "")
        (destination "")
        (version org-vcard-default-version)
        (language org-vcard-default-language)
        (style org-vcard-default-style))
    (setq source
          (completing-read
           "Source: "
           '("file" "buffer" "region")))
    (setq destination
          (completing-read
           "Destination: "
           '("file" "buffer")))
    (cond
     ((eq nil arg)
      t)
     ((= 1 arg)
      (setq version
            (completing-read
             "Version: "
             (mapcar
              'car
              (cadr
               (assoc
                language
                (cadr
                 (assoc
                  style
                  org-vcard-styles-languages-mappings)))))
             nil t org-vcard-default-version)))
     ((= 2 arg)
      (setq language
            (completing-read
             "Language: "
             (mapcar
              'car
              (cadr
               (assoc
                style
                org-vcard-styles-languages-mappings)))
             nil t org-vcard-default-language)))
     ((= 3 arg)
      (setq style
            (completing-read
             "Style: "
             (mapcar
              'car
              org-vcard--styles-functions)
             nil t org-vcard-default-style)))
     ((= 4 arg)
      (progn
        (setq version
              (completing-read
               "Version: "
               (mapcar
                'car
                (cadr
                 (assoc
                  language
                  (cadr
                   (assoc
                    style
                    org-vcard-styles-languages-mappings)))))
               nil t org-vcard-default-version))
        (setq language
              (completing-read
               "Language: "
               (mapcar
                'car
                (cadr
                 (assoc
                  style
                  org-vcard-styles-languages-mappings)))
               nil t org-vcard-default-language))
        (setq style
              (completing-read
               "Style: "
               (mapcar
                'car
                org-vcard--styles-functions)
               nil t org-vcard-default-style)))))
    (org-vcard-transfer-helper
     source destination style language version 'import)))

(declare-function quoted-printable-decode-string "qp")
(defun org-vcard-import-parse (source &optional filename)
  "Read and parse SOURCE and return a list of vCards.

Each vCard is a list of cons cells, each cell containing the vCard property
in the car, and the value of that property in the cdr.

SOURCE must be one of \"file\", \"buffer\" or \"region\"."
  (let ((property "")
        (value "")
        (charset "")
        (encoding "")
        (cards '())
        (current-card '()))
    (cond
     ((string= "file" source)
      (unless filename
        (setq filename
              (read-file-name
               "Source filename? "
               default-directory
               org-vcard-default-import-file
               t))))
     ((string= "region" source)
      (narrow-to-region (region-beginning) (region-end)))
     ((string= "buffer" source)
      t)
     (t
      (error "Invalid source type")))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (goto-char (point-min))
      (setq case-fold-search t)
      (while (re-search-forward "BEGIN:VCARD" (point-max) t)
        (setq current-card '())
        (forward-line)
        (while (not (looking-at "END:VCARD"))
          (re-search-forward "^\\([^:]+\\): *")
          ;; Parse property.
          (setq property (match-string-no-properties 1)
                value ""
                charset (when (string-match ";CHARSET=\\([^;:]+\\)" property)
                          ;; Save the value of the charset.
                          (prog1 (match-string-no-properties 1 property)
                            ;; Remove the charset from the property name.
                            (setq property (replace-match "" nil nil property))))
                charset (cdr
                         (assoc-string
                          charset
                          org-vcard-character-set-mapping
                          :case-fold))
                encoding (when (string-match ";ENCODING=\\([^;:]+\\)" property)
                           ;; Save the value of the encoding.
                           (prog1 (upcase (match-string-no-properties 1 property))
                             ;; Remove the encoding from the property name.
                             (setq property (replace-match "" nil nil property)))))
          ;; Consume value and continuation lines.
          (while
              (progn
                ;; Add the text from the current point to the end of the
                ;; line (minus line ending) to the value.
                (setq value
                      (concat
                       value
                       (string-trim-right
                        (buffer-substring-no-properties
                         (point)
                         (line-end-position))
                        ;; Remove DOS line ending.
                        "[\u000D\015]")))
                ;; Set point to the start of the next continuation line,
                ;; if there is one.
                (or (re-search-forward "^[\u0009\u0020\011\040]"
                                       ;; Don't go past the next line.
                                       (line-end-position 2)
                                       ;; No error and don't move point if failed.
                                       t)
                    (and (string= encoding "QUOTED-PRINTABLE")
                         ;; = at the end of a line encoded with quoted-printable
                         ;; means to continue on to the next line.
                         (re-search-forward "=$" (line-end-position 2) t)
                         ;; Include the line break in the value.
                         (setq value (concat value "\n"))
                         ;; Consume the next line as long as `forward-line'
                         ;; returns non-nil.
                         (forward-line)))))
          (forward-line)
          ;; Deal with possible quoted-printable encoding.
          (when (and
                 encoding
                 (string= encoding "QUOTED-PRINTABLE"))
            (require 'qp) ; Part of GNU Emacs.
            (setq value
                  (decode-coding-string
                   (quoted-printable-decode-string value)
                   (or charset 'utf-8-emacs)
                   :nocopy)))
          ;; Handle CHARSET if necessary.
          (pcase org-vcard-active-version
            ((or "4.0" "3.0")
             ;; vCard 4.0 mandates UTF-8 as the only possible encoding,
             ;; and 3.0 mandates encoding not per-property, but via the
             ;; CHARSET parameter on the containing MIME object. So we
             ;; just ignore the presence and/or value of the CHARSET
             ;; modifier in 4.0 and 3.0 contexts.
             t)
            ("2.1"
             (setq value
                   (string-as-multibyte
                    (encode-coding-string value charset)))))
          (setq property
                (org-vcard--canonicalise-property-name property))
          (setq current-card
                (append current-card (list (cons property value)))))
        (push current-card cards))
      (nreverse cards))))

;;;###autoload
(defun org-vcard-import-via-menu (style language version)
  "User command for importing from vCard via Emacs' menu bar."
  (let ((source nil)
        (destination nil))
    (setq source
          (completing-read
           "Source: "
           '("file" "buffer" "region")))
    (setq destination
          (completing-read
           "Destination: "
           '("file" "buffer")))
    (org-vcard-transfer-helper
     source destination style language version 'import)))

(defun org-vcard-transfer-helper
    (source destination style language version direction)
  "Dispatch export and import requests to the appropriate functions.

Appropriate values for SOURCE and DESTINATION are determined by
the functions called. Appropriate values for STYLE and VERSION are
determined by the contents of the `org-vcard-contacts-styles-mappings'
variable. DIRECTION must be either 'export or 'import."
  (let ((position nil))
    (org-vcard--check-contacts-styles)
    (setq org-vcard-active-style style)
    (setq org-vcard-active-language language)
    (setq org-vcard-active-version version)
    (cond
     ((eq 'export direction)
      (setq position 0))
     ((eq 'import direction)
      (setq position 1))
     (t
      (error "Invalid direction type")))
    (dolist (style org-vcard--styles-functions)
      (if (string= (car style) org-vcard-active-style)
          (funcall (nth position (cadr style)) source destination)))))


;;
;; User-facing general commands.
;;

(defun org-vcard-reload-styles ()
  "Reload the styles in the org-vcard 'styles' directory."
  (interactive)
  (setq org-vcard--styles-functions
        (org-vcard--create-styles-functions))
  (setq org-vcard-styles-languages-mappings
        (org-vcard--create-styles-languages-mappings))
  (org-vcard-create-org-vcard-mode-menu))


;;
;; org-vcard-mode setup
;;

(define-minor-mode org-vcard-mode
  "Toggle org-vcard mode.

Interactively, with no argument, toggle the mode; with a positive prefix
argument, enable the mode; with any other prefix argument, disable the mode.

When called from Lisp, argument omitted or nil enables the mode, and
 `toggle' toggles the mode.

Enabling org-vcard mode will add an Org-vCard entry to Emacs' menu bar."
  nil                    ; The initial value.
  nil                    ; The indicator for the mode line.
  org-vcard-mode-keymap  ; The minor mode bindings.
  :group 'org-vcard)


;; --

(provide 'org-vcard)

;;; org-vcard.el ends here
