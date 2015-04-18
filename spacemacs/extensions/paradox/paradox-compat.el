;;; paradox-compat.el --- Compatibility functions for using paradox with emacs < 24.4

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: https://github.com/Bruce-Connor/paradox
;; Version: 1.0.1
;; Keywords: package packages mode-line
;; Package-Requires: ((emacs "24.1") (tabulated-list "1.0") (package "1.0") (json "1.4"))
;; Prefix: paradox 
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Code:

(eval-when-compile (require 'package))
;; (require 'json)
;; (require 'cl)

(defun paradox--print-info-compat (pkg)
  "Return a package entry suitable for `tabulated-list-entries' (package-1.0 version).
PKG has the form ((PACKAGE . VERSION) STATUS DOC).
Return (KEY [NAME VERSION STATUS DOC]), where KEY is the
identifier (NAME . VERSION-LIST)."
  (let* ((package (caar pkg))
         (version (cdr (car pkg)))
         (status  (nth 1 pkg))
         (doc (or (nth 2 pkg) ""))
         (face (or (cdr (assoc-string status paradox-status-face-alist))
                   'font-lock-warning-face))
         (url (paradox--package-homepage package))
         (name (symbol-name package))
         (name-length (length name))
         (button-length (length paradox-homepage-button-string)))
    (paradox--incf status)
    (list (cons package version)
          (vconcat
           (append (list (concat 
                          (propertize name
                                      'face 'paradox-name-face
                                      'button t
                                      'follow-link t
                                      'package-symbol package
                                      'help-echo (format "Package: %s" name)
                                      'action 'package-menu-describe-package)
                          (if (and paradox-use-homepage-buttons url
                                   (< (+ name-length button-length) paradox-column-width-package))
                              (concat
                               (make-string (- paradox-column-width-package name-length button-length) ?\s)
                               (propertize paradox-homepage-button-string
                                           'face 'paradox-homepage-button-face
                                           'mouse-face 'custom-button-mouse
                                           'help-echo (format "Visit %s" url)
                                           'button t
                                           'follow-link t
                                           'action 'paradox-menu-visit-homepage))
                            ""))
                         (propertize (package-version-join version)
                                     'font-lock-face face)
                         (propertize status 'font-lock-face face))
                   (paradox--count-print package)
                   (list 
                    (propertize (concat desc-prefix doc desc-suffix)
                                'font-lock-face
                                (if (> paradox-lines-per-entry 1)
                                    'paradox-description-face-multiline
                                  'paradox-description-face))))))))

(defun paradox--print-entry-compat (id cols)
  "Printer used by `paradox-menu-mode'.
Just like default printer, except columns are printed with
`paradox--print-col-compat'."
  (let ((beg   (point))
	(x     (max tabulated-list-padding 0))
	(ncols (length tabulated-list-format))
	(inhibit-read-only t))
    (if (> tabulated-list-padding 0)
	(insert (make-string x ?\s)))
    (dotimes (n ncols)
      (setq x (paradox--print-col-compat n (aref cols n) x)))
    (insert ?\n)
    (put-text-property beg (point) 'tabulated-list-id id)
    (put-text-property beg (point) 'tabulated-list-entry cols)))

(defun paradox--print-col-compat (n col-desc x)
  "Insert a specified Tabulated List entry at point.
N is the column number, COL-DESC is a column descriptor \(see
`tabulated-list-entries'), and X is the column number at point.
Return the column number after insertion.

This is like `tabulated-list-print-col', except the help-echo
property is respected."
  ;; TODO: don't truncate to `width' if the next column is align-right
  ;; and has some space left.
  (let* ((format    (aref tabulated-list-format n))
	 (name      (nth 0 format))
	 (width     (nth 1 format))
	 (props     (nthcdr 3 format))
	 (pad-right (or (plist-get props :pad-right) 1))
         (right-align (plist-get props :right-align))
	 (label     (if (stringp col-desc) col-desc (car col-desc)))
         (label-width (string-width label))
	 (help-echo (concat (car format) ": " label))
	 (opoint (point))
	 (not-last-col (< (1+ n) (length tabulated-list-format))))
    ;; Truncate labels if necessary (except last column).
    (and not-last-col
	 (> label-width width)
	 (setq label (truncate-string-to-width label width nil nil t)
               label-width width))
    (setq label (bidi-string-mark-left-to-right label))
    (when (and right-align (> width label-width))
      (let ((shift (- width label-width)))
        (insert (propertize (make-string shift ?\s)
                            'display `(space :align-to ,(+ x shift))))
        (setq width (- width shift))
        (setq x (+ x shift))))
    (if (stringp col-desc)
	(insert (if (get-text-property 0 'help-echo label)
		    label
		  (propertize label 'help-echo help-echo)))
      (apply 'insert-text-button label (cdr col-desc)))
    (let ((next-x (+ x pad-right width)))
      ;; No need to append any spaces if this is the last column.
      (when not-last-col
        (when (> pad-right 0) (insert (make-string pad-right ?\s)))
        (insert (propertize
                 (make-string (- next-x x label-width pad-right) ?\s)
                 'display `(space :align-to ,next-x))))
      (put-text-property opoint (point) 'tabulated-list-column-name name)
      next-x)))

(defun paradox--package-homepage (pkg)
  "PKG is just the symbol that identifies the package."
  (let ((extras (elt (cdr-safe (assoc pkg package-archive-contents)) 4)))
    (and (listp extras) (cdr-safe (assoc :url extras)))))

(defmacro package--push-compat (package desc status listname)
  "Convenience macro for `package-menu--generate'.
If the alist stored in the symbol LISTNAME lacks an entry for a
package PACKAGE with descriptor DESC, add one.  The alist is
keyed with cons cells (PACKAGE . VERSION-LIST), where PACKAGE is
a symbol and VERSION-LIST is a version list."
  `(let* ((version (package-desc-vers ,desc))
          (key (cons ,package version)))
     (unless (assoc key ,listname)
       (push (list key ,status (package-desc-doc ,desc)) ,listname))))

(defun paradox-menu--refresh (packages &optional keywords)
  ;; Construct list of ((PACKAGE . VERSION) STATUS DESCRIPTION).
  (let (info-list name)
    ;; Installed packages:
    (dolist (elt package-alist)
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
        (package--push-compat name (cdr elt)
                       (if (stringp (cadr (assq name package-load-list)))
                           "held" "installed")
                       info-list)))

    ;; Built-in packages:
    (dolist (elt package--builtins)
      (setq name (car elt))
      (when (and (not (eq name 'emacs)) ; Hide the `emacs' package.
                 (or (eq packages t) (memq name packages)))
        (package--push-compat name (cdr elt) "built-in" info-list)))

    ;; Available and disabled packages:
    (dolist (elt package-archive-contents)
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
        (let ((hold (assq name package-load-list)))
          (package--push-compat name (cdr elt)
                         (cond
                          ((and hold (null (cadr hold))) "disabled")
                          ((memq name package-menu--new-package-list) "new")
                          (t "available"))
                         info-list))))

    ;; Obsolete packages:
    (dolist (elt package-obsolete-alist)
      (dolist (inner-elt (cdr elt))
        (when (or (eq packages t) (memq (car elt) packages))
          (package--push-compat (car elt) (cdr inner-elt) "obsolete" info-list))))

    ;; Print the result.
    (setq tabulated-list-entries (mapcar 'package-menu--print-info info-list))
    (tabulated-list-print remember-pos)))

(defun paradox--get-or-return-package (pkg)
  (if (or (markerp pkg) (null pkg))
      (if (derived-mode-p 'package-menu-mode)
          (car (tabulated-list-get-id))
        (error "Not in Package Menu."))
    pkg))

(provide 'paradox-compat)
;;; paradox-compat.el ends here.
