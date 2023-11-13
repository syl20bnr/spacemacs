;;; diredy.el --- Flexible grouping for files in Dired buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

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

;; This library provides a command, `diredy', that rearranges a Dired
;; buffer into a hierarchy by file size and MIME type.

;;; Code:

;;;; Requirements

(require 'taxy)
(require 'taxy-magit-section)

(require 'dired)
(require 'mailcap)

;;;; Functions

(eval-and-compile
  (defun diredy--machine-size (size)
    "Return number of bytes represented by human-readable SIZE."
    (declare (pure t) (side-effect-free t))
    (let ((case-fold-search t))
      (string-match (rx bos (group (1+ digit)) (0+ space)
                        (group (repeat 1 (any "kmg")))
                        (optional (optional "i") "b"))
                    size)
      (* (pcase (match-string 2 size)
           ((or "k" "K") 1024)
           ((or "m" "M") (* 1024 1024))
           ((or "g" "G") (* 1024 1024 1024)))
         (string-to-number (match-string 1 size))))))

;;;; Variables

(defvar diredy-taxy
  (cl-macrolet ((label
                 (prefix size)
                 (propertize (concat prefix " " size)
                             :machine-size (number-to-string (diredy--machine-size size)))))
    (cl-labels ((file-name
                 (string) (let* ((start (text-property-not-all 0 (length string) 'dired-filename nil string))
                                 (end (text-property-any start (length string) 'dired-filename nil string)))
                            (substring string start end)))
                (file-extension
                 (filename) (file-name-extension filename))
                (file-type (string)
                           (when-let ((extension (file-extension (file-name string))))
                             (mailcap-extension-to-mime extension)))
                (file-size
                 (filename) (file-attribute-size (file-attributes filename)))
                (file-size-group
                 (string) (pcase (file-size (file-name string))
                            ('nil "No size")
                            ((pred (<= (diredy--machine-size "1G")))
                             (label ">=" "1G"))
                            ((pred (<= (diredy--machine-size "100M")))
                             (label ">=" "100M"))
                            ((pred (<= (diredy--machine-size "10M")))
                             (label ">=" "10M"))
                            ((pred (<= (diredy--machine-size "1M")))
                             (label ">=" "1M"))
                            ((pred (<= (diredy--machine-size "100K")))
                             (label ">=" "100K"))
                            ((pred (<= (diredy--machine-size "1K")))
                             (label ">=" "1K"))
                            (_ (label "<" "1K"))))
                (file-dir? (string) (if (file-directory-p (file-name string))
                                        "Directory" "File"))
	        (make-fn (&rest args)
		         (apply #'make-taxy-magit-section
			        :make #'make-fn
			        :level-indent 1
			        :item-indent 2
                                :format-fn #'identity
			        args)))
      (make-fn
       :name "Diredy"
       :make #'make-fn
       :take (apply-partially #'taxy-take-keyed (list #'file-dir? #'file-size-group #'file-type))))))

(defvar dired-mode)

;;;; Customization


;;;; Commands

(defun diredy ()
  "Apply grouping to current Dired buffer."
  (interactive)
  (cl-assert (eq 'dired-mode major-mode))
  (use-local-map (make-composed-keymap (list dired-mode-map magit-section-mode-map)))
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let* ((lines (save-excursion
		    (cl-loop until (eobp)
			     collect (string-trim (buffer-substring (point-at-bol) (point-at-eol)))
			     do (forward-line 1))))
	   (filled-taxy (thread-last diredy-taxy
			             taxy-emptied
			             (taxy-fill lines)
			             (taxy-mapc* (lambda (taxy)
					           (setf (taxy-taxys taxy)
					                 (cl-sort (taxy-taxys taxy)
                                                                  (lambda (a b)
                                                                    (string< (or (get-text-property 0 :machine-size a) a)
                                                                             (or (get-text-property 0 :machine-size b) b)))
						                  :key #'taxy-name))))))
	   (inhibit-read-only t))
      (delete-region (point) (point-max))
      (taxy-magit-section-insert filled-taxy :items 'first
	:initial-depth 0 :blank-between-depth 1))))

;;;; Footer

(provide 'diredy)

;;; diredy.el ends here
