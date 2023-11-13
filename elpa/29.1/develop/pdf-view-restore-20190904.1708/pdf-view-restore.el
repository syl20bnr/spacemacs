;;; pdf-view-restore.el --- Support for opening last known pdf position in pdfview mode -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Kevin Kim <kevinkim1991@gmail.com>

;; Author: Kevin Kim <kevinkim1991@gmail.com>
;; URL: https://github.com/007kevin/pdf-view-restore
;; Keywords: files convenience
;; Version: 0.1
;; Package-Requires: ((pdf-tools "0.90") (emacs "26.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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
;; Support for saving and opening last known pdf position in pdfview mode.
;; Information  will be saved relative to the pdf being viewed so ensure
;; `pdf-view-restore-filename' is in the same directory as the viewing pdf.
;;
;; To enable, add the following:
;;   (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)

;;; Code:

(require 'pdf-view)

(defcustom pdf-view-restore-filename ".pdf-view-restore"
  "Filename to save the last known pdf position."
  :group 'pdf-view
  :type 'string)

(defcustom use-file-base-name-flag t
  "Flag to control whether to use only the base name of file or to use full file path. Defaults to using base name.
  
  pdf-view-restore uses this setting to determine what to use as the key to search saved files.
  If set to t, only the base name is used. This will allow moving files while saving sync information.
  However, that may cause conflicts if you have many files with the same name. 
  Setting to nil will use the full path but then you may lose information if you move files."
  :group 'pdf-view
  :type 'boolean)

;;;###autoload
(define-minor-mode pdf-view-restore-mode
  "Automatically restore last known pdf position"
  :global nil
  (if (not pdf-view-restore-mode)
      (remove-hook 'pdf-view-after-change-page-hook 'pdf-view-restore-save)
    (pdf-view-restore)
    (add-hook 'pdf-view-after-change-page-hook 'pdf-view-restore-save nil t)))

(defun pdf-view-restore ()
  "Restore page."
  (when (derived-mode-p 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (let ((page (pdf-view-restore-get-page)))
      (when page (pdf-view-goto-page page)))))

(defun pdf-view-restore-save ()
  "Save restore information."
  (when (derived-mode-p 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (let ((page (pdf-view-current-page)))   
      (pdf-view-restore-set-page page))))

(defun pdf-view-restore-get-page ()
  "Return restore page."
  (let* ((alist (pdf-view-restore-unserialize))
         (key (pdf-view-restore-key))
         (val (cdr (assoc key alist))))
    val))

(defun pdf-view-restore-set-page (page)
  "Save restore PAGE."
  (let ((alist (pdf-view-restore-unserialize))
        (key (pdf-view-restore-key)))
    (setf (alist-get key alist nil nil 'equal) page)
    (pdf-view-restore-serialize alist)))

(defun pdf-view-restore-key ()
  "Key for storing data is based on filename."
  (if use-file-base-name-flag
   (file-name-base buffer-file-name)
   buffer-file-name))

;;; Serialization
(defun pdf-view-restore-serialize (data)
  "Serialize DATA to `pdf-view-restore-filename'.
The saved data can be restored with `pdf-view-restore-unserialize'."
  (when (file-writable-p pdf-view-restore-filename)
    (with-temp-file pdf-view-restore-filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun pdf-view-restore-unserialize ()
  "Read data serialized by `pdf-view-restore-serialize' from `pdf-view-restore-filename'."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p pdf-view-restore-filename)
      (with-temp-buffer
        (insert-file-contents pdf-view-restore-filename)
        ;; this will blow up if the contents of the file aren't
        ;; lisp data structures
        (read (buffer-string))))))


(provide 'pdf-view-restore)
;;; pdf-view-restore.el ends here
