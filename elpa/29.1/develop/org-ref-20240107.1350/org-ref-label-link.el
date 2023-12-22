;;; org-ref-label-link.el --- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;

;;; Code:


(defface org-ref-label-face
  `((t (:inherit org-link :foreground "dark magenta")))
  "Color for ref links in `org-ref'."
  :group 'org-ref-faces)


;;** label link (maybe deprecated)


(org-link-set-parameters
 "label"
 :export (lambda (path _desc format)
	   (cond
	    ((eq format 'latex)
	     (format "\\label{%s}" path))))
 :face 'org-ref-label-face
 :help-echo "A label")


;;;###autoload
(defun org-ref-insert-label-link ()
  "Insert a new label with completion.
The completion helps ensure you use a unique label."
  (interactive)
  (let* ((known-labels (mapcar 'car (org-ref-get-labels)))
	 (new-label (completing-read "Label: " known-labels nil)))
    (when (member new-label known-labels)
      (warn "Inserting duplicate label"))
    (insert (format "label:%s" new-label))))


(provide 'org-ref-label-link)

;;; org-ref-label-link.el ends here
