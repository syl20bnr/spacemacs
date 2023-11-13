;;; org-ref-ivy.el --- org-ref with ivy completion -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 1.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((org-ref "0") (ivy-bibtex "0"))

;; This file is not currently part of GNU Emacs.

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
;;
;; This customizes org-ref specifically around using ivy and ivy-bibtex. The
;; citation selection looks like `ivy-bibtex' but it is a customized ivy
;; function with customized actions.

(require 'org-ref-core)
(require 'ivy-bibtex)


;; all these functions are defined in ivy-bibtex, but not in a variable. Here
;; you can set them as you see fit.
(defcustom org-ref-citation-alternate-insert-actions
  '(("p" ivy-bibtex-open-pdf "Open PDF file (if present)")
    ("u" ivy-bibtex-open-url-or-doi "Open URL or DOI in browser")
    ;; this insert-citation only inserts an org-ref cite.
    ;; ("c" ivy-bibtex-insert-citation "Insert citation")
    ("R" ivy-bibtex-insert-reference "Insert formatted reference")
    ("k" ivy-bibtex-insert-key "Insert BibTeX key")
    ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry")
    ("a" ivy-bibtex-add-PDF-attachment "Attach PDF to email")
    ("e" ivy-bibtex-edit-notes "Edit notes")
    ("s" ivy-bibtex-show-entry "Show entry")
    ("l" ivy-bibtex-add-pdf-to-library "Add PDF to library")
    ("h" (lambda (candidate)
	   (org-insert-heading)
	   (insert (bibtex-completion-apa-format-reference
		    (cdr (assoc "=key=" candidate)))
		   " "
		   (format "cite:&%s" (cdr (assoc "=key=" candidate)))))
     "Insert org-heading")
    ("m" (lambda (candidate)
	   (insert (bibtex-completion-apa-format-reference
		    (cdr (assoc "=key=" candidate)))))
     "Insert formatted citation")
    ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options")
    ("d" (lambda (_)
	   "Add a bibtex entry from doi and insert cite to it at point."
	   (funcall-interactively 'doi-utils-add-bibtex-entry-from-doi
				  (read-string
				   "DOI: "
				   ;; now set initial input
				   (doi-utils-maybe-doi-from-region-or-current-kill)))
	   (org-ref-insert-cite-key
	    (current-kill 0 t)))
     "Insert from a DOI"))
  "Alternate actions to do instead of inserting."
  :type '(list (repeat (string function string)))
  :group 'org-ref)


;; This is modified from ivy-bibtex which had " " instead of "" which breaks marking.
(defun org-ref-ivy-bibtex-display-transformer (candidate)
  "Prepare bib entry CANDIDATE for display."
  (let* ((width (- (frame-width) 2))
	 (idx (get-text-property 1 'idx candidate))
	 (entry (cdr (nth idx (ivy-state-collection ivy-last)))))
    (s-concat (if (s-starts-with-p ivy-mark-prefix candidate) ivy-mark-prefix "")
	      (bibtex-completion-format-entry entry width))))


(ivy-configure 'org-ref-cite-insert-ivy :display-transformer-fn 'org-ref-ivy-bibtex-display-transformer)


(defun org-ref-cite-multi-insert-ivy (candidates)
  "A multi-action function to insert CANDIDATES."
  (with-ivy-window
    (org-ref-insert-cite-keys
     (mapcar (lambda (entry)
	       (cdr (assoc "=key=" (cdr entry))))
	     candidates)
     ivy-current-prefix-arg)))


(defun org-ref-cite-insert-ivy ()
  "Function for inserting a citation."
  (interactive)
  ;; Set this in the function so it is updated if you change the functions while
  ;; writing
  (ivy-set-actions
   'org-ref-cite-insert-ivy
   org-ref-citation-alternate-insert-actions)

  ;; This initializes bibtex if the variable is not defined.
  (unless bibtex-completion-display-formats-internal
    (bibtex-completion-init))

  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (candidates (bibtex-completion-candidates)))
    (ivy-read "org-ref-ivy BibTeX entries: " candidates
	      :preselect (ivy-thing-at-point)
	      :multi-action #'org-ref-cite-multi-insert-ivy
	      :action '(1
			("o" (lambda (candidate)
			       (org-ref-insert-cite-key
				(cdr (assoc "=key=" (cdr candidate)))
				ivy-current-prefix-arg))
			 "insert")
			("r" (lambda (candidate)
			       (let* ((object (org-element-context))
				      (type (org-element-property :type object))
				      (begin (org-element-property :begin object))
				      (end (org-element-property :end object))
				      (link-string (org-element-property :path object))
				      (data (org-ref-parse-cite-path link-string))
				      (references (plist-get data :references))
				      (cp (point))
				      (key)
				      keys i)
				 ;;   We only want this to work on citation links
				 (when (assoc type org-ref-cite-types)
				   (setq key (org-ref-get-bibtex-key-under-cursor))
				   (if (null key)
				       ;; delete the whole cite
				       (cl--set-buffer-substring begin end "")
				     (setq i (seq-position
					      references key
					      (lambda (el key)
						(string= key
							 (plist-get el :key)))))
				     (setf (plist-get (nth i references) :key)
					   (cdr (assoc "=key=" (cdr candidate))))
				     (setq data (plist-put data :references references))
				     (save-excursion
				       (goto-char begin)
				       (re-search-forward link-string)
				       (replace-match (org-ref-interpret-cite-data data)))
				     (goto-char cp)))))
			 "Replace key at point"))
	      :caller 'org-ref-cite-insert-ivy)))


(setq org-ref-insert-cite-function 'org-ref-cite-insert-ivy)


(provide 'org-ref-ivy)

;;; org-ref-ivy.el ends here
