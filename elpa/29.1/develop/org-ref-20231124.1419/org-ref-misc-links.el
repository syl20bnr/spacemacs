;;; org-ref-misc-links.el --- Miscellaneous links -*- lexical-binding: t; -*-
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
;; * Miscellaneous links


;;; Commentary:
;;

;;; Code:

;;** List of figures

;;;###autoload
(defun org-ref-list-of-figures (&optional _arg)
  "Generate buffer with list of figures in them.
ARG does nothing.
Ignore figures in COMMENTED sections."
  (interactive)
  (save-excursion
    (widen)
    (let* ((c-b (buffer-name))
	   (counter 0)
	   (list-of-figures
	    (org-element-map (org-ref-parse-buffer) 'link
	      (lambda (link)
		"create a link for to the figure"
		(when
		    (and (string= (org-element-property :type link) "file")
			 (string-match-p
			  "[^.]*\\.\\(png\\|jpg\\|eps\\|pdf\\|svg\\)$"
			  (org-element-property :path link))
			 ;; ignore commented sections
			 (save-excursion
			   (goto-char (org-element-property :begin link))
			   (not (or (org-in-commented-heading-p)
				    (org-at-comment-p)
				    (-intersection (org-get-tags) org-export-exclude-tags)))))
		  (cl-incf counter)

		  (let* ((start (org-element-property :begin link))
			 (linenum (progn (goto-char start) (line-number-at-pos)))
			 (fname (org-element-property :path link))
			 (parent (car (cdr
				       (org-element-property :parent link))))
			 (caption (cl-caaar (plist-get parent :caption)))
			 (name (plist-get parent :name)))

		    (if caption
			(format "[[file:%s::%s][Figure %s:]] %s\n" c-b linenum counter caption)
		      ;; if it has no caption, try the name
		      ;; if it has no name, use the file name
		      (cond (name
			     (format "[[file:%s::%s][Figure %s:]] %s\n" c-b linenum counter name))
			    (fname
			     (format "[[file:%s::%s][Figure %s:]] %s\n"
				     c-b linenum counter fname))))))))))
      (switch-to-buffer "*List of Figures*")
      (setq buffer-read-only nil)
      (org-mode)
      (erase-buffer)
      (insert (mapconcat 'identity list-of-figures ""))
      (goto-char (point-min))
      ;; open links in the same window
      (setq-local org-link-frame-setup
		  '((file . find-file)))
      (setq buffer-read-only t)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))))


(org-link-set-parameters "list-of-figures"
			 :follow #'org-ref-list-of-figures
			 :export (lambda (_path _desc format)
				   (cond
				    ((eq format 'latex)
				     (format "\\listoffigures")))))

;;** List of tables
;;;###autoload
(defun org-ref-list-of-tables (&optional _arg)
  "Generate a buffer with a list of tables.
ARG does nothing."
  (interactive)
  (save-excursion
    (widen)
    (let* ((c-b (buffer-name))
           (counter 0)
           (list-of-tables
            (org-element-map (org-ref-parse-buffer 'element) 'table
              (lambda (table)
                "create a link for to the table"
		(save-excursion
		  (goto-char (org-element-property :begin table))
		  (when
		      ;; ignore commented sections
		      (not (or (org-in-commented-heading-p)
			       (-intersection (org-get-tags) org-export-exclude-tags)
			       (looking-at "#\\+RESULTS:") ))
		    (cl-incf counter)
		    (let* ((start (org-element-property :begin table))
			   (linenum (progn (goto-char start) (line-number-at-pos)))
			   (caption (cl-caaar (org-element-property :caption table)))
			   (name (org-element-property :name table)))
		      (if caption
			  (format "[[file:%s::%s][Table %s:]] %s\n" c-b linenum counter caption)
			;; if it has no caption, try the name
			;; if it has no name, use generic name
			(cond (name
			       (format "[[file:%s::%s][Table %s:]] %s\n"
				       c-b linenum counter name))
			      (t
			       (format "[[file:%s::%s][Table %s:]] No caption\n"
				       c-b linenum counter)))))))))))
      (switch-to-buffer "*List of Tables*")
      (setq buffer-read-only nil)
      (org-mode)
      (erase-buffer)
      (insert (mapconcat 'identity list-of-tables ""))
      (goto-char (point-min))
      ;; open links in the same window
      (setq-local org-link-frame-setup
		  '((file . find-file)))
      (setq buffer-read-only t)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" #'(lambda () (interactive) (kill-buffer))))))


(org-link-set-parameters "list-of-tables"
			 :follow #'org-ref-list-of-tables
			 :export (lambda (_path _desc format)
				   (cond
				    ((eq format 'latex)
				     (format "\\listoftables")))))


;;* Index link
;;
;; You need these lines in the header
;; 
;; #+latex_header: \usepackage{makeidx}
;; #+latex_header: \makeindex
(org-link-set-parameters "index"
			 :follow (lambda (path)
				   (occur path))
			 :export (lambda (path _desc format)
				   (cond
				    ((eq format 'latex)
				     (format "\\index{%s}" path)))))


;; this will generate a temporary index of entries in the file when clicked on.
;;;###autoload
(defun org-ref-index (&optional _path)
  "Open an *index* buffer with links to index entries.
PATH is required for the org-link, but it does nothing here."
  (interactive)
  (let ((*index-links* '())
	(*initial-letters* '()))

    ;; get links
    (org-element-map (org-ref-parse-buffer) 'link
      (lambda (link)
	(let ((plist (nth 1 link)))

	  (when (equal (plist-get plist ':type) "index")
	    (add-to-list
	     '*index-links*
	     (cons (plist-get plist :path)
		   (format
		    "[[elisp:(progn (switch-to-buffer \"%s\") (goto-char %s) (org-cycle '(64)))][%s]] "
		    (current-buffer)
		    (plist-get plist :begin) ;; position of link
		    ;; grab a description
		    (save-excursion
		      (goto-char (plist-get plist :begin))
		      (if (thing-at-point 'sentence)
			  ;; get a sentence
			  (let ((s (thing-at-point 'sentence)))
			    (cl-loop for char in '("[" "]" "\n")
				     do
				     (setq s (replace-regexp-in-string
					      (regexp-quote char) " " s)))
			    (concat s " "))
			;; or call it a link
			"link")))))))))

    ;; sort the links
    (setq *index-links* (cl-sort *index-links* 'string-lessp :key 'car))

    ;; now separate out into chunks first letters
    (dolist (link *index-links*)
      (push (substring (car link) 0 1) *initial-letters*))

    (setq *initial-letters* (reverse *initial-letters*))

    ;; now create the index
    (switch-to-buffer (get-buffer-create "*index*"))
    (org-mode)
    (erase-buffer)
    (insert "#+TITLE: Index\n\n")
    (dolist (letter *initial-letters*)
      (insert (format "* %s\n" (upcase letter)))
      ;; now process the links
      (while (and
	      *index-links*
	      (string= letter (substring (car (car *index-links*)) 0 1)))
	(let ((link (pop *index-links*)))
	  (insert (format "%s %s\n\n" (car link) (cdr link))))))
    (switch-to-buffer "*index*")))


(org-link-set-parameters "printindex"
			 :follow #'org-ref-index
			 :export (lambda (_path _desc format)
				   (cond
				    ((eq format 'latex)
				     (format "\\printindex")))))


(defun org-ref-idxproc (_backend)
  "Preprocess index entries.
Each entry is replaced by a radio target. The printindex is
replaced by links to them."
  (let* ((index-links (reverse (org-element-map (org-element-parse-buffer) 'link
				 (lambda (lnk)
				   (when (string= "index" (org-element-property :type lnk))
				     lnk)))))
	 (sorted-groups (seq-sort-by
			 (lambda (x)
			   "Alphabetically sort groups"
			   (car x))
			 #'string-lessp
			 (seq-group-by
			  (lambda (lnk)
			    (org-element-property :path lnk))
			  index-links)))
	 (link-replacements '()))

    ;; Sort within each group
    (cl-loop for (key . links) in sorted-groups do
	     (setf (cdr (assoc key sorted-groups))
		   (sort links (lambda (a b)
				 (< (org-element-property :begin a)
				    (org-element-property :begin b))))))
    
    ;; Compute replacements for each index link.
    (cl-loop for (key . links) in sorted-groups do
	     (cl-loop for i from 0 for lnk in links do
		      (cl-pushnew (cons
				   (org-element-property :begin lnk)
				   (format "<<%s-%s>>" key i))
				  link-replacements)))

    (cl-loop for il in index-links collect
	     (cl--set-buffer-substring
	      (org-element-property :begin il)
	      (org-element-property :end il)
	      (cdr (assoc (org-element-property :begin il) link-replacements))))

    ;; Now we replace the printindex link
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (lnk)
	(when (string= "printindex" (org-element-property :type lnk))
	  (cl--set-buffer-substring (org-element-property :begin lnk)
				    (org-element-property :end lnk)
				    ;; If sorted-groups is empty, we should do nothing I think.
				    (if sorted-groups
					(format "*Index*\n\n%s"
						(string-join
						 (cl-loop for (key . links) in sorted-groups collect
							  (format "%s: %s"
								  key
								  (string-join 
								   (cl-loop for i from 0 for lnk in links collect
									    (format "[[%s-%s][%s-%s]] "
										    (org-element-property :path lnk)
										    i
										    (org-element-property :path lnk)
										    i))
								   ", ")))
						 "\n\n"))
				      "")))))))


(provide 'org-ref-misc-links)

;;; org-ref-misc-links.el ends here
