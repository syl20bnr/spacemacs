;; $Id$
;;

;;
;; Emacs functions for simple Perl pod parsing
;;  parse result format based on pod2text
;;
;; required in bioperl-mode.el
;;
;; Author: Mark A. Jensen
;; Email : maj -at- fortinbras -dot- us
;;
;; Part of The Documentation Project
;; http://www.bioperl.org/wiki/The_Documentation_Project
;;
;;

;; Copyright (C) 2009 Mark A. Jensen

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

(defvar pod-keywords
  '( "pod" "head1" "head2" "head3" "head4" "over" "item" "back" "begin" "end" "for" "encoding" "cut" )
  "Perl pod keywords (sans '=') ")

(defvar pod-format-codes
  '( "I" "B" "C" "L" "E" "F" "S" "X" "Z" )
  "Perl pod format codes (sans <>)" )

(defun pod-parse-buffer (buf &optional alt-format)
  "Parse the pod in the BUF.
Removes code and leaves pod. Does some simple formatting a la
pod2text as setup for pod-mode. If ALT-FORMAT is true, headers
are flanked by '='s as in pod2text -a. "
  (save-excursion
    (set-buffer buf)
    (let (
	  (cur-state)
	  (cur-content)
	  (tmp-state)
	  (tmp-content)
	  (encoding-type)
	  (parse-tree '(("Root")))
	  (line)
	  (header-level)
	  (beg (goto-char (point-min)))
	  (end)
	  (tbeg) (tend) ;; text region
	  )
      (goto-char (point-min))
      ;; get encoding if present
      (if (re-search-forward "^=encoding\\s +\\(.*?\\)\\s " (point-max) t)
	  (setq encoding-type (match-string 1)))
      (goto-char (point-min))
      (while (not (eobp))
	(setq end (re-search-forward "^=\\([a-z0-9]+\\)\\(?:$\\|\\s *\\(.*?\\)\\)$" (point-max) 1))
	(if (not end)
	    t ;; done
	  (setq tmp-state (match-string 1))
	  (setq tmp-content (match-string 2))
	  (if (not cur-state)
	      (progn
		(beginning-of-line)
		(pod-do-format "ignore" beg (point))
		(setq end beg)))
	  (setq cur-state tmp-state)
	  (setq cur-content tmp-content)
	  (if (not cur-state)
	      t ;; done
	    ;; otherwise, in a pod region
	    (if (not (member cur-state pod-keywords))
		(error (concat "'" cur-state "' not a pod keyword")))
	    (cond
	     ( (not cur-state)
	       nil ;; ????
	       )
	     ( (string-equal cur-state "cut")
	       (forward-line 0)
	       (kill-line 2)
	       (pod-do-format "text" beg (point))
	       (setq cur-state nil) )
	     ( (string-equal cur-state "pod")
	       (forward-line 0)
	       (kill-line 2)
	       (pod-do-format "text" beg (point))
	       )
	     ( (string-match "head\\([1-4]\\)" cur-state)
	       (setq head-level (string-to-number (match-string 1 cur-state)))
	       (forward-line 0)
	       (kill-line 2)
	       (pod-do-format "text" beg (point))
	       (if (not cur-content)
		   nil
		 (if (not alt-format)
		     (progn
		       (insert-char ?  (* 2 (1- head-level)))
		       (insert cur-content "\n"))
		   (cond
		    ((= head-level 1)
		     (insert "==== " cur-content " ====\n"))
		    ((= head-level 2)
		     (insert "==   " cur-content "   ==\n"))
		    ((= head-level 3)
		     (insert "=    " cur-content "    =\n"))
		    ((= head-level 4)
		     (insert "-    " cur-content "    -\n"))))
		 ))
	     ( (string-equal cur-state "over")
	       (let (
		     (indent-level cur-content)
		     (back (save-excursion
			     (re-search-forward "^=back" (point-max) t)))
		     )
		 (unless back
		   (error "=over has no matching =back"))
		 (forward-line 0)
		 (kill-line 2)
		 (pod-do-format "text" beg (point))
		 (setq beg (point))
		 (while (re-search-forward "^=item\\s +\\(.*?\\)$" back t)
		   (let ( (item (match-string 1) ) )
		     (forward-line 0)
		     (kill-line 2)
		     (pod-do-format "text" beg (point))
		     (if (not alt-format)
			 (insert "    * " item "\n")
		       (insert ":   " item "\n")))
		   (setq beg (point)))
		 (re-search-forward "^=back" (point-max))
		 (forward-line 0)
		 (kill-line 2)
		 (pod-do-format "text" beg (point))
		 ))
	     ( (string-equal cur-state "begin")
	       (let (
		     (format cur-content)
		     (end (save-excursion
			    (re-search-forward (concat "^=end\\s +" format)
					       (point-max) t)))
		     (content-beg) (content-end)
		     )
		 (unless end
		   (error (concat "=begin " format " has no matching end.")))
		 (forward-line 0)
		 (kill-line 2)
		 (setq content-beg (point))
		 (re-search-forward  (concat "^=end\\s +" format))
		 (forward-line 0)
		 (kill-line 2)
		 (setq content-end (point))
		 (pod-do-format format content-beg content-end)
		 ))
	     ( (string-equal cur-state "for")
	       (string-match "\\([a-z]+\\)\\s +\\(.*?\\)$" cur-content)
	       (let (
		     (format (match-string 1 cur-content))
		     (content (match-string 2 cur-content))
		     )
		 (forward-line 0)
		 (kill-line 2)
		 (pod-do-format "text" beg (point))
		 ))
	     ( (string-equal cur-state "encoding")
	       (let (
		     (type cur-content)
		     )
		 (forward-line 0)
		 (kill-line 2)
		 (pod-do-format "text" beg (point))
		 t))
	     ))
	  ;; movement here?
	  (setq beg (point))
	  (setq cur-content nil) ;; clear means 'moved off pod descr line'
	))
      (pod-do-format (if cur-state "text" "ignore") beg (point-max))
      t)
    ))

(defun pod-do-format (format beg end)
  "Handle pod =begin format ... =end format blocks.
FORMAT is a format identifier (a string); BEG and END define the
text region."
  ;; ignore for now
  (if (= beg end)
      nil
    (cond
     ((string-equal format "ignore")
      (delete-region beg end))
     ((string-equal format "text")
      ;; format ordinary and verbatim lines
      (save-excursion
	(goto-char beg)
	(forward-line 0)
	(while (and (< (point) end) (not (eobp)))
	  (cond
	   ((string-match "[[:blank:]]" (char-to-string (char-after)))
	    (insert-char ?  4)
	    (setq end (+ end 4)))
	   ((string-match "[[:space:]]" (char-to-string (char-after)))
	    t)
	   (t
	    (insert-char ?  4)
	    (setq end (+ end 4))))
	  (forward-line 1)
	  )))
     (t
      ;; otherwise, remove (ignore)
      (delete-region beg end)) )))

(provide 'pod)
