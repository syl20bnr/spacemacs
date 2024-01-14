;;; org-ref-latex.el --- org-ref functionality for LaTeX files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: languages

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

;;; Commentary: Make cites in LaTeX documents clickable, and with tooltips.
;; We use font-lock to add some functionality to the keys.

;;; Code:

(require 'org-ref-citation-links)
(require 'bibtex-completion)

(defvar latex-mode-map)
(defvar org-ref-cite-types)

(defcustom orl-cite-keymap
  (let ((map (copy-keymap latex-mode-map)))
    (define-key map (kbd "S-<left>") #'orl-cite-shift-left)
    (define-key map (kbd "S-<right>") #'orl-cite-shift-right)
    (define-key map (kbd "S-<up>") #'orl-sort-cite-keys)
    (define-key map (kbd "S-<down>") (lambda () (interactive) (orl-sort-cite-keys t)))
    (define-key map (kbd "<tab>") #'org-ref-latex-insert-citation)

    (define-key map [mouse-1] (lambda ()
				(interactive)
				(let ((bibtex-completion-bibliography (org-ref-latex-get-bibliography)))
				  (bibtex-completion-show-entry (list (org-ref-latex-key-at-point)))
				  (bibtex-beginning-of-entry))))
    map)
  "Key map for cite keys.")


(defvar org-ref-latex-cite-re
  (concat "\\\\\\(?1:" (mapconcat
			(lambda (x)
			  (replace-regexp-in-string "\\*" "\\\\*" x))
			(mapcar 'car org-ref-cite-types)
			"\\|")
	  "\\)"
	  "\\(?2:\\[[^]]*\\]\\)?"	; optional []
	  "\\(?3:\\[[^]]*\\]\\)?"	; optional []
	  "{\\(?4:[^}]*\\)}")           ; group 4 contains the keys
  "Regexp for LaTeX citations. \\citetype[opti{o}nal][optiona{l}]{some,keys}.
The clickable part are the keys.")


(defun org-ref-latex-get-bibliography ()
  "Find bibliographies in the tex file."
  (save-excursion
    (let ((bibliography '()))
      (goto-char (point-min))
      (while (re-search-forward "\\\\bibliography{\\(?1:.*\\)}" nil t)
	(setq bibliography (append bibliography
				   (mapcar (lambda (f)
					     (concat f ".bib"))
					   (split-string (match-string-no-properties 1) ",")))))
      (goto-char (point-min))
      (while (re-search-forward "\\\\addbibresource{\\(?1:.*\\)}" nil t)
	(setq bibliography (append bibliography (list (match-string-no-properties 1)))))

      bibliography)))


(defun org-ref-next-latex-cite (&optional limit)
  "Font-lock function to make cites in LaTeX documents clickable."
  (while (re-search-forward org-ref-latex-cite-re limit t)
    (setq font-lock-extra-managed-props (delq 'help-echo font-lock-extra-managed-props))

    ;; save data on cite. This is useful later for modifying cites, clicking, etc.
    (add-text-properties (match-beginning 0) (match-end 0) 
			 `(org-ref-latex-cite (
					       :cite-begin ,(match-beginning 0)
					       :cite-end ,(match-end 0)
					       :cite-key-begin ,(+ 1 (match-beginning 4))
					       :cite-key-end ,(- (match-end 4) 1)
					       :cite-type ,(match-string-no-properties 1)
					       :optional-1 ,(match-string-no-properties 2)
					       :optional-2 ,(match-string-no-properties 3)
					       :keys ,(mapcar
						       #'s-trim
						       (s-split
							","
							(match-string-no-properties 4))))))
    
    (goto-char (match-beginning 0))
    (let ((end (match-end 0)))
      (cl-loop for key in (mapcar #'s-trim (split-string (match-string-no-properties 4) ","))
	       unless (string-empty-p key)
	       do
	       (save-match-data
		 (search-forward key)
		 (add-text-properties
		  (match-beginning 0)
		  (match-end 0)
		  `(
		    org-ref-cite-key ,key
		    mouse-face highlight
		    local-map ,orl-cite-keymap
		    help-echo ,(let* ((bibtex-completion-bibliography (org-ref-latex-get-bibliography)))
				 (condition-case nil
				     (bibtex-completion-apa-format-reference key)
				   (error (display-warning :warning (format "Key %s missing." key)))))))))
      (goto-char end))))


(defun org-ref-latex-cite-on ()
  "Add the font-lock on for citations."
  (font-lock-add-keywords
   'latex-mode
   '((org-ref-next-latex-cite 0 font-lock-constant-face))))

(add-hook 'LaTeX-mode-hook 'org-ref-latex-cite-on)
(add-hook 'LaTeX-mode-hook 'org-ref-latex-cite-on)


;; * Insert cite link

(defun org-ref-latex-on-citation-p ()
  "Return non-nil if on a citation."
  (get-text-property (point) 'org-ref-latex-cite))


(defun org-ref-latex-key-at-point ()
  "Return key at point if there is one."
  (interactive)
  (get-text-property (point) 'org-ref-cite-key))

(defun orl-insert-key (key)
  (cond
   ;; on a key
   ((get-text-property (point) 'org-ref-cite-key)
    (forward-sexp)
    (insert (format ",%s" key)))
   ;; on the leading \
   ((and (get-text-property (point) 'org-ref-latex-cite)
	 (looking-at "\\\\"))
    (search-forward "{")
    (insert (format "%s," key)))
   ;; on {
   ((and (get-text-property (point) 'org-ref-latex-cite)
	 (looking-at "{"))
    (forward-char)
    (insert (format "%s," key)))
   ;; on }
   ((and (get-text-property (point) 'org-ref-latex-cite)
	 (looking-at "}")) 
    (insert (format ",%s" key)))
   ;; on citetype
   ((and (get-text-property (point) 'org-ref-latex-cite)
	 (get-text-property (point) 'org-ref-latex-cite-type))
    (search-forward "{")
    (insert (format "%s," key)))
   ;; everything else
   (t
    (insert (format "\\cite{%s}" key)))))


(defun org-ref-latex-insert-citation ()
  "Insert a citation key with completion.
Should DWIM inserting into existing or new citations."
  (interactive)

  ;; This initializes bibtex if the variable is not defined.
  (unless bibtex-completion-display-formats-internal
    (bibtex-completion-init))

  (let* ((bibtex-completion-bibliography (org-ref-latex-get-bibliography))
	 (candidates (bibtex-completion-candidates)))
    (ivy-read "org-ref-ivy BibTeX entries: " candidates
	      :preselect (ivy-thing-at-point) 
	      :action '(1
			("o" (lambda (candidate)
			       (orl-insert-key (cdr (assoc "=key=" (cdr candidate)))))
			 "insert")))))

(defun orl-swap-citation (direction)
  (let* ((key (org-ref-latex-key-at-point))
	 (data (get-text-property (point) 'org-ref-latex-cite))
	 (keys (plist-get data :keys))
	 (i (seq-position keys key #'string=))
	 cite-string)
    (if (> direction 0)
	(org-ref-swap-list-elements i (min (+ i 1) (- (length keys) 1)) keys)
      (org-ref-swap-list-elements i (max (- i 1) 0) keys))
    (setq data (plist-put data :keys keys))

    ;; now render it
    (setq cite-string (concat (format "\\%s" (plist-get data :cite-type))
			      (or (plist-get data :optional-1) "")
			      (or (plist-get data :optional-2) "")
			      (format  "{%s}" (s-join "," (plist-get data :keys)))))
    (cl--set-buffer-substring (plist-get data :cite-begin)
			      (plist-get data :cite-end)
			      cite-string)
    (search-forward key)
    (goto-char (match-beginning 0))))


(defun orl-cite-shift-left ()
  "Shift cite key at point to the left."
  (interactive)
  (orl-swap-citation -1))


(defun orl-cite-shift-right ()
  "Shift cite-key at point to the right."
  (interactive)
  (orl-swap-citation +1))


(defun orl-sort-cite-keys (&optional descending)
  "Sort keys in ascending order by year.
With prefix arg DESCENDING, sort in descending order."
  (interactive "P")
  (let* ((data (get-text-property (point) 'org-ref-latex-cite))
	 (keys (plist-get data :keys)) 
	 cite-string)

    (setq data (plist-put data :keys
			  (cl-loop for entry in
				   (cl-sort (cl-loop for key in keys collect
						     (list :key key :year (bibtex-completion-get-value
									   "year"
									   (bibtex-completion-get-entry
									    key))))
					    (if descending
						(lambda (x y)
						  (> (string-to-number (plist-get x :year))
						     (string-to-number (plist-get y :year))))
					      (lambda (x y)
						(< (string-to-number (plist-get x :year))
						   (string-to-number (plist-get y :year))))))
				   collect
				   (plist-get entry :key))))
    
    

    ;; now render it
    (setq cite-string (concat (format "\\%s" (plist-get data :cite-type))
			      (or (plist-get data :optional-1) "")
			      (or (plist-get data :optional-2) "")
			      (format  "{%s}" (s-join "," (plist-get data :keys)))))
    (cl--set-buffer-substring (plist-get data :cite-begin)
			      (plist-get data :cite-end)
			      cite-string)
    (search-forward (car (plist-get data :keys)))
    (goto-char (match-beginning 0))))



(provide 'org-ref-latex)
;;; org-ref-latex.el ends here
