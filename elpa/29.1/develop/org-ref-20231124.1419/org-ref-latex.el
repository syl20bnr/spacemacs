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


(defvar org-ref-latex-cite-re
  (concat "\\\\\\(?1:" (mapconcat
			(lambda (x)
			  (replace-regexp-in-string "\\*" "\\\\*" x))
			(mapcar 'car org-ref-cite-types)
			"\\|")
	  "\\)"
	  "\\(?2:\\[[^]]*\\]\\)?"		; optional []
	  "\\(?3:\\[[^]]*\\]\\)?"		; optional []
	  "{\\(?4:[^}]*\\)}")           ; group 4 contains the keys
  "Regexp for LaTeX citations. \\citetype[opti{o}nal][optiona{l}]{some,keys}.
The clickable part are the keys.")


(defun org-ref-latex-get-bibliography ()
  "Find bibliographies in the tex file"
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
		  `(mouse-face highlight
			       local-map ,(let ((map (copy-keymap latex-mode-map)))
					    (define-key map [mouse-1]
					      `(lambda ()
						 (interactive)
						 (let ((bibtex-completion-bibliography (org-ref-latex-get-bibliography)))
						   (bibtex-completion-show-entry (list ,key))
						   (bibtex-beginning-of-entry))))
					    map)
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


(provide 'org-ref-latex)
;;; org-ref-latex.el ends here
