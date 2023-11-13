;;; org-ref-bibliography-links.el --- Bibliography and bibliographystyle links
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

(defcustom org-ref-latex-bib-resolve-func #'file-relative-name
  "Function to expand paths to the bibliography file on latex export.
Use this to convert a path to what you want, e.g. relative to
some directory, absolute, etc."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-validate-bibliography nil
  "If non-nil, validate bibliography files in fontification.
This can be slow, so we don't do it by default."
  :type 'boolean
  :group 'org-ref)


;;* bibliography* links

(defun org-ref-get-bibfile-path (bibfile)
  "Get a path to BIBFILE as local file, or using kpsewhich.
This should allow you to use a bib file that is setup with TeX
variables like BIBINPUTS."
  (or (when (file-exists-p bibfile) bibfile)
      (let ((f (replace-regexp-in-string
		"\n$" ""
		(shell-command-to-string (format "kpsewhich %s" bibfile)))))
	(unless (string= "" f)
	  f))))


(defun org-ref-bibliography*-export (cmd bibfiles _desc backend)
  "Exporting function for bibliography links.
To be used as a partial function e.g.
 (apply-partially \"bibliography\" 'org-ref-bibliography*-export)
Argument CMD is the command it should export to.
Argument BIBFILES is a comma-separated list of strings.
Argument BACKEND is the export backend."
  (cond
   ((eq backend 'latex)
    (format "%s{%s}"
	    cmd
	    (if (not (string= "" bibfiles)) 
		(replace-regexp-in-string
		 "\\.bib" ""
		 (mapconcat
		  'identity
		  (mapcar
		   (lambda (f)
		     (funcall org-ref-latex-bib-resolve-func (org-ref-get-bibfile-path f)))
		   (split-string bibfiles ","))
		  ","))
	      "")))))


(defun org-ref-bibliography-activate (start end path _bracketp)
  "Activate a bibliography link.
Adds a warning face to non-existent or invalid bib-files.
START and END are the bounds of the link.
PATH is a comma-separated list of bibfiles."
  (goto-char start)
  (cl-loop for p in (split-string path ",") do
	   (setq p (string-trim p))
	   (search-forward p)
	   (setq p (org-ref-get-bibfile-path p))
	   (put-text-property (match-beginning 0) (match-end 0) 'org-ref-bibfile p)
	   (put-text-property (match-beginning 0) (match-end 0) 'help-echo (format "File exists at %s" p))

	   ;; activate files that don't exist
	   (when (or (null p) (not (file-exists-p p)))
	     (put-text-property (match-beginning 0) (match-end 0)
				'face 'font-lock-warning-face)
	     (put-text-property (match-beginning 0) (match-end 0)
				'help-echo "This file was not found."))

	   (when (and p (file-exists-p p))
	     (when org-ref-validate-bibliography
	       ;; Let's do a validation, but only if it has changed since the last time we checked.
	       (let* ((mod-time-last-check (or (get-text-property (match-beginning 0)
								  'mod-time-last-check)
					       '(0 0 0 0)))
		      (last-modified (file-attribute-modification-time (file-attributes p)))
		      (bibtex-valid))
		 (if (time-equal-p mod-time-last-check last-modified)
		     (setq bibtex-valid (get-text-property (match-beginning 0) 'bibtex-valid))

		   ;; the times were not equal, so we check and store the state.
		   (setq bibtex-valid (save-match-data
					(with-current-buffer (find-file-noselect p)
					  (bibtex-validate))))
		   (put-text-property (match-beginning 0)
				      (match-end 0)
				      'mod-time-last-check
				      last-modified)
		   (put-text-property (match-beginning 0)
				      (match-end 0)
				      'bibtex-valid
				      bibtex-valid))

		 (unless bibtex-valid
		   (put-text-property (match-beginning 0) (match-end 0)
				      'face 'font-lock-warning-face)
		   (put-text-property (match-beginning 0) (match-end 0)
				      'help-echo "This file did not pass `bibtex-validate'.")))))))


(defun org-ref-bibliography*-follow (_path)
  "Function to follow bibliography links."
  (interactive)
  (save-excursion
    (unless (get-text-property (point) 'org-ref-bibfile)
      (re-search-forward ":"))
    (find-file (org-ref-get-bibfile-path (get-text-property (point) 'org-ref-bibfile)))))


(defun org-ref-printbibliography-export (options _desc backend)
  "Export function for printbibliography links.
Argument OPTIONS are the options used for the command.
Optional argument BACKEND is the export backend."
  (cond
   ((eq backend 'latex)
    ;; write out the biblatex bibliography command
    (format "\\printbibliography%s"
	    (if (not (string= "" options))
		(format "[%s]" options)
	      "")))))

(defvar org-ref-cite-types)
(declare-function org-element-map "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-parse-buffer "org-element")
(declare-function bibtex-completion-show-entry "bibtex-completion")
(declare-function org-ref-possible-bibfiles "org-ref-core")
(declare-function org-ref-parse-cite-path "org-ref-citation-links")

(defun org-ref-insert-bibliography-link ()
  "Insert a bibliography link for the files used in this buffer."
  (interactive)
  (let* ((cite-links (org-element-map (org-element-parse-buffer) 'link
		       (lambda (lnk)
			 (when (assoc (org-element-property :type lnk) org-ref-cite-types)
			   lnk))))
	 (keys (delete-dups (cl-loop for cl in cite-links append
				     (cl-loop for ref in (plist-get (org-ref-parse-cite-path
								     (org-element-property :path cl))
								    :references)
					      collect (plist-get ref :key)))))
	 (files (delete-dups (cl-loop for key in keys collect
				      (save-window-excursion
					(bibtex-completion-show-entry (list key))
					(buffer-file-name))))))
    (insert (format "[[bibliography:%s]]" (string-join files ",")))))


(defun org-ref-bibliography-complete (&optional arg)
  "Completion function for bibliography links."
  (concat "bibliography:"
	  (completing-read "Bibliography file: " (org-ref-possible-bibfiles))))

(defun org-ref-nobibliography-complete (&optional arg)
  "Completion function for bibliography links."
  (concat "nobibliography:"
	  (completing-read "Bibliography file: " (org-ref-possible-bibfiles))))


(defvar org-ref-default-citation-link)

(defun org-ref-bibtex-store-link ()
  "Store a link from a bibtex file. Only supports the cite link.
This essentially the same as the store link in org-bibtex, but it
creates a cite link."
  (when (eq major-mode 'bibtex-mode)
    (let ((link (concat org-ref-default-citation-link
			":&"
			(save-excursion
                          (bibtex-beginning-of-entry)
			  (cdr (assoc "=key=" (bibtex-parse-entry)))))))
      (push (list link) org-stored-links)
      (car org-stored-links))))


;; ** bibliography* links

(org-link-set-parameters "bibliography"
			 :follow #'org-ref-bibliography*-follow
			 :store #'org-ref-bibtex-store-link
			 :complete #'org-ref-bibliography-complete
			 :help-echo "Bibliography link"
			 :export (apply-partially 'org-ref-bibliography*-export "\\bibliography")
			 :activate-func #'org-ref-bibliography-activate
			 :face 'org-link)


(org-link-set-parameters "nobibliography"
			 :complete #'org-ref-nobibliography-complete
			 :store #'org-ref-bibtex-store-link
			 :help-echo "No bibliography link"
			 :activate-func #'org-ref-bibliography-activate
			 :follow #'org-ref-bibliography*-follow
			 :export (apply-partially 'org-ref-bibliography*-export "\\nobibliography")
			 :face 'org-link)


(org-link-set-parameters "nobibliography*"
			 :complete #'org-ref-nobibliography-complete
			 :store #'org-ref-bibtex-store-link
			 :help-echo "No bibliography link"
			 :activate-func #'org-ref-bibliography-activate
			 :follow #'org-ref-bibliography*-follow
			 :export (apply-partially 'org-ref-bibliography*-export "\\nobibliography*")
			 :face 'org-link)


(org-link-set-parameters "printbibliography"
			 :export #'org-ref-printbibliography-export)

;; Note I removed the addbibresource link, it goes in the header, not the document.

;; *** bibliographystyle
(defvar-local org-ref-bst-styles nil
  "A list of known bibliography styles. Used to cache results.")


(defun org-ref-clear-bst-cache ()
  "Clear `org-ref-bst-styles' to reload it."
  (interactive)
  (setq org-ref-bst-styles nil))


(defun org-ref-bibliography-styles ()
  "Return a list of known bibliography styles.
Returns `org-ref-bst-styles' or sets it and returns it."
  (or org-ref-bst-styles
      (setq org-ref-bst-styles
	    (mapcar 'file-name-nondirectory
		    (mapcar 'file-name-sans-extension
			    (-flatten
			     (mapcar (lambda (path)
				       (setq path (replace-regexp-in-string "!" "" path))
				       (when (file-directory-p path)
					 (f-entries path (lambda (f) (f-ext? f "bst")))))
				     (split-string
				      ;; https://tex.stackexchange.com/questions/431948/get-a-list-of-installed-bibliography-styles-with-kpsewhich?noredirect=1#comment1082436_431948
				      (shell-command-to-string "kpsewhich -expand-path '$BSTINPUTS'")
				      ":"))))))))


(defun org-ref-bibliographystyle-complete-link (&optional arg)
  "Completion function for bibliography style links.
ARG is a not used."
  (when (executable-find "kpsewhich")
    (concat "bibliographystyle:"
	    (completing-read "Style: " (org-ref-bibliography-styles)))))


(defun org-ref-bibliographystyle-activate (start _end path _bracketp)
  "Activation function for bibliography styles.
START is the beginning position of the link.
Optional argument PATH contains the selected style."
  (unless (member path (org-ref-bibliography-styles))
    (goto-char start)
    (search-forward path)
    (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-warning-face)
    (put-text-property (match-beginning 0) (match-end 0) 'help-echo "Unrecognized style")))


(defun org-ref-bibliographystyle-export (style _desc backend)
  "Export function for bibliographystyle links.
Argument STYLE is the desired style.
Optional argument BACKEND is the export backend."
  (cond
   ((or (eq backend 'latex)
	(eq backend 'beamer))
    ;; write out the latex bibliography command
    (format "\\bibliographystyle{%s}" style))
   ;; No other backend needs this I think
   (t
    "")))


(org-link-set-parameters "bibliographystyle"
			 :complete #'org-ref-bibliographystyle-complete-link
			 :activate-func #'org-ref-bibliographystyle-activate
			 :export #'org-ref-bibliographystyle-export)


(provide 'org-ref-bibliography-links)

;;; org-ref-bibliography-links.el ends here
