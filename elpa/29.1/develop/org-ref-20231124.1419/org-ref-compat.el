;;; org-ref-compat.el --- Compatibility functions for org-cite

;;; Commentary:
;;
;; The main function of this library is to provide compatibility functions for
;; converting between org-ref and org-cite.

(require 'org-ref-export)


(defcustom org-ref-to-org-cite-mapping
  '(("cite" . "")
    ("citep" . "")
    ("Citep" . "//caps-full")
    ("citealp" . "//bare")
    ("Citealp" . "//bare-caps")
    ("Citealp*" . "//bare-caps-full")
    ("citep*" . "//full")

    ("citet" . "/text")

    ("citeyear" . "/noauthor/bare")
    ("citeyearpar" . "/noauthor")
    
    ("nocite" . "/nocite")

    ("citeauthor" . "/author")
    ("citeauthor*" . "/author/full")
    ("Citeauthor" . "/author/caps"))
  
  "A-list of (org-ref-type . org-cite-style).
This builds from
https://blog.tecosaur.com/tmio/2021-07-31-citations.html#cite-syntax.
There is no way to get them all though, there are conflicting
translations with some biblatex and some natbib commands. This
list maps the natbib commands. I have also opted to use the full
names rather than the short names."
  :group 'org-ref)


(defun org-ref-to-org-cite ()
  (interactive)
  (let ((ref-cites (reverse (org-ref-get-cite-links)))
	ref-type
	path
	beg end)
    ;; This takes care of the cite links
    (cl-loop for rc in ref-cites do
	     (setq
	      type (org-element-property :type rc)
	      path (org-element-property :path rc)
	      beg (org-element-property :begin rc)
	      end (org-element-property :end rc))
	     (cl--set-buffer-substring
	      beg end
	      (format "[cite%s:%s]"
		      (or (cdr (assoc type org-ref-to-org-cite-mapping)) "")
		      ;; This is not 100% correct, if someone has put an extra &
		      ;; anywhere in a note, this will be a little wrong. It
		      ;; would be a little more correct if I also look for a
		      ;; word next to it. The most correct would probably be to
		      ;; build the data and then use org-element interpret I
		      ;; think
		      (replace-regexp-in-string "&" "@" path))))

    ;; Next replace bibliography links. I assume the paths are ok, and we just
    ;; need to convert them to keywords.
    (cl-loop for bib-link in
	     (reverse (org-element-map (org-element-parse-buffer) 'link
			(lambda (bl)
			  (when (member (org-element-property :type bl)
					'("bibliography" "nobibliography"))
			    bl))))
	     do
	     (cl--set-buffer-substring
	      (org-element-property :begin bib-link)
	      (org-element-property :end bib-link)
	      (format "#+bibliography: %s%s"
		      (org-element-property :path bib-link)
		      (if (string= "bibliography"
				   (org-element-property :type bib-link))
			  "\n#+print_bibliography:"
			""))))
    ;; Note it is a bit ambiguous what do do about where the bibliography is to
    ;; be printed. This should be done via #+print_bibliography:. In org-ref the
    ;; bibliography normally goes where the bibliography link was, and I sue
    ;; that convention.
    
    ))


(provide 'org-ref-compat)

;;; org-ref-compat.el ends here
