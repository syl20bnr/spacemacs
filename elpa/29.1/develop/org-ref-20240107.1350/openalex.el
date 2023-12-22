;;; openalex.el --- Org-ref interface to OpenAlex

;;; Commentary:
;; This is an elisp interface to OpenAlex (https://docs.openalex.org/) for org-ref.
;;
;; This provides functionality for the Work and Author API
;;
;; See
;; https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication#the-polite-pool
;; for why we add email to the request.

(require 'dash)
(require 'request)

;;; Code:

(defun oa--response-parser ()
  "Parse the response from json to elisp."
  (let ((json-array-type 'list)
	(json-object-type 'plist)
	(json-key-type 'keyword)
	(json-false nil)
	(json-encoding-pretty-print nil))
    (json-read)))


;; * Work object

(defun oa--work (entity-id &optional filter)
  "Retrieve json data for a Work object for ENTITY-ID.
ENTITY-ID is an OpenAlex ID, DOI, Pubmed id,etc.

ENTITY-ID may also be a list of ids with a filter.

If FILTER is non-nil it should be a string like \"filter=openalex:\"

https://docs.openalex.org/api-entities/works"

  (let* ((url (concat  "https://api.openalex.org/works"
		       (if filter
			   (concat "?" filter entity-id)
			 (concat "/" entity-id))
		       (if user-mail-address
			   (if filter
			       (concat "&mailto=" user-mail-address)
			     (concat "?mailto=" user-mail-address))
			 "")))
	 (req (request url :sync t :parser 'oa--response-parser))
	 (data (request-response-data req)))
    ;; this is for convenience to inspect data in a browser.
    (plist-put data :oa-url url)
    data))


;; * Viewing works
;;
;; This section provides a replacer and helper function to format org-entries
;; from the results returned in OpenAlex.




(defun oa--replacer (key object)
  "Replacer function for `s-format'.
QUERY is a string that is either a sexp for a function to
evaluate or a dot notation path to data in OBJECT. If QUERY is a
sexp, it is read and evaluated. Otherwise, the path is split, and
looked up sequentially in object.

OBJECT is a plist, usually from a Work request."
  (if (or (s-starts-with? "(" key)
	  (boundp (intern-soft key)))
      ;; this is a function we should evaluate
      ;; it is kind of janky and might miss context.
      (eval (read key) t)
    ;; just get data
    (let ((fields (s-split "\\." key))
	  result)
      (while fields
	(setq object (plist-get object (intern-soft (concat ":" (pop fields))))))
      (or (string-replace "\\n" "" (format "%s" object)) "Not found"))))




;; ** help functions for complex data
;;
;; Some things like authors need to be constructed, and cannot just be looked
;; up. In other cases, I want logic, e.g. if data is there provide something,
;; and if not return an empty string. These functions do that work.
(defun oa--authors (wrk)
  "Return an author string for WRK.
The string is a comma-separated list of links to author pages in OpenAlex."
  (s-join ", " (cl-loop for author in (plist-get wrk :authorships)
			collect
			(format "[[elisp:(oa-author \"%s\")][%s]]"
				(plist-get
				 (plist-get author :author)
				 :id)
				(plist-get
				 (plist-get author :author)
				 :display_name)))))


;; I want some links if they can be made so the buffer is interactive. It might
;; be nice to integrate M-, navigation.
(defun oa--elisp-get-bibtex (wrk)
  "Return a elisp link to get a bibtex entry for WRK if there is a doi."
  (if-let ((doi (plist-get wrk :doi)))
      (format "[[elisp:(doi-add-bibtex-entry \"%s\")][Get bibtex entry]]" doi)
    ""))


(defun oa--elisp-get-oa-related (wrk)
  "Return a elisp link to get related works for WRK."
  (format "[[elisp:(progn (xref--push-markers) (oa--related-works \"%s\"))][Get related work (%s)]]"
	  (plist-get wrk :id)
	  (length (plist-get wrk :related_works))))


(defun oa--elisp-get-oa-refs (wrk)
  "Return a elisp link to get references for WRK."
  (format "[[elisp:(progn (xref--push-markers) (oa--referenced-works \"%s\"))][Get references (%s)]]"
	  (plist-get wrk :id)
	  (length  (plist-get wrk :referenced_works))))


(defun oa--elisp-get-oa-cited-by (wrk)
  "Return a elisp link to get works that cite WRK."
  (format "[[elisp:(progn (xref--push-markers) (oa--cited-by-works \"%s\"))][Get cited by (%s)]]"
	  (plist-get wrk :id)
	  (plist-get wrk :cited_by_count)))


(defun oa--works-entries (works)
  "Return a list of org-formatted entries in WORKS.
WORKS is a list of results from OpenAlex."
  (cl-loop for wrk in (plist-get works :results)
	   collect
	   (s-format "** ${title}
:PROPERTIES:
:HOST: ${primary_location.source.display_name}
:YEAR: ${publication_year}
:CITED_BY_COUNT: ${cited_by_count}
:AUTHOR: ${(oa--authors wrk)}
:DOI: ${doi}
:OPENALEX: ${id}
:END:


${(oa--elisp-get-bibtex wrk)}

- ${(oa--elisp-get-oa-refs wrk)}
- ${(oa--elisp-get-oa-related wrk)}
- ${(oa--elisp-get-oa-cited-by wrk)}

"
		     'oa--replacer wrk)))


(defun oa--works-buffer (bufname header entries)
  "Create an org-buffer with BUFNAME representing the results in WORKS.
HEADER is the first thing in the buffer
WORKS is usually a list of results from OpenAlex.
Argument ENTRIES A list of strings for each org entry."
  (let ((buf (get-buffer-create bufname)))
    
    (with-current-buffer buf
      (erase-buffer)
      (insert header)
      (insert "#+COLUMNS: %25ITEM %YEAR %CITED_BY_COUNT
elisp:org-columns    elisp:org-columns-quit


#+caption: Sort
| year     | [[elisp:(oa-buffer-sort-year t)][old first]] | [[elisp:(oa-buffer-sort-year)][new first]] |
| cited by | [[elisp:(oa-buffer-sort-cited-by-count t)][low first]] | [[elisp:(oa-buffer-sort-cited-by-count)][high first]] |

")
      (insert (s-join "\n" entries))
      (org-mode)
      (goto-char (point-min))
      (org-next-visible-heading 1))
    ;; (display-buffer-in-side-window buf '((side . right)))
    (pop-to-buffer buf)))


;; There is something funny about pages here, maybe 25 results per page?
;; https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/paging I
;; am not sure how to do pages in this approach, so I am just getting these 25
;; at a time.
(defun oa--related-works (entity-id)
  "Show the Related works buffer for ENTITY-ID."
  (let* ((wrk (oa--work entity-id))
	 (related-work (plist-get wrk :related_works))
	 split
	 entries)
    (while related-work
      (setq split (-split-at 25 related-work)
	    related-work (nth 1 split))
      
      ;; split is what we process now
      (setq entries (append entries
			    (oa--works-entries
			     (oa--work (s-join "|" (nth 0 split))
				       "filter=openalex:")))))
    
    (oa--works-buffer
     "*OpenAlex - Related works*"
     (format "* OpenAlex - Related works for %s ([[%s][json]])
%s\n\n"
	     entity-id
	     (plist-get wrk :oa-url)
	     (s-format ":PROPERTIES:
:TITLE: ${title}
:HOST: ${primary_location.source.display_name}
:AUTHOR: ${(oa--authors wrk)}
:DOI: ${doi}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:END:" 'oa--replacer wrk))
     entries)))


(defun oa--referenced-works (entity-id)
  "Show the Referenced work for ENTITY-ID."
  (let* ((wrk (oa--work entity-id))
	 (referenced-work (plist-get wrk :referenced_works))
	 split
	 (entries '()))
    (while referenced-work
      (setq split (-split-at 25 referenced-work)
	    referenced-work (nth 1 split))
      ;; split is what we process now
      (setq entries (append entries
			    (oa--works-entries
			     (oa--work (s-join "|" (nth 0 split))
				       "filter=openalex:")))))
    (oa--works-buffer
     "*OpenAlex - References*"
     (format "* OpenAlex - References from %s ([[%s][json]])
%s\n\n"
	     entity-id
	     (plist-get wrk :oa-url)
	     (s-format ":PROPERTIES:
:TITLE: ${title}
:HOST: ${primary_location.source.display_name}
:AUTHOR: ${(oa--authors wrk)}
:DOI: ${doi}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:END:" 'oa--replacer wrk))
     entries)))


;; This function is different than the previous two. First we follow a URL
;; provided by the data, and second, here we do follow pages.
(defun oa--cited-by-works (entity-id)
  "Show the Cited by buffer for ENTITY-ID."
  (let* ((wrk (oa--work entity-id))
	 (url (plist-get wrk :cited_by_api_url))
	 (cited-by-works (request-response-data
			  (request url
			    :sync t
			    :parser 'oa--response-parser)))
	 (count (plist-get (plist-get cited-by-works :meta) :count))
	 (per-page (plist-get (plist-get cited-by-works :meta) :per_page))
	 (entries '())
	 (page 2))
    ;; get first page
    (setq entries (oa--works-entries cited-by-works))
    (while (> count (* per-page (- page 1)))
      (setq cited-by-works (request-response-data
			    (request (format "%s&page=%s" url page)
			      :sync t
			      :parser 'oa--response-parser)))
      (setq entries (append entries (oa--works-entries cited-by-works)))
      (cl-incf page))
    
    (oa--works-buffer
     "*OpenAlex - Cited by*"
     (format "* OpenAlex - %s Cited by ([[%s][json]])
%s"
	     entity-id
	     url
	     (s-format ":PROPERTIES:
:TITLE: ${title}
:HOST: ${primary_location.source.display_name}
:AUTHOR: ${(oa--authors wrk)}
:DOI: ${doi}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:END:\n\n" 'oa--replacer wrk))
     entries)))


;; ** buffer utilities for sorting entries

(defun oa-buffer-sort-year (&optional ascending)
  "Sort org headings by year in descending order (new to old).
With prefix arg ASCENDING, sort in ascending order (old to new)"
  (interactive "P")
  (if ascending
      (org-sort-entries nil ?f
			(lambda () (string-to-number (or (org-entry-get (point) "YEAR") "0")))
			(lambda (y1 y2)
			  (< y1 y2)))
    (org-sort-entries nil ?f
		      (lambda () (string-to-number (or (org-entry-get (point) "YEAR") "0")))
		      (lambda (y1 y2)
			(> y1 y2)))))


(defun oa-buffer-sort-cited-by-count (&optional ascending)
  "Sort orgheadings by cited by count in descending order high to low.
With prefix arg ASCENDING sort from low to high."
  (interactive "P")
  (if ascending
      (org-sort-entries nil ?f
			(lambda ()
			  (string-to-number
			   (or (org-entry-get (point) "CITED_BY_COUNT")
			       "0")))
			#'<)
    (org-sort-entries nil ?f
		      (lambda ()
			(string-to-number
			 (or
			  (org-entry-get (point) "CITED_BY_COUNT")
			  "0")))
		      #'>)))


;; * Interactive versions for org-ref citations

(defun oa-related-works ()
  "Open the side window for Related works on cite at point."
  (interactive)
  (oa--related-works (concat "https://doi.org/" (org-ref-get-doi-at-point))))


(defun oa-referenced-works ()
  "Open the side window for References from the cite at point."
  (interactive)
  (oa--referenced-works (concat "doi:" (org-ref-get-doi-at-point))))


(defun oa-cited-by-works ()
  "Open the side window for Citing works for the cite at point."
  (interactive)
  (oa--cited-by-works (concat "doi:" (org-ref-get-doi-at-point))))



(defhydra+ org-ref-citation-hydra ()
  "Add related documents action to `org-ref-citation-hydra'."
  ("xr" oa-related-works "Related documents" :column "OpenAlex"))

(defhydra+ org-ref-citation-hydra ()
  "Add cited by documents action to `org-ref-citation-hydra'."
  ("xc" oa-cited-by-works "Cited by documents" :column "OpenAlex"))

(defhydra+ org-ref-citation-hydra ()
  "Add references from action to `org-ref-citation-hydra'."
  ("xf" oa-referenced-works "References from" :column "OpenAlex"))


;; * utilities

(defun oa-kill-buffers ()
  "Kill OpenAlex buffers."
  (interactive)
  (cl-loop for buf in (buffer-list)
	   do
	   (when (s-starts-with? "*OpenAlex" (buffer-name buf))
	     (kill-buffer buf))))

;; * Author object

(defun oa--author (entity-id &optional filter)
  "Get an Author object for entity-id"
  (let* ((url (concat  "https://api.openalex.org/authors"
		       (if filter
			   (concat "?" filter entity-id)
			 (concat "/" entity-id))
		       (if user-mail-address
			   (concat "?mailto=" user-mail-address)
			 "")))
	 (req (request url :sync t :parser 'oa--response-parser))
	 (data (request-response-data req))) 
    ;; this is for convenience to inspect data in a browser.
    (plist-put data :oa-url url)
    data))


(defun oa--author-entries (works-data url)
  "Get entries from WORKS-DATA."
  (let* ((meta (plist-get works-data :meta)) 
	 (per-page (plist-get meta :per_page))
	 (count (plist-get meta :count))
	 (pages (/ count per-page))
	 (entries '())
	 purl)
    ;; if there is a remainder we need to get the rest
    (when (> (mod count per-page) 0) (cl-incf pages))
    
    ;; Now we have to loop through the pages
    (cl-loop for i from 1 to pages
	     do
	     (setq purl (concat url (format "&page=%s" i))
		   works-data (request-response-data
			       (request purl
				 :sync t
				 :parser 'oa--response-parser))
		   entries (append entries
				   (cl-loop for result in (plist-get works-data :results)
					    collect
					    (s-format "** ${title}
:PROPERTIES:
:ID: ${id}
:DOI: ${ids.doi}
:YEAR: ${publication_year}
:HOST_VENUE: ${primary_location.source.display_name}
:AUTHORS: ${(oa--authors result)}
:CITED_BY_COUNT: ${cited_by_count}
:END:

${(oa--elisp-get-bibtex result)}

- ${(oa--elisp-get-oa-refs result)}
- ${(oa--elisp-get-oa-related result)}
- ${(oa--elisp-get-oa-cited-by result)}

    " 'oa--replacer result)))))
    entries))


(defun oa-author (entity-id)
  "View Author for ENTITY-ID in an org-buffer."
  (let* ((buf (get-buffer-create "*OpenAlex - Author*"))
	 (data (oa--author entity-id))
	 (works-count (plist-get data :works_count))
	 (works-url (plist-get data :works_api_url))
	 (works-data (request-response-data
		      (request works-url
			:sync t
			:parser 'oa--response-parser))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (s-format "* ${display_name} ([[${oa-url}][json]])
:PROPERTIES:
:ORCID: ${orcid}
:SCOPUS: ${ids.scopus}
:WORKS_COUNT: ${works_count}
:CITED_BY_COUNT: ${cited_by_count}
:INSTITUTION: ${last_known_institution.display_name}, ${last_known_institution.country_code}
:END:

#+COLUMNS: %25ITEM %YEAR %CITED_BY_COUNT
elisp:org-columns    elisp:org-columns-quit

** Articles

#+caption: Sort
| year     | [[elisp:(oa-buffer-sort-year t)][old first]] | [[elisp:(oa-buffer-sort-year)][new first]] |
| cited by | [[elisp:(oa-buffer-sort-cited-by-count t)][low first]] | [[elisp:(oa-buffer-sort-cited-by-count)][high first]] |

"
			'oa--replacer data))
      (insert (s-join "\n" (oa--author-entries works-data works-url)))
      ;; It would be nice to have a table or two with citations by year
      (insert "\n** Bibliometrics\n\n")

      (let ((data (oa--author-entries works-data works-url))
	    (years '())
	    (cites '()))
	(cl-loop for result in data
		 do
		 (if (cdr (assoc (plist-get result :publication_year) years))
		     (setf (cdr (assoc (plist-get result :publication_year) years))
			   (+ 1 (cdr (assoc (plist-get result :publication_year) years))))
		   (push (cons (plist-get result :publication_year) 1) years)))
	(insert (format "*** Publication count by year

| year | count |
|------|-------|
%s
" (cl-loop for (year . count) in  (sort years (lambda (a b) (< (car a) (car b))))
	   concat
	   (format "| %s | %s |\n" year count))))

	;;  now we cature citations by year
	(cl-loop for result in data
		 do
		 (cl-loop for cnt in (plist-get result :counts_by_year)
			  do
			  (if (cdr (assoc (plist-get cnt :year) cites))
			      (setf (cdr (assoc (plist-get cnt :year) cites))
				    (+ (cdr (assoc (plist-get cnt :year) cites))
				       (plist-get cnt :cited_by_count)))
			    (push (cons (plist-get cnt :year)
					(plist-get cnt :cited_by_count))
				  cites))))

	(insert (format "*** Citation count by year

| year | count |
|------|-------|
%s
" (cl-loop for (year . count) in  (sort cites (lambda (a b) (< (car a) (car b))))
	   concat
	   (format "| %s | %s |\n" year count)))))
      
      (org-mode)
      (goto-char (point-min))
      (org-next-visible-heading 1))
    (pop-to-buffer buf)))


;; * Full text search


(defun oa-fulltext-search (query &optional page)
  "Perform a fulltext search on QUERY.
PAGE is optional, and loads that page of results. Defaults to 1."
  (interactive (list (read-string "Query: ")
		     (read-number "Page: ")))
  (when (null page) (setq page 1))
  (let* ((url (format "https://api.openalex.org/works?filter=fulltext.search:%s&page=%s&mailto=%s"
		      (url-hexify-string query)
		      page
		      user-mail-address))
	 (req (request url
		:sync t
		:parser #'oa--response-parser))
	 (data (request-response-data req))
	 (metadata (plist-get data :meta))
	 (count (plist-get metadata :count))
	 (per-page (plist-get metadata :per_page))
	 (npages (+ (/ count per-page) (if (= 0 (mod count per-page)) 0 1)))
	 (results (plist-get data :results))
	 (next-page (format "[[elisp:(oa-fulltext-search \"%s\" %s)][Next page: %s]]"
			    query
			    (+ page 1)
			    (+ page 1)))
	 (buf (get-buffer-create "*OpenAlex Full-text search*")))
    
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert (s-format "#+title: Full-text search: ${query}

[[elisp:(oa-fulltext-search \"${query}\" ${page})]]"
			'oa--replacer data))
      (insert (s-format
	       "
${meta.count} results: Page ${meta.page} of ${(format \"%s\" npages)} ${(format \"%s\" next-page)}
\n\n"
	       'oa--replacer data))
      
      (insert
       (cl-loop for result in results concat
		(s-format "* ${title}
:PROPERTIES:
:JOURNAL: ${primary_location.source.display_name}
:AUTHOR: ${(oa--authors result)}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:DOI: ${ids.doi}
:END:

${(oa--elisp-get-bibtex result)}

- ${(oa--elisp-get-oa-refs result)}
- ${(oa--elisp-get-oa-related result)}
- ${(oa--elisp-get-oa-cited-by result)}

" 'oa--replacer result)))

      (insert next-page)
      
      (goto-char (point-min)))
    (pop-to-buffer buf)))


;; * NSF Collaborators and Other Affiliations
(defun oa-coa (orcid &optional COA-file)
  "Get a list of collaborators for the past 5 years in tab-delimited form.
This is for Table 4 in the COA_template at
https://www.nsf.gov/bfa/dias/policy/coa/coa_template.xlsx.

ORCID is a string like 0000-0003-2625-9232.

If COA-FILE is non-nil write results to that file, otherwise save
to the clipboard. You should be able to paste the results
directly into Excel.

Results are sorted in alphaphabetical order by last name.

Caveats: OpenAlex provides the name in Firstname Initial Lastname
form. I assume this can be split on spaces, and the last word is
the last name. That is not always correct, so some manual name
fixing may be required.

The Institutions are not always reliable. I use the most recent
institution if an author is listed multiple times. Sometimes this
is empty, and sometimes an author has multiple institutions
listed.

There may be duplicates for people who have different names in
OpenAlex, e.g. missing initials, differences in abbreviations,
including having a period or not.

Your name will be included, you will need to delete this manually
in the Excel sheet.

This only gets the coauthors in publications known to OpenAlex.
Recently published papers are probably missing.
"
  (interactive (list (read-string "ORCID: ")
		     (when (y-or-n-p "Save to file?")
		       (read-file-name "File: "))))
  (let* ((url (format
	       "https://api.openalex.org/works?filter=author.orcid:https://orcid.org/%s&email=%s"
	       orcid
	       user-mail-address))
	 (req (request url :sync t :parser 'oa--response-parser))
	 (data (request-response-data req))
	 (meta (plist-get data :meta))
	 (count (plist-get meta :count))
	 (per-page (plist-get meta :per_page))
	 (pages (/ count per-page))
	 (current-year (string-to-number (format-time-string "%Y" (current-time))))
	 ;; this is the first page of results
	 (results (plist-get data :results))
	 (current-authors '()))

    ;; Now we need to accumulate the rest of the results from other pages
    (when (> (mod count per-page) 0) (cl-incf pages))

    (cl-loop for i from 2 to pages
	     do
	     (setq purl (concat url (format "&page=%s" i))
		   works-data (request-response-data
			       (request purl
				 :sync t
				 :parser 'oa--response-parser))
		   results (append results (plist-get works-data :results))))
    ;; Now results is a list of your publications. We need to iterate over each
    ;; one, and accumulate author information
    (cl-loop for result in results do
	     (let ((year (plist-get result :publication_year))
		   authors)
	       (when (> year (- current-year 5))
		 (cl-loop for authorship in (plist-get result :authorships) do
			  (let* ((author (plist-get authorship :author))
				 (name (plist-get author :display_name))
				 (name-parts (mapcar #'capitalize (split-string name)))
				 (name-string (concat (car (last name-parts)) ", "
						      (string-join (butlast name-parts) " ")))
				 
				 (institutions (plist-get authorship :institutions)) 
				 (institution (plist-get (car institutions) :display_name)))
			    ;; name, institution, contact info, last-active
			    ;; we won't have contact info from openalex.
			    (push (list name-string institution "" year
					(plist-get result :publication_date))
				  current-authors))))))
    
    (setq current-authors (sort current-authors
				(lambda (a b)
				  "Sort first on name, then on year in descending order"
				  (if (string= (nth 0 a) (nth 0 b))
				      (> (nth 3 a) (nth 3 b))
				    (string< (car a) (car b))))))
    
    ;; now filter for unique authors
    (setq current-authors (cl-loop for group in (seq-group-by (lambda (x)
								(car x))
							      current-authors)	   
   				   collect (cadr group)))

    ;; Finally lets fix the year so Excel reads it correctly. I use the publication date
    (setq current-authors (cl-loop for row in current-authors 
				   collect
				   (list "A:"
					 (nth 0 row)
					 (nth 1 row)
					 (nth 2 row)
					 (nth 4 row))))

    (if COA-file
	(with-temp-file COA-file
	  (cl-loop for row in current-authors do
		   (insert (string-join (mapcar (lambda (x)
						  (format "%s" (or x "")))
						row)
					"\t")
			   "\n")))
      

      (kill-new (mapconcat (lambda (row)
			     (concat (string-join (mapcar (lambda (x)
							    (format "%s" (or x "")))
							  row)
						  "\t")
				     "\n"))
			   current-authors))
      (message "COA data on the clipboard."))))


(provide 'openalex)

;;; openalex.el ends here
