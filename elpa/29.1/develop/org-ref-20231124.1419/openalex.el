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
			   (concat "?mailto=" user-mail-address)
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

(defun oa--replacer (query object)
  "Replacer function for `s-format'.
QUERY is a string that is either a sexp for a function to
evaluate or a dot notation path to data in OBJECT. If QUERY is a
sexp, it is read and evaluated. Otherwise, the path is split, and
looked up sequentially in object.

OBJECT is a plist, usually from a Work request."
  (if (s-starts-with? "(" query)
      ;; this is a function
      (eval (read query))
    ;; just get data
    (let ((fields (s-split "\\." query))
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
:HOST: ${host_venue.display_name}
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
:HOST: ${host_venue.display_name}
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
:HOST: ${host_venue.display_name}
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
:HOST: ${host_venue.display_name}
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
  (oa--related-works (concat "doi:" (org-ref-get-doi-at-point))))


(defun oa-referenced-works ()
  "Open the side window for References from the cite at point."
  (interactive)
  (oa--referenced-works (concat "doi:" (org-ref-get-doi-at-point))))


(defun oa-cited-by-works ()
  "Open the side window for Citing works for the cite at point."
  (interactive)
  (oa--cited-by-works (concat "doi:" (org-ref-get-doi-at-point))))


(defhydra+ org-ref-citation-hydra () ("ar" oa-related-works "Related documents" :column "OpenAlex"))
(defhydra+ org-ref-citation-hydra () ("ac" oa-cited-by-works "Cited by documents" :column "OpenAlex"))
(defhydra+ org-ref-citation-hydra () ("af" oa-referenced-works "References from" :column "OpenAlex"))


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


(defun oa-author-entries (works-data url)
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
:HOST_VENUE: ${host_venue.display_name}
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

#+caption: Sort
| year     | [[elisp:(oa-buffer-sort-year t)][old first]] | [[elisp:(oa-buffer-sort-year)][new first]] |
| cited by | [[elisp:(oa-buffer-sort-cited-by-count t)][low first]] | [[elisp:(oa-buffer-sort-cited-by-count)][high first]] |

"
			'oa--replacer data))
      (insert (s-join "\n" (oa-author-entries works-data works-url)))
      (org-mode)
      (goto-char (point-min))
      (org-next-visible-heading 1))
    (pop-to-buffer buf)))


(provide 'openalex)

;;; openalex.el ends here
