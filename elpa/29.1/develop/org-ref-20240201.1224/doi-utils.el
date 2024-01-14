;;; doi-utils.el --- DOI utilities for making bibtex entries

;; Copyright (C) 2015-2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((org-ref))

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

;; This package provides functionality to download PDFs and bibtex entries from
;; a DOI, as well as to update a bibtex entry from a DOI.  It depends slightly
;; on org-ref, to determine where to save pdf files too, and where to insert
;; bibtex entries in the default bibliography.

;; The principle commands you will use from here are:

;; - doi-utils-get-bibtex-entry-pdf with the cursor in a bibtex entry.
;; - doi-utils-insert-bibtex-entry-from-doi to insert a bibtex entry at your cursor, clean it and try to get a pdf.
;; - doi-utils-add-bibtex-entry-from-doi to add an entry to your default bibliography (cleaned with pdf if possible).
;; - doi-utils-update-bibtex-entry-from-doi with cursor in an entry to update its fields.

;;; Code:

(defvar url-http-end-of-headers)
(declare-function org-ref-find-bibliography "org-ref-core")
(declare-function org-ref-clean-bibtex-entry "org-ref-core")
(declare-function bibtex-completion-edit-notes "bibtex-completion")
(declare-function org-bibtex-yank "org-bibtex")
(declare-function org-ref-possible-bibfiles "org-ref-core")

(eval-when-compile
  (require 'cl-lib))
(require 'bibtex)
(require 'dash)
(require 'json)
(require 'org)                          ; org-add-link-type

(or (require 'ol-bibtex nil t)
    (require 'org-bibtex)) ; org-bibtex-yank

(require 'url-http)
(require 'url-handlers)
(require 'org-ref-utils)
(require 'hydra)

;;* Customization
(defgroup doi-utils nil
  "Customization group for doi-utils."
  :tag "DOI utils"
  :group 'doi-utils)


(defcustom doi-utils-download-pdf
  t
  "Try to download PDFs when adding bibtex entries when non-nil."
  :type 'boolean
  :group 'doi-utils)

(defcustom doi-utils-open-pdf-after-download
  nil
  "Open PDF after adding bibtex entries."
  :type 'boolean
  :group 'doi-utils)


(defcustom doi-utils-timestamp-field
  "DATE_ADDED"
  "The bibtex field to store the date when an entry has been added."
  :type 'string
  :group 'doi-utils)

(defcustom doi-utils-timestamp-format-function
  'current-time-string
  "The function to format the timestamp for a bibtex entry.
Set to a function that returns nil to avoid setting timestamps in the entries.
e.g. (lambda () nil)"
  :type 'function
  :group 'doi-utils)


(defcustom doi-utils-dx-doi-org-url
  "https://doi.org/"
  "Base url to retrieve doi metadata from. A trailing / is required."
  :type 'string
  :group 'doi-utils)


(defcustom doi-utils-metadata-function 'doi-utils-get-json-metadata
  "Function for retrieving json metadata from `doi-utils-dx-doi-org-url'.
The default is `doi-utils-get-json-metadata', but it sometimes
fails with a proxy. An alternative is
`doi-utils-get-json-metadata-curl' which requires an external
program to use curl."
  :type 'function
  :group 'doi-utils)


(defcustom doi-utils-async-download t
  "Use `doi-utils-async-download-pdf' to get pdfs asynchrounously.
If nil use `doi-utils-get-bibtex-entry-pdf' synchronously."
  :type 'boolean
  :group 'doi-utils)


;;* Getting pdf files from a DOI

;; The idea here is simple. When you visit http://dx.doi.org/doi or
;; https://doi.org/doi, you get redirected to the journal site. Once you have
;; the url for the article, you can usually compute the url to the pdf, or find
;; it in the page. Then you simply download it.

;; There are some subtleties in doing this that are described here. To get the
;; redirect, we have to use url-retrieve, and a callback function. The callback
;; does not return anything, so we communicate through global variables.
;; url-retrieve is asynchronous, so we have to make sure to wait for it to
;; finish.

(defvar *doi-utils-waiting* t
  "Stores waiting state for url retrieval.")

(defvar *doi-utils-redirect* nil
  "Stores redirect url from a callback function.")

(defun doi-utils-redirect-callback (&optional status)
  "Callback for `url-retrieve' to set the redirect.
Optional argument STATUS Unknown why this is optional."
  (when (plist-get status :error)
    (signal (car (plist-get status :error)) (cdr(plist-get status :error))))
  (when (plist-get status :redirect) ;  is nil if there none
    (setq *doi-utils-redirect* (plist-get status :redirect)))
  ;; we have done our job, so we are not waiting any more.
  (setq *doi-utils-waiting* nil))

;; To actually get the redirect we use url-retrieve like this.

(defun doi-utils-get-redirect (doi)
  "Get redirect url from `doi-utils-dx-doi-org-url'/doi."
  ;; we are going to wait until the url-retrieve is done
  (setq *doi-utils-waiting* t)
  ;; start with no redirect. it will be set in the callback.
  (setq *doi-utils-redirect* nil)
  (url-retrieve
   (format "%s%s" doi-utils-dx-doi-org-url doi)
   'doi-utils-redirect-callback)
  ;; I suspect we need to wait here for the asynchronous process to
  ;; finish. we loop and sleep until the callback says it is done via
  ;; `*doi-utils-waiting*'. this works as far as i can tell. Before I
  ;; had to run this a few times to get it to work, which i suspect
  ;; just gave the first one enough time to finish.
  (while *doi-utils-waiting* (sleep-for 0.1)))

;; Once we have a redirect for a particular doi, we need to compute the url to
;; the pdf. We do this with a series of functions. Each function takes a single
;; argument, the redirect url. If it knows how to compute the pdf url it does,
;; and returns it. We store the functions in a variable:

(defvar doi-utils-pdf-url-functions nil
  "Functions that return a url to a pdf from a redirect url.
Each function takes one argument, the redirect url.  The function
must return a pdf-url, or nil.")


;;** APS journals

(defun aps-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s*\\)://journals.aps.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/abstract/" "/pdf/" *doi-utils-redirect*)))


;;** Science

(defun science-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://www.sciencemag.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf")))


;;** Nature

(defun nature-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://www.nature.com" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".pdf")))


;;** Elsevier/ScienceDirect
;; You cannot compute these pdf links; they are embedded in the redirected pages.

(defvar *doi-utils-pdf-url* nil
  "Stores url to pdf download from a callback function.")

;;** Wiley
;; Wiley have changed the url structure from
;; http://onlinelibrary.wiley.com/doi/10.1002/anie.201402680/abstract
;; http://onlinelibrary.wiley.com/doi/10.1002/anie.201402680/pdf
;; to
;; http://onlinelibrary.wiley.com/doi/abs/10.1002/anie.201402680
;; http://onlinelibrary.wiley.com/doi/pdf/10.1002/anie.201402680
;; Hence fewer steps are now required.

;; https://onlinelibrary.wiley.com/doi/10.1002/adts.202200926
;; https://onlinelibrary.wiley.com/doi/epdf/10.1002/adts.202200926

;; (defun wiley-pdf-url (*doi-utils-redirect*)
;;   "Get url to the pdf from *DOI-UTILS-REDIRECT*."
;;   (when (string-match "^http\\(s?\\)://onlinelibrary.wiley.com" *doi-utils-redirect*)
;;     (replace-regexp-in-string "doi/abs" "doi/pdf" *doi-utils-redirect*)))


(defun wiley-pdf-url-2 (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*.
[2023-04-10 Mon] updated a new rule.
https://onlinelibrary.wiley.com/doi/pdfdirect/10.1002/anie.201310461?download=true"
  (when (string-match "^http\\(s?\\)://onlinelibrary.wiley.com" *doi-utils-redirect*)
    (concat
     (replace-regexp-in-string "doi/" "doi/pdfdirect/" *doi-utils-redirect*)
     "?download=true")))


(defun agu-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "https://agupubs.onlinelibrary.wiley.com"
		      *doi-utils-redirect*)
    (replace-regexp-in-string "/full/" "/pdfdirect/" *doi-utils-redirect*)))


;;** Springer

(defun springer-chapter-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http\\(s?\\)://link.springer.com/chapter/" *doi-utils-redirect*)
    (replace-regexp-in-string "/chapter" "/content/pdf"
			      (concat *doi-utils-redirect* ".pdf"))))


(defun springer-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://link.springer.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/article/" "/content/pdf/"
			      (concat *doi-utils-redirect* ".pdf"))))


;;** ACS
;; here is a typical url http://pubs.acs.org/doi/abs/10.1021/nl500037x
;; the pdf is found at http://pubs.acs.org/doi/pdf/10.1021/nl500037x

;; we just change /abs/ to /pdf/.

(defun acs-pdf-url-1 (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://pubs.acs.org/doi/abs/" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/" "/pdf/" *doi-utils-redirect*)))

;; 1/20/2016 I noticed this new pattern in pdf urls, where there is no abs in
;; the url
(defun acs-pdf-url-2 (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://pubs.acs.org/doi/" *doi-utils-redirect*)
    (replace-regexp-in-string "/doi/" "/doi/pdf/" *doi-utils-redirect*)))

;; 1/18/2019: It looks like they are using https now
(defun acs-pdf-url-3 (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^https://pubs.acs.org/doi/" *doi-utils-redirect*)
    (replace-regexp-in-string "/doi/" "/doi/pdf/" *doi-utils-redirect*)))


;;** IOP

(defun iop-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://iopscience.iop.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* "/pdf")))

;;** JSTOR

(defun jstor-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://www.jstor.org" *doi-utils-redirect*)
    (concat (replace-regexp-in-string "/stable/" "/stable/pdfplus/" *doi-utils-redirect*) ".pdf")))


;;** AIP

(defun aip-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://scitation.aip.org" *doi-utils-redirect*)
    ;; get stuff after content
    (let (p1 p2 s p3)
      (setq p2 (replace-regexp-in-string
                "^http\\(s?\\)://scitation.aip.org/" "" *doi-utils-redirect*))
      (setq s (split-string p2 "/"))
      (setq p1 (mapconcat 'identity (-remove-at-indices '(0 6) s) "/"))
      (setq p3 (concat "/" (nth 0 s) (nth 1 s) "/" (nth 2 s) "/" (nth 3 s)))
      (format "http://scitation.aip.org/deliver/fulltext/%s.pdf?itemId=/%s&mimeType=pdf&containerItemId=%s"
              p1 p2 p3))))

(defun aip-pdf-url-2 (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  ;; [2021-08-28 Sat] Seems like they changed the link a little?
  ;; https://aip.scitation.org/doi/10.1063/1.5019667
  ;; to
  ;; https://aip.scitation.org/doi/pdf/10.1063/1.5019667
  (when (string-match "^http\\(s?\\)://aip.scitation.org" *doi-utils-redirect*)
    (concat "https://aip.scitation.org/doi/pdf" (cl-second (split-string *doi-utils-redirect* "doi")))))

;;** Taylor and Francis

(defun tandfonline-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://www.tandfonline.com" *doi-utils-redirect*)
    (replace-regexp-in-string "/abs/\\|/full/" "/pdf/" *doi-utils-redirect*)))

;;** ECS

(defun ecs-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://jes.ecsdl.org" *doi-utils-redirect*)
    (replace-regexp-in-string "\.abstract$" ".full.pdf" *doi-utils-redirect*)))

;; http://ecst.ecsdl.org/content/25/2/2769
;; http://ecst.ecsdl.org/content/25/2/2769.full.pdf


(defun ecst-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://ecst.ecsdl.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf")))



;;** RSC

(defun rsc-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://pubs.rsc.org" *doi-utils-redirect*)
    (let ((url (downcase *doi-utils-redirect*)))
      (setq url (replace-regexp-in-string "articlelanding" "articlepdf" url))
      url)))

;;** Science Direct

;; https://www.sciencedirect.com/science/article/pii/S001085452200577X?via%3Dihub
;; https://www.sciencedirect.com/science/article/pii/S001085452200577X/pdfft?isDTMRedir=true&download=true

(defun science-direct-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://www.sciencedirect.com" *doi-utils-redirect*)
    (replace-string "?via%3Dihub" "/pdfft?isDTMRedir=true&download=true" *doi-utils-redirect*)))

;; (defun doi-utils-get-science-direct-pdf-url (redirect-url)
;;   "Science direct hides the pdf url in html.  We get it out here.
;; REDIRECT-URL is where the pdf url will be in."
;;   (let ((first-url
;;          (with-current-buffer (url-retrieve-synchronously redirect-url)
;;            (goto-char (point-min))
;;            (when (re-search-forward "pdf_url\" content=\"\\([^\"]*\\)\"" nil t)
;;              (match-string-no-properties 1)))))
;;     (and first-url
;;          (with-current-buffer (url-retrieve-synchronously first-url)
;;            (goto-char (point-min))
;;            (when (re-search-forward "or click <a href=\"\\([^\"]*\\)\">" nil t)
;;              (match-string-no-properties 1))))))

;; (defun science-direct-pdf-url (*doi-utils-redirect*)
;;   "Get url to the pdf from *DOI-UTILS-REDIRECT*."
;;   (when (string-match "^http\\(s?\\)://www.sciencedirect.com" *doi-utils-redirect*)
;;     (doi-utils-get-science-direct-pdf-url *doi-utils-redirect*)))

;; sometimes I get
;; http://linkinghub.elsevier.com/retrieve/pii/S0927025609004558
;; which actually redirect to
;; http://www.sciencedirect.com/science/article/pii/S0927025609004558

;; https://www.sciencedirect.com/science/article/pii/S001085452200577X?via%3Dihub
;; https://www.sciencedirect.com/science/article/pii/S001085452200577X/pdfft?isDTMRedir=true&download=true

;; (defun linkinghub-elsevier-pdf-url (*doi-utils-redirect*)
;;   "Get url to the pdf from *DOI-UTILS-REDIRECT*."
;;   (when (string-match
;; 	 "^https://linkinghub.elsevier.com/retrieve" *doi-utils-redirect*)
;;     (science-direct-pdf-url
;;      (replace-regexp-in-string
;;       ;; change URL to science direct and use function to get pdf URL
;;       "https://linkinghub.elsevier.com/retrieve"
;;       "https://www.sciencedirect.com/science/article"
;;       *doi-utils-redirect*))))

;; https://www.sciencedirect.com/science/article/pii/S1385894723014973/pdfft?isDTMRedir=true&download=true

(defun linkinghub-elsevier-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match
	 "^https://linkinghub.elsevier.com/retrieve" *doi-utils-redirect*)
    (concat
     (replace-regexp-in-string
      ;; change URL to science direct and use function to get pdf URL
      "https://linkinghub.elsevier.com/retrieve"
      "https://www.sciencedirect.com/science/article"
      *doi-utils-redirect*)
     "/pdfft?isDTMRedir=true")))

;;** PNAS
;; http://www.pnas.org/content/early/2014/05/08/1319030111
;; http://www.pnas.org/content/early/2014/05/08/1319030111.full.pdf

;; with supporting info
;; http://www.pnas.org/content/early/2014/05/08/1319030111.full.pdf+html?with-ds=yes

(defun pnas-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://www.pnas.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf?with-ds=yes")))


;;** Copernicus Publications
(defvar copernicus-journal-urls '(
                                  "^https://www.adv-geosci.net/"
                                  "^https://www.adv-radio-sci.net/"
                                  "^https://www.adv-sci-res.net/"
                                  "^https://www.adv-stat-clim-meteorol-oceanogr.net/"
                                  "^https://www.ann-geophys.net/"
                                  "^https://www.arch-anim-breed.net/"
                                  "^https://www.astra-proc.net/"
                                  "^https://www.atmos-chem-phys.net/"
                                  "^https://www.atmos-chem-phys-discuss.net/"
                                  "^https://www.atmos-meas-tech.net/"
                                  "^https://www.atmos-meas-tech-discuss.net/"
                                  "^https://www.biogeosciences.net/"
                                  "^https://www.biogeosciences-discuss.net/"
                                  "^https://www.clim-past.net/recent_papers.html"
                                  "^https://www.clim-past-discuss.net/"
                                  "^https://www.drink-water-eng-sci.net/"
                                  "^https://www.drink-water-eng-sci-discuss.net/"
                                  "^https://www.eg-quaternary-sci-j.net/"
                                  "^https://www.earth-surf-dynam.net/"
                                  "^https://www.earth-surf-dynam-discuss.net/"
                                  "^https://www.earth-syst-dynam.net/"
                                  "^https://www.earth-syst-dynam-discuss.net/"
                                  "^https://www.earth-syst-sci-data.net/"
                                  "^https://www.earth-syst-sci-data-discuss.net/"
                                  "^https://www.foss-rec.net/"
                                  "^https://www.geogr-helv.net/"
                                  "^https://www.geosci-instrum-method-data-syst.net/"
                                  "^https://www.geosci-instrum-method-data-syst-discuss.net/"
                                  "^https://www.geosci-model-dev.net/"
                                  "^https://www.geosci-model-dev-discuss.net/"
                                  "^https://www.hist-geo-space-sci.net/"
                                  "^https://www.hydrol-earth-syst-sci.net/"
                                  "^https://www.hydrol-earth-syst-sci-discuss.net/"
                                  "^https://www.j-sens-sens-syst.net/"
                                  "^https://www.mech-sci.net/"
                                  "^https://www.nat-hazards-earth-syst-sci.net/"
                                  "^https://www.nonlin-processes-geophys-discuss.net/"
                                  "^https://www.ocean-sci.net/"
                                  "^https://www.ocean-sci-discuss.net/"
                                  "^https://www.primate-biol.net/"
                                  "^https://www.proc-iahs.net/"
                                  "^https://www.sci-dril.net/"
                                  "^https://www.soil-journal.net/"
                                  "^https://www.soil-discuss.net/"
                                  "^https://www.solid-earth.net/"
                                  "^https://www.solid-earth-discuss.net/"
                                  "^https://www.stephan-mueller-spec-publ-ser.net/"
                                  "^https://www.the-cryosphere.net/"
                                  "^https://www.the-cryosphere-discuss.net/"
                                  "^https://www.web-ecol.net/"
                                  "^https://www.wind-energ-sci.net/"
                                  "^https://www.wind-energ-sci-discuss.net/"
                                  )
  "List of Copernicus URLs.")

(defun doi-utils-get-copernicus-pdf-url (redirect-url)
  "Copernicus hides the pdf url in html.  We get it out here.
REDIRECT-URL is where the pdf url will be in."
  (setq *doi-utils-waiting* t)
  (url-retrieve
   redirect-url
   (lambda (status)
     (goto-char (point-min))
     (re-search-forward "citation_pdf_url\" content=\"\\([^\"]*\\)\"" nil t)

     (setq *doi-utils-pdf-url* (match-string 1)
	   *doi-utils-waiting* nil)))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)

(defun copernicus-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."

  (car (cl-loop for copurl in copernicus-journal-urls
	        when (string-match copurl *doi-utils-redirect*)
	        collect
	        (progn (doi-utils-get-copernicus-pdf-url *doi-utils-redirect*)
	               *doi-utils-pdf-url*))))


;;** Sage
(defun sage-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://pss.sagepub.com" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf")))


;;** Journal of Neuroscience
(defun jneurosci-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://www.jneurosci.org" *doi-utils-redirect*)
    (concat *doi-utils-redirect* ".full.pdf")))

;;** Generic .full.pdf
(defun generic-full-pdf-url (*doi-utils-redirect*)
  (let ((pdf (concat *doi-utils-redirect* ".full.pdf")))
    (when (url-http-file-exists-p pdf)
      pdf)))

;;** IEEE
;; 10.1109/re.2014.6912247
;; http(s)://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6912247
;; http(s)://ieeexplore.ieee.org/ielx7/6903646/6912234/06912247.pdf
;; http(s)://ieeexplore.ieee.org/iel7/6903646/6912234/06912247.pdf?arnumber=6912247
;; <meta name="citation_pdf_url" content="http(s)://ieeexplore.ieee.org/iel7/6903646/6912234/06912247.pdf?arnumber=6912247">
;; <frame src="http(s)://ieeexplore.ieee.org/ielx7/6903646/6912234/06912247.pdf?tp=&arnumber=6912247&isnumber=6912234" frameborder=0 />
(defun ieee-pdf-url (*doi-utils-redirect*)
  "Get a url to the pdf from *DOI-UTILS-REDIRECT* for IEEE urls."
  (when (string-match "^https?://ieeexplore.ieee.org" *doi-utils-redirect*)
    (with-current-buffer (url-retrieve-synchronously *doi-utils-redirect*)
      (goto-char (point-min))
      (when (re-search-forward "<meta name=\"citation_pdf_url\" content=\"\\([[:ascii:]]*?\\)\">" nil t)
	(let ((framed-url (match-string 1)))
          (with-current-buffer (url-retrieve-synchronously framed-url)
            (goto-char (point-min))
            (when (re-search-forward "<frame src=\"\\(http[[:ascii:]]*?\\)\"" nil t)
              (match-string 1))))))))

;; At least some IEEE papers need the following new pdf-link parsing
;; Example: 10.1109/35.667413
(defun ieee2-pdf-url (*doi-utils-redirect*)
  "Get a url to the pdf from *DOI-UTILS-REDIRECT* for IEEE urls."
  (when (string-match "^https?://ieeexplore.ieee.org" *doi-utils-redirect*)
    (with-current-buffer (url-retrieve-synchronously *doi-utils-redirect*)
      (goto-char (point-min))
      (when (re-search-forward "\"pdfUrl\":\"\\([[:ascii:]]*?\\)\"" nil t)
	(let ((framed-url (match-string 1)))
          (with-current-buffer (url-retrieve-synchronously (concat "http://ieeexplore.ieee.org" framed-url))
            (goto-char (point-min))
            (when (re-search-forward "<frame src=\"\\(http[[:ascii:]]*?\\)\"" nil t)
              (match-string 1))))))))

;; Another try to get the ieee pdf
;; <iframe src="http(s)://ieeexplore.ieee.org/ielx5/8/4538127/04538164.pdf?tp=&arnumber=4538164&isnumber=4538127" frameborder=0>
(defun ieee3-pdf-url (*doi-utils-redirect*)
  "Get a url to the pdf from *DOI-UTILS-REDIRECT* for IEEE urls."
  (when (string-match "^https?://ieeexplore.ieee.org" *doi-utils-redirect*)
    (with-current-buffer (url-retrieve-synchronously *doi-utils-redirect*)
      (goto-char (point-min))
      (when (re-search-forward "\"pdfUrl\":\"\\([[:ascii:]]*?\\)\"" nil t)
	(let ((framed-url (match-string 1)))
          (with-current-buffer (url-retrieve-synchronously (concat "http://ieeexplore.ieee.org" framed-url))
            (goto-char (point-min))
            (when (re-search-forward "<iframe src=\"\\(http[[:ascii:]]*?\\)\"" nil t)
              (match-string 1))))))))

;; ACM Digital Library
;; https://dl.acm.org/doi/10.1145/1368088.1368132
(defun acm-pdf-url (*doi-utils-redirect*)
  "Get a url to the pdf from *DOI-UTILS-REDIRECT* for ACM urls."
  (when (string-match "^https?://dl.acm.org" *doi-utils-redirect*)
    (replace-regexp-in-string "doi" "doi/pdf" *doi-utils-redirect* )))

;;** Optical Society of America (OSA)
(defun osa-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^https://www.osapublishing.org" *doi-utils-redirect*)
    (replace-regexp-in-string "abstract.cfm" "viewmedia.cfm" *doi-utils-redirect* )))



;;** Publishers using Highwire Press metatags
;; For context and details, see:
;; https://webmasters.stackexchange.com/questions/72746/where-are-the-complete-set-of-highwire-press-metatags-defined

(defun highwire-pdf-url (*doi-utils-redirect*)
  "Typical URL:  http://biomechanical.asmedigitalcollection.asme.org/article.aspx?articleid=1427237

On this page the pdf might be here:     <meta name=\"citation_author\" content=\"Dalong Li\" /><meta name=\"citation_author_email\" content=\"dal40@pitt.edu\" /><meta name=\"citation_author\" content=\"Anne M. Robertson\" /><meta name=\"citation_author_email\" content=\"rbertson@pitt.edu\" /><meta name=\"citation_title\" content=\"A Structural Multi-Mechanism Damage Model for Cerebral Arterial Tissue\" /><meta name=\"citation_firstpage\" content=\"101013\" /><meta name=\"citation_doi\" content=\"10.1115/1.3202559\" /><meta name=\"citation_keyword\" content=\"Mechanisms\" /><meta name=\"citation_keyword\" content=\"Biological tissues\" /><meta name=\"citation_keyword\" content=\"Stress\" /><meta name=\"citation_keyword\" content=\"Fibers\" /><meta name=\"citation_journal_title\" content=\"Journal of Biomechanical Engineering\" /><meta name=\"citation_journal_abbrev\" content=\"J Biomech Eng\" /><meta name=\"citation_volume\" content=\"131\" /><meta name=\"citation_issue\" content=\"10\" /><meta name=\"citation_publication_date\" content=\"2009/10/01\" /><meta name=\"citation_issn\" content=\"0148-0731\" /><meta name=\"citation_publisher\" content=\"American Society of Mechanical Engineers\" /><meta name=\"citation_pdf_url\" content=\"http://biomechanical.asmedigitalcollection.asme.org/data/journals/jbendy/27048/101013_1.pdf\" />

It is in the citation_pdf_url.

It would be better to parse this, but here I just use a regexp.
"

  (when (or (string-match "^http\\(s?\\)://biomechanical.asmedigitalcollection.asme.org" *doi-utils-redirect*)
	    (string-match "^http\\(s?\\)://ojs.aaai.org" *doi-utils-redirect*)
	    (string-match "^http\\(s?\\)://aclanthology.org" *doi-utils-redirect*))
    (setq *doi-utils-waiting* 0)
    (url-retrieve
     *doi-utils-redirect*
     (lambda (status)
       (or (progn (goto-char (point-min))
		  (re-search-forward "citation_pdf_url\"? content=\"\\(.*\\)\"" nil t))
	   (progn (goto-char (point-min))
		  (re-search-forward "\"\\([^\"]*\\)\" name=\"?citation_pdf_url" nil t)))
       ;; (message-box (match-string 1))
       (setq *doi-utils-pdf-url* (match-string 1)
	     *doi-utils-waiting* nil)))
    (while (and *doi-utils-waiting* (< *doi-utils-waiting* 5))
      (setq *doi-utils-waiting* (+ *doi-utils-waiting* 0.1))
      (sleep-for 0.1))
    *doi-utils-pdf-url*))


;; Society for Industrial and Applied Mathematics (SIAM)
(defun siam-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s?\\)://epubs.siam.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/doi/" "/doi/pdf/" *doi-utils-redirect* )))

;; PLOS journals
;; https://plos.org/
(defun plos-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http\\(s*\\)://journals.plos.org" *doi-utils-redirect*)
    (concat (replace-regexp-in-string (regexp-quote "/article?id=") "/article/file?id=" *doi-utils-redirect*) "&type=printable")))


;; https://www.frontiersin.org/articles/10.3389/fchem.2022.1037997/full
;; https://www.frontiersin.org/articles/10.3389/fchem.2022.1037997/pdf
(defun frontiers-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http\\(s*\\)://www.frontiersin.org" *doi-utils-redirect*)
    (replace-regexp-in-string "/full" "/pdf" *doi-utils-redirect*)))




;; https://chemistry-europe.onlinelibrary.wiley.com/doi/10.1002/celc.201902035
;; https://chemistry-europe.onlinelibrary.wiley.com/doi/epdf/10.1002/celc.201902035
(defun chemistry-europe-pdf-url (*doi-utils-redirect*)
  (when (string-match "^http\\(s*\\)://chemistry-europe.onlinelibrary.wiley.com" *doi-utils-redirect*)
    (concat
     (replace-regexp-in-string "/doi" "/doi/pdfdirect" *doi-utils-redirect*)
     "?download=true")))


;; ** from issue #1081

(defun arxiv-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match-p "^https?://arxiv\\.org" *doi-utils-redirect*)
    (concat (replace-regexp-in-string "/abs/" "/pdf/" *doi-utils-redirect*)
	    ".pdf")))


(defun rss-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match-p "roboticsproceedings" *doi-utils-redirect*)
    (concat (replace-regexp-in-string "\\.html" ".pdf" *doi-utils-redirect*))))


(defun ieeestamp-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^https?://ieeexplore\\.ieee\\.org/document/\\([0-9]+\\)"
		      *doi-utils-redirect*)
    (concat "https://ieeexplore.ieee.org/stampPDF/getPDF.jsp?tp=&arnumber="
	    (match-string 1 *doi-utils-redirect*))))


;;** Add all functions

(setq doi-utils-pdf-url-functions
      (list
       'aps-pdf-url
       'science-pdf-url
       'nature-pdf-url
       ;; 'wiley-pdf-url
       'wiley-pdf-url-2
       'springer-chapter-pdf-url
       'springer-pdf-url
       'acs-pdf-url-1
       'acs-pdf-url-2
       'acs-pdf-url-3
       'iop-pdf-url
       'jstor-pdf-url
       'aip-pdf-url
       'aip-pdf-url-2
       'science-direct-pdf-url
       'linkinghub-elsevier-pdf-url
       'tandfonline-pdf-url
       'ecs-pdf-url
       'ecst-pdf-url
       'rsc-pdf-url
       'pnas-pdf-url
       'copernicus-pdf-url
       'sage-pdf-url
       'jneurosci-pdf-url
       'ieee-pdf-url
       'ieee2-pdf-url
       'ieee3-pdf-url
       'acm-pdf-url
       'osa-pdf-url
       'highwire-pdf-url
       'siam-pdf-url
       'agu-pdf-url
       'plos-pdf-url
       'frontiers-pdf-url
       'chemistry-europe-pdf-url 
       'generic-full-pdf-url
       'arxiv-pdf-url
       'rss-pdf-url
       'ieeestamp-pdf-url))

;;** Get the pdf url for a doi

(defun doi-utils-get-pdf-url (doi)
  "Return a url to a pdf for the DOI if one can be calculated.
Loops through the functions in `doi-utils-pdf-url-functions'
until one is found."
  (doi-utils-get-redirect doi)

  (unless *doi-utils-redirect*
    (error "No redirect found for %s" doi))
  (catch 'pdf-url
    (dolist (func doi-utils-pdf-url-functions)
      (let ((this-pdf-url (funcall func *doi-utils-redirect*)))
        (when this-pdf-url
          (throw 'pdf-url this-pdf-url))))))

;;** Finally, download the pdf
(defvar bibtex-completion-library-path)
(defvar bibtex-completion-bibliography)
(declare-function async-start "async")

;;;###autoload
(defun doi-utils-async-download-pdf ()
  "Download the PDF for bibtex entry at point asynchronously.
It is not fully async, only the download is. Fully async is
harder because you need to run `doi-utils-get-pdf-url' async
too. "
  (interactive)
  (require 'async)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (;; get doi, removing http://dx.doi.org/ if it is there.
          (doi (replace-regexp-in-string
                "https?://\\(dx.\\)?.doi.org/" ""
                (bibtex-autokey-get-field "doi")))
          (key (cdr (assoc "=key=" (bibtex-parse-entry))))
          (pdf-url)
          (pdf-file))

      (setq pdf-file
	    (concat (cond
		     ((stringp bibtex-completion-library-path)
		      bibtex-completion-library-path)
		     ((= 1 (length bibtex-completion-library-path))
		      (car bibtex-completion-library-path))
		     (t
		      (completing-read "Dir: " bibtex-completion-library-path)))
		    key ".pdf"))

      (unless doi (error "No DOI found to get a pdf for"))

      (when (file-exists-p pdf-file)
	(error "%s already exists. Delete to re-download" pdf-file))

      ;; (doi-utils-get-pdf-url "10.1063/1.5019667")
      ;; If you get here, try getting the pdf file
      (async-start
       `(lambda ()
	  (setq package-user-dir ,package-user-dir)
	  (require 'package)
	  (package-initialize)
	  (setq load-path (list ,@load-path))
	  (require 'doi-utils)

	  (setq pdf-url (doi-utils-get-pdf-url ,doi))
	  (when pdf-url
	    (url-copy-file pdf-url ,pdf-file t)

	    (let* ((header (with-temp-buffer
			     (set-buffer-multibyte nil)
			     (insert-file-contents-literally ,pdf-file nil 0 5)
			     (buffer-string)))
		   (valid (and (stringp header)
			       (string-equal (encode-coding-string header 'utf-8) "%PDF-"))))
	      (if valid
		  (format "%s downloaded" ,pdf-file)
		(delete-file ,pdf-file)
		(require 'browse-url)
		(browse-url pdf-url)
		(message "Invalid pdf (file deleted). Header = %s" header)))))
       `(lambda (result)
	  (message "doi-utils-async-download-pdf: %s"  result))))))


;;;###autoload
(defun doi-utils-get-bibtex-entry-pdf (&optional arg)
  "Download pdf for entry at point if the pdf does not already exist locally.
The entry must have a doi. The pdf will be saved, by the name
%s.pdf where %s is the bibtex label. Files will not be
overwritten. The pdf will be checked to make sure it is a pdf,
and not some html failure page. You must have permission to
access the pdf. We open the pdf at the end if
`doi-utils-open-pdf-after-download' is non-nil.

With one prefix ARG, directly get the pdf from a file (through
`read-file-name') instead of looking up a DOI. With a double
prefix ARG, directly get the pdf from an open buffer (through
`read-buffer-to-switch') instead. These two alternative methods
work even if the entry has no DOI, and the pdf file is not
checked."
  (interactive "P")
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (;; get doi, removing http://dx.doi.org/ if it is there.
          (doi (replace-regexp-in-string
                "https?://\\(dx.\\)?.doi.org/" ""
                (bibtex-autokey-get-field "doi")))
          (key (cdr (assoc "=key=" (bibtex-parse-entry))))
          (pdf-url)
          (pdf-file))

      (setq pdf-file
	    (concat (cond
		     ((stringp bibtex-completion-library-path)
		      bibtex-completion-library-path)
		     ((= 1 (length bibtex-completion-library-path))
		      (car bibtex-completion-library-path))
		     (t
		      (completing-read "Dir: " bibtex-completion-library-path)))
		    key ".pdf"))
      ;; now get file if needed.
      (unless (file-exists-p pdf-file)
	(cond
	 ((and (not arg)
	       doi
	       (setq pdf-url (doi-utils-get-pdf-url doi)))
	  (url-copy-file pdf-url pdf-file)
	  ;; now check if we got a pdf
          (if (org-ref-pdf-p pdf-file)
              (message "%s saved" pdf-file)
            (delete-file pdf-file)
            (message "No pdf was downloaded.")
            (browse-url pdf-url)))
	 ((equal arg '(4))
	  (copy-file (expand-file-name (read-file-name "Pdf file: " nil nil t))
		     pdf-file))
	 ((equal arg '(16))
	  (with-current-buffer (read-buffer-to-switch "Pdf buffer: ")
	    (write-file pdf-file)))
	 (t
	  (message "We don't have a recipe for this journal.")))

	(when (file-exists-p pdf-file)
	  (bibtex-set-field "file" pdf-file))

	(when (and doi-utils-open-pdf-after-download (file-exists-p pdf-file))
	  (org-open-file pdf-file))))))

;;* Getting bibtex entries from a DOI

;; [[http://homepages.see.leeds.ac.uk/~eeaol/notes/2013/02/doi-metadata/][found]]
;; you can download metadata about a DOI from http://dx.doi.org. You just have
;; to construct the right http request to get it. Here is a function that gets
;; the metadata as a plist in emacs.
;;
;;
(defvar doi-utils-cache nil
  "Cache variable for storing data we can reuse.
A-list (doi . data) where doi is doi string, and data is what is
retrieved from it. This is transient, and disappears when you
restart Emacs. This mostly exists to prevent
`doi-utils-update-field' from needing to download the data for
every field.")


(defun doi-utils-clear-cache ()
  "Clear `doi-utils-cache'."
  (interactive)
  (setq doi-utils-cache '()))


(defun doi-utils-get-json-metadata (doi)
  "Try to get json metadata for DOI.  Open the DOI in a browser if we do not get it."
  (if-let ((data (cdr (assoc doi doi-utils-cache))))
      ;; We have the data already, so we return it.
      data
    (let ((url-request-method "GET")
          (url-mime-accept-string "application/citeproc+json")
          (json-object-type 'plist)
          (json-data)
	  (url (concat doi-utils-dx-doi-org-url doi)))
      (with-temp-buffer
	(url-insert
	 (url-retrieve-synchronously url))
	(setq json-data (buffer-string))

	(when (or (string-match "<title>Error: DOI Not Found</title>" json-data)
		  (string-match "Resource not found" json-data)
		  (string-match "Status *406" json-data)
		  (string-match "400 Bad Request" json-data))
	  (browse-url (concat doi-utils-dx-doi-org-url doi))
	  (error "Something went wrong.  We got this response:
%s

Opening %s" json-data url))

	(setq data (json-read-from-string json-data))
	(cl-pushnew (cons doi data) doi-utils-cache)
	data))))


(defun doi-utils-get-json-metadata-curl (doi)
  "Try to get json metadata for DOI.  Open the DOI in a browser if we do not get it."
  (let ((json-object-type 'plist)
        (json-data)
	(url (concat doi-utils-dx-doi-org-url doi)))
    (with-temp-buffer
      (call-process "curl" nil t nil
                    "--location"
                    "--silent"
                    "--header"
                    "Accept: application/citeproc+json"
                    url)
      (setq json-data (buffer-string))
      (cond
       ((or (string-match "<title>Error: DOI Not Found</title>" json-data)
	    (string-match "Resource not found" json-data)
	    (string-match "Status *406" json-data)
	    (string-match "400 Bad Request" json-data))
	(browse-url url)
	(error "Something went wrong.  We got this response:
%s
Opening %s" json-data url))
       ;; everything seems ok with the data
       (t
	(json-read-from-string json-data))))))

;; We can use that data to construct a bibtex entry. We do that by defining a
;; template, and filling it in. I wrote this template expansion code which
;; makes it easy to substitute values like %{} in emacs lisp.


(defun doi-utils-expand-template (s)
  "Expand a string template S containing %{} with the eval of its contents."
  (replace-regexp-in-string "%{\\([^}]+\\)}"
			    (lambda (arg)
			      (let ((sexp (substring arg 2 -1)))
				(format "%s" (eval (read sexp)))))
			    s))


;; Now we define a function that fills in that template from the metadata.

;; As different bibtex types share common keys, it is advantageous to separate
;; data extraction from json, and the formatting of the bibtex entry.

;; We use eval-and-compile because we use the three following forms in the
;; `doi-utils-def-bibtex-type' macro.  Since the macro is expanded at compile
;; time, we need to ensure these defuns and defvars are evaluated at
;; compile-time.
(eval-and-compile
  (defvar doi-utils-json-metadata-extract
    '((type       (plist-get results :type))
      (author     (mapconcat (lambda (x)
			       (message "%s" x)
			       (if (plist-get x :name)
				   (plist-get x :name)
				 (concat (plist-get x :given) " " (plist-get x :family))))
                             (plist-get results :author) " and "))
      (title      (plist-get results :title))
      (subtitle   (plist-get results :subtitle))
      (journal    (plist-get results :container-title))
      (series     (plist-get results :container-title))
      (publisher  (plist-get results :publisher))
      (volume     (plist-get results :volume))
      (issue      (plist-get results :issue))
      (number     (plist-get results :issue))
      (year       (or (elt (elt (plist-get (plist-get results :issued) :date-parts) 0) 0)
                      (elt (elt (plist-get (plist-get results :approved) :date-parts) 0) 0)
                      ))
      ;; Some dates don't have a month in them.
      (month      (let ((date (elt
			       (plist-get (plist-get results :issued) :date-parts) 0)))
		    (if (>= (length date) 2)
			(elt date 1)
		      "-")))
      (pages      (or (plist-get results :page)
		      (plist-get results :article-number)))
      (doi        (plist-get results :DOI))
      (url        (plist-get results :URL))
      (booktitle  (plist-get results :container-title))
      (school     (or (plist-get results :school)
                      (plist-get (plist-get results :institution) :name)))))

  ;; Next, we need to define the different bibtex types. Each type has a bibtex
  ;; type (for output) and the type as provided in the doi record. Finally, we
  ;; have to declare the fields we want to output.

  (defvar doi-utils-bibtex-type-generators nil)

  (defun doi-utils-concat-prepare (lst &optional acc)
    "Minimize the number of args passed to `concat' from LST.
Given a list LST of strings and other expressions, which are
intended to be passed to `concat', concat any subsequent strings,
minimising the number of arguments being passed to `concat'
without changing the results.  ACC is the list of additional
expressions."
    (cond ((null lst) (nreverse acc))
          ((and (stringp (car lst))
                (stringp (car acc)))
           (doi-utils-concat-prepare (cdr lst) (cons (concat (car acc) (car lst))
                                                     (cdr acc))))
          (t (doi-utils-concat-prepare (cdr lst) (cons (car lst) acc))))))

(defmacro doi-utils-def-bibtex-type (name matching-types &rest fields)
  "Define a BibTeX type identified by (symbol) NAME.
MATCHING-TYPES is a list of strings.  FIELDS are symbols that
match to retrieval expressions in
`doi-utils-json-metadata-extract'.  This type will only be used
when the `:type' parameter in the JSON metadata is contained in
MATCHING-TYPES."
  `(push (lambda (type results)
           (when
               (or ,@(mapcar
                      (lambda (match-type)
                        `(string= type ,match-type)) matching-types))
             (let ,(mapcar (lambda (field)
                             (let ((field-expr
                                    (assoc field doi-utils-json-metadata-extract)))
                               (if field-expr
                                   ;; need to convert to string first
                                   `(,(car field-expr) (format "%s" ,(cadr field-expr)))
                                 (error "Unknown bibtex field type %s" field))))
                           fields)
               (concat
                ,@(doi-utils-concat-prepare
                   (-flatten
                    (list (concat "@" (symbol-name name) "{,\n")
                          ;; there seems to be some bug with mapcan,
                          ;; so we fall back to flatten
                          (mapcar (lambda (field)
                                    `("  " ,(symbol-name field) " = {" ,field "},\n"))
                                  fields)
                          "}\n")))))))
         doi-utils-bibtex-type-generators))

(doi-utils-def-bibtex-type article ("journal-article" "article-journal" "article")
                           author title journal year volume number pages doi url)

(doi-utils-def-bibtex-type inproceedings ("proceedings-article" "paper-conference")
                           author title booktitle year month pages doi url)

(doi-utils-def-bibtex-type book ("book")
                           author title series publisher year pages doi url)

(doi-utils-def-bibtex-type inbook ("chapter" "book-chapter" "reference-entry")
                           author title booktitle series publisher year pages doi url)
(doi-utils-def-bibtex-type phdthesis ("phdthesis" "thesis" "dissertation")
                  author title school publisher year)

;; this is what preprints in chemrxiv look like for now
(doi-utils-def-bibtex-type misc ("posted-content")
			   author title year doi url)



;; With the code generating the bibtex entry in place, we can glue it to the json retrieval code.

(defun doi-utils-doi-to-bibtex-string (doi)
  "Return a bibtex entry as a string for the DOI.  Not all types are supported yet."
  (let* ((results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type)))
    ;; (format "%s" results) ; json-data
    (or (-some (lambda (g) (funcall g type results)) doi-utils-bibtex-type-generators)
        (message "%s not supported yet\n%S." type results))))

;; That is just the string for the entry. To be useful, we need a function that
;; inserts the string into a buffer. This function will insert the string at the
;; cursor, clean the entry, try to get the pdf.

(defun doi-utils-insert-bibtex-entry-from-doi (doi)
  "Insert and clean bibtex entry from a DOI."
  (insert (doi-utils-doi-to-bibtex-string doi))
  (backward-char)
  ;; set date added for the record
  (let ((ts (funcall doi-utils-timestamp-format-function)))
    (when ts
      (bibtex-set-field doi-utils-timestamp-field
			ts)))
  (org-ref-clean-bibtex-entry)
  (when (buffer-file-name)
      (save-buffer)))


;;;###autoload
(defun doi-utils-add-bibtex-entry-from-doi (doi &optional bibfile)
  "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in .  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the initial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use."
  (interactive
   (list (read-string
          "DOI: "
          ;; now set initial input
          (doi-utils-maybe-doi-from-region-or-current-kill))))

  (unless bibfile
    (setq bibfile (completing-read "Bibfile: " (org-ref-possible-bibfiles))))
  ;; Wrap in save-window-excursion to restore your window arrangement after this
  ;; is done.
  (save-window-excursion
    (with-current-buffer
        (find-file-noselect bibfile)
      ;; Check if the doi already exists
      (goto-char (point-min))
      (if (re-search-forward (concat doi "\\_>") nil t)
          (message "%s is already in this file" doi)
        (goto-char (point-max))

	(when (not (looking-back "\n\n" (min 3 (point))))
	  (insert "\n\n"))

        (doi-utils-insert-bibtex-entry-from-doi doi)
        (save-buffer)))))

(defalias 'doi-add-bibtex-entry 'doi-utils-add-bibtex-entry-from-doi
  "Alias function for convenience.")

(defun doi-utils-maybe-doi-from-region-or-current-kill ()
  "Try to get a DOI from the active region or current kill."
  (let* ((the-active-region (if (region-active-p) ;; nil if no active region
                                (buffer-substring (region-beginning) (region-end))
                              nil))
         (the-current-kill (ignore-errors (current-kill 0 t)))  ;; nil if empty kill ring
         ;; DOI urls
         ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
         ;; Ex: https://dx.doi.org/10.1007/978-3-319-60876-1_10
         (doi-url-prefix-regexp "^https?://\\(dx\\.\\)?doi\\.org/")
         ;; https://www.crossref.org/blog/dois-and-matching-regular-expressions/
         (doi-regexp "10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+$"))
    (cond
     ;; Check if a DOI can be found in the active region
     ;; DOI raw
     ;; Ex: 10.1109/MALWARE.2014.6999410
     ((and (stringp the-active-region)
           (s-match (concat "^" doi-regexp) the-active-region))
      the-active-region)
     ;; DOI url
     ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
     ((and (stringp the-active-region)
           (s-match (concat doi-url-prefix-regexp doi-regexp) the-active-region))
      (replace-regexp-in-string doi-url-prefix-regexp "" the-active-region))
     ;; DOI url as customized
     ((and (stringp the-active-region)
           (s-match (regexp-quote doi-utils-dx-doi-org-url) the-active-region))
      (replace-regexp-in-string (regexp-quote doi-utils-dx-doi-org-url) "" the-active-region))
     ;; Check if DOI can be found in the current kill
     ;; DOI raw
     ;; Ex: 10.1109/MALWARE.2014.6999410
     ((and (stringp the-current-kill)
           (s-match (concat "^" doi-regexp) the-current-kill))
      the-current-kill)
     ;; DOI url
     ;; Ex: https://doi.org/10.1109/MALWARE.2014.6999410
     ((and (stringp the-current-kill)
           (s-match (concat doi-url-prefix-regexp doi-regexp) the-current-kill))
      (replace-regexp-in-string doi-url-prefix-regexp "" the-current-kill))
     ;; DOI url as customized
     ((and (stringp the-current-kill)
           (s-match (regexp-quote doi-utils-dx-doi-org-url) the-current-kill))
      (replace-regexp-in-string (regexp-quote doi-utils-dx-doi-org-url) "" the-current-kill))
     ;; otherwise, return nil
     (t
      nil))))

;;;###autoload
(defun doi-utils-doi-to-org-bibtex (doi)
  "Convert a DOI to an ‘org-bibtex’ form and insert it at point."
  (interactive "sDOI: ")
  (with-temp-buffer
    (insert (doi-utils-doi-to-bibtex-string doi))
    (bibtex-clean-entry)
    (kill-region (point-min) (point-max)))
  (org-bibtex-yank)
  (org-metaright)
  (org-metaright))

;;* Updating bibtex entries

;; I wrote this code because it is pretty common for me to copy bibtex entries
;; from ASAP articles that are incomplete, e.g. no page numbers because it is
;; not in print yet. I wanted a convenient way to update an entry from its DOI.
;; Basically, we get the metadata, and update the fields in the entry.

;; There is not bibtex set field function, so I wrote this one.


;;;###autoload
(defun bibtex-set-field (field value &optional nodelim)
  "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'."
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
        ;; we found a field
        (progn
          (goto-char (car (cdr found)))
          (when value
            (bibtex-kill-field)
            (bibtex-make-field field nil nil nodelim)
            (backward-char)
            (insert value)))
      ;; make a new field
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field nil nil nodelim)
      (backward-char)
      (insert value))))


(defun plist-get-keys (plist)
  "Return keys in a PLIST."
  (-slice plist 0 nil 2))


;;;###autoload
(defun doi-utils-update-bibtex-entry-from-doi (doi)
  "Update fields in a bibtex entry from the DOI.
Every field will be updated, so previous change will be lost."
  (interactive (list
                (or (replace-regexp-in-string
                     "https?://\\(dx.\\)?doi.org/" ""
                     (bibtex-autokey-get-field "doi"))
                    (read-string "DOI: "))))
  (let* ((results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type))
         (author (mapconcat
                  (lambda (x)
		    ;; There are two possible ways an author is named. The most
		    ;; common is with :given and :family, but sometimes there is
		    ;; :name instead.
		    (if (plist-get x :name)
			(plist-get x :name)
		      (concat (plist-get x :given)
			      " " (plist-get x :family))))
                  (plist-get results :author) " and "))
         (title (plist-get results :title))
         (journal (plist-get results :container-title))
         (year (format "%s"
                       (elt
                        (elt
                         (plist-get
                          (plist-get results :issued) :date-parts) 0) 0)))
         (volume (plist-get results :volume))
         (number (or (plist-get results :issue) ""))
         (pages (or (plist-get results :page) ""))
         (url (or (plist-get results :URL) ""))
         (doi (plist-get results :DOI))
         mapping)

    ;; map the json fields to bibtex fields. The code each field is mapped to is
    ;; evaluated.
    (setq mapping '((:author . (bibtex-set-field "author" author))
                    (:title . (bibtex-set-field "title" title))
                    (:container-title . (bibtex-set-field "journal" journal))
                    (:issued . (bibtex-set-field "year" year))
                    (:volume . (bibtex-set-field "volume" volume))
                    (:issue . (bibtex-set-field "number" number))
                    (:page . (bibtex-set-field "pages" pages))
                    (:DOI . (bibtex-set-field "doi" doi))
                    (:URL . (bibtex-set-field "url" url))))

    ;; now we have code to run for each entry. we map over them and evaluate the code
    (mapc
     (lambda (key)
       (eval (cdr (assoc key mapping))))
     (plist-get-keys results)))

  (org-ref-clean-bibtex-entry))


;; A downside to updating an entry is it overwrites what you have already fixed.
;; So, we next develop a function to update the field at point.


;;;###autoload
(defun doi-utils-update-field ()
  "Update the field at point in the bibtex entry.
Data is retrieved from the doi in the entry."
  (interactive)
  (let* ((doi (bibtex-autokey-get-field "doi"))
         (results (funcall doi-utils-metadata-function doi))
         (field (car (bibtex-find-text-internal nil nil ","))))
    (cond
     ((string= field "volume")
      (bibtex-set-field field (plist-get results :volume)))
     ((string= field "number")
      (bibtex-set-field field (plist-get results :issue)))
     ((string= field "pages")
      (bibtex-set-field field (or (plist-get results :page)
                                  (plist-get results :article-number))))
     ((string= field "year")
      (bibtex-set-field field (plist-get results :year)))
     (t
      (message "%s not supported yet." field)))))



;;* DOI functions for WOS

;; I came across this API http://wokinfo.com/media/pdf/OpenURL-guide.pdf to make
;; links to the things I am interested in here. Based on that document, here are
;; three links based on a doi:10.1021/jp047349j that take you to different Web
;; Of Science (WOS) pages.


;; 1. go to article in WOS: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/10.1021/jp047349j
;; 2. citing articles: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F10.1021/jp047349j&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes
;; 3. related articles: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F10.1021/jp047349j&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes

;; These are pretty easy to construct, so we can write functions that will
;; create them and open the url in our browser. There are some other options
;; that could be considered, but since we usually have a doi, it seems like the
;; best way to go for creating the links. Here are the functions.

;;;###autoload
(defun doi-utils-wos (doi)
  "Open Web of Science entry for DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/%s" doi)))

;;;###autoload
(defun doi-utils-wos-citing (doi)
  "Open Web of Science citing articles entry for DOI.
May be empty if none are found."
  (interactive "sDOI: ")
  (browse-url
   (concat
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
    doi
    "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes")))

;;;###autoload
(defun doi-utils-wos-related (doi)
  "Open Web of Science related articles page for DOI."
  (interactive "sDOI: ")
  (browse-url
   (concat "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
           doi
           "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes")))


;;* DOI functions for ADS

;;;###autoload
(defun doi-utils-ads (doi)
  "Open ADS entry for DOI"
  (interactive "sDOI: ")
  (browse-url
   (concat
    "https://ui.adsabs.harvard.edu/abs/" "%22" doi "%22")))


;;* A new doi link for org-mode
;; The idea is to add a menu to the doi link, so rather than just clicking to open the article, you can do other things.
;; 1. open doi
;; 2. open in wos
;; 3. open citing articles
;; 4. open related articles
;; 5. open bibtex entry
;; 6. get bibtex entry


;;;###autoload
(defun doi-utils-open (doi)
  "Open DOI in browser."
  (interactive "sDOI: ")
  (browse-url (concat doi-utils-dx-doi-org-url doi)))


;;;###autoload
(defun doi-utils-open-bibtex (doi)
  "Search through variable `bibtex-completion-bibliography' for DOI."
  (interactive "sDOI: ")
  (cl-loop for f in (if (listp bibtex-completion-bibliography)
			bibtex-completion-bibliography
		      (list bibtex-completion-bibliography))
	   when (progn (find-file f)
		       (when (search-forward doi (point-max) t)
			 (bibtex-beginning-of-entry)))
	   return f))


;;;###autoload
(defun doi-utils-crossref (doi)
  "Search DOI in CrossRef."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://search.crossref.org/?q=%s" doi)))


;;;###autoload
(defun doi-utils-google-scholar (doi)
  "Google scholar the DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://scholar.google.com/scholar?q=%s" doi)))


;;;###autoload
(defun doi-utils-pubmed (doi)
  "Search Pubmed for the DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
    (url-hexify-string doi))))

(declare-function org-element-property "org-element")

(defhydra doi-link-follow (:color blue :hint nil)
  "DOI actions:
"
  ("o" (doi-utils-open (org-element-property :path (org-element-context))) "open")
  ("w" (doi-utils-wos (org-element-property :path (org-element-context))) "wos")
  ("c" (doi-utils-wos-citing (org-element-property :path (org-element-context))) "wos citing articles")
  ("r" (doi-utils-wos-related (org-element-property :path (org-element-context))) "wos related articles" )
  ("a" (doi-utils-ads (org-element-property :path (org-element-context))) "ads")
  ("s" (doi-utils-google-scholar (org-element-property :path (org-element-context))) "Google Scholar")
  ("f" (doi-utils-crossref (org-element-property :path (org-element-context))) "CrossRef")
  ("p" (doi-utils-pubmed (org-element-property :path (org-element-context))) "Pubmed")
  ("b" (doi-utils-open-bibtex (org-element-property :path (org-element-context))) "open in bibtex")
  ("g" (doi-utils-add-bibtex-entry-from-doi (org-element-property :path (org-element-context))) "get bibtex entry"))


(org-link-set-parameters "doi"
			 :follow (lambda (_) (doi-link-follow/body))
			 :export (lambda (doi desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"%s%s\">%s</a>"
					     doi-utils-dx-doi-org-url
					     doi
					     (or desc (concat "doi:" doi))))
				    ((eq format 'latex)
				     (format "\\href{%s%s}{%s}"
					     doi-utils-dx-doi-org-url
					     doi
					     (or desc (concat "doi:" doi)))))))

;;* Getting a doi for a bibtex entry missing one

;; Some bibtex entries do not have a DOI, maybe because they were entered by
;; hand, or copied from a source that did not have it available. Here we develop
;; some functions to help you find the DOI using Crossref.

;; Here is our example bibtex entry.
;; #+BEGIN_SRC bibtex
;; @article{deml-2014-oxide,
;;   author =	 {Ann M. Deml and Vladan Stevanovi{\'c} and
;;                   Christopher L. Muhich and Charles B. Musgrave and
;;                   Ryan O'Hayre},
;;   title =	 {Oxide Enthalpy of Formation and Band Gap Energy As
;;                   Accurate Descriptors of Oxygen Vacancy Formation
;;                   Energetics},
;;   journal =	 {Energy Environ. Sci.},
;;   volume =	 7,
;;   number =	 6,
;;   pages =	 1996,
;;   year =	 2014,
;;   doi =		 {10.1039/c3ee43874k,
;;   url =		 {http://dx.doi.org/10.1039/c3ee43874k}},

;; }


;; The idea is to query Crossref in a way that is likely to give us a hit
;; relevant to the entry.

;; According to http://search.crossref.org/help/api we can send a query with a
;; free form citation that may give us something back. We do this to get a list
;; of candidates, which could be used to get the doi.

(declare-function org-ref-bib-citation "org-ref-bibtex")
;;;###autoload
(defun doi-utils-crossref-citation-query ()
  "Query Crossref with the title of the bibtex entry at point.
Get a list of possible matches. Choose one with completion."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
	 (raw-json-string)
         (json-string)
         (json-data)
         (doi (bibtex-autokey-get-field "doi")))
    (unless (string= "" doi)
      (error "Entry already has a doi field"))

    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "http://search.crossref.org/dois?q="
          (url-hexify-string (org-ref-bib-citation))))
      (save-excursion
      	(goto-char (point-min))
      	(while (re-search-forward "<i>\\|</i>" nil t)
      	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "&amp;" nil t)
	  (replace-match "&"))
      	(goto-char (point-min))
      	(while (re-search-forward "&quot;" nil t)
      	  (replace-match "\\\"" nil t)))
      (setq raw-json-string (buffer-substring url-http-end-of-headers (point-max)))
      ;; decode json string
      (setq json-string (decode-coding-string (encode-coding-string raw-json-string 'utf-8) 'utf-8))
      (setq json-data (json-read-from-string json-string)))

    (let* ((name (format "Crossref hits for %s" (org-ref-bib-citation)))
           (candidates (mapcar (lambda (x)
                                 (cons
                                  (concat
                                   (cdr (assoc 'fullCitation x)))
                                  (cdr (assoc 'doi x))))
                               json-data))

	   (doi (cdr (assoc (completing-read "DOI: " candidates) candidates))))
      (bibtex-make-field "doi" t)
      (backward-char)
      ;; crossref returns doi url, but I prefer only a doi for the doi field
      (insert (replace-regexp-in-string "^https?://\\(dx.\\)?doi.org/" "" doi))
      (when (string= "" (bibtex-autokey-get-field "url"))
        (bibtex-make-field "url" t)
        (backward-char)
        (insert doi)))))



;;* Debugging a DOI

;; I wrote this function to help debug a DOI. This function generates an
;; org-buffer with the doi, gets the json metadata, shows the bibtex entry, and
;; the pdf link for it.
(defun doi-utils-get-json (doi)
  "Return json data as a string for DOI."
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/citeproc+json")
        (json-data))
    (with-temp-buffer
      (url-insert
       (url-retrieve-synchronously
	(concat doi-utils-dx-doi-org-url doi)))
      (setq json-data (buffer-string))
      (if (string-match "Resource not found" json-data)
          (progn
            (browse-url (concat doi-utils-dx-doi-org-url doi))
            (error "Resource not found.  Opening website"))
	json-data))))


;;;###autoload
(defun doi-utils-debug (doi)
  "Generate an org-buffer showing data about DOI."
  (interactive "sDOI: ")
  (switch-to-buffer "*debug-doi*")
  (erase-buffer)
  (org-mode)
  (insert (concat "doi:" doi) "\n\n")
  (insert "* JSON
"
	  (let ((url-request-method "GET")
		(url-mime-accept-string "application/citeproc+json"))
	    (pp
	     (json-read-from-string (with-temp-buffer
				      (url-insert
				       (url-retrieve-synchronously
					(concat doi-utils-dx-doi-org-url doi)))
				      (buffer-string)))))
	  "\n\n")
  (goto-char (point-min)))

;;* Adding a bibtex entry from a crossref query

;; The idea here is to perform a query on Crossref, get a completion buffer of
;; candidates, and select the entry(ies) you want to add to your bibtex file.
;; You can select a region, e.g. a free form citation, or set of words, or you
;; can type the query in by hand.

;;;###autoload
(defun doi-utils-add-entry-from-crossref-query (query bibtex-file)
  "Search Crossref with QUERY and use completion to select an entry to add to BIBTEX-FILE."
  (interactive (list
                (read-string
                 "Query: "
                 ;; now set initial input
                 (cond
                  ;; If region is active assume we want it
                  ((region-active-p)
                   (replace-regexp-in-string
                    "\n" " "
                    (buffer-substring (region-beginning) (region-end))))
                  ;; type or paste it in
                  (t
                   nil)))
                (completing-read
                 "Bibfile: "
                 (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                         bibtex-completion-bibliography))))
  (let* ((json-data (with-temp-buffer
		      (url-insert
		       (url-retrieve-synchronously
			(concat
			 "https://api.crossref.org/works?query="
			 (url-hexify-string query))))
		      
		      (json-read-from-string (buffer-string))))
	 (name (format "Crossref hits for %s"
		       ;; remove carriage returns. They can make completion confusing.
		       (replace-regexp-in-string "\n" " " query)))
	 (candidates (let-alist json-data
		       (cl-loop for item across .message.items
				collect (let-alist item
					  (cons (format "%s, %s, %s, %s."
							(string-join .title " ")
							(string-join
							 (cl-loop for author across .author collect
								  (let-alist author
								    (format "%s %s"
									    .given .family)))
							 ", ")
							.publisher
							.created.date-parts)			       
						.DOI)))))
	 (doi (cdr (assoc (completing-read "Choice: " candidates) candidates))))

    (with-current-buffer (find-file-noselect bibtex-file)
      (doi-utils-add-bibtex-entry-from-doi
       (replace-regexp-in-string
	"^https?://\\(dx.\\)?doi.org/" "" doi)
       bibtex-file)
      (save-buffer))))


(defalias 'crossref-add-bibtex-entry 'doi-utils-add-entry-from-crossref-query
  "Alias function for convenience.")

;; * Convenience

(defun doi-utils-toggle-pdf-download ()
  "Toggle the setting of `doi-utils-download-pdf'.
I find this useful when downloading the pdfs slows down adding a
lot of references; then you just toggle it off."
  (interactive)
  (message "Setting doi-utils-download-pdf to %s"
	   (setq doi-utils-download-pdf (not doi-utils-download-pdf))))

;;* The end
(provide 'doi-utils)
;;; doi-utils.el ends here
