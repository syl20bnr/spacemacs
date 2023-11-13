;;; org-ref-wos.el --- Web of Science functions              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

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

;; Adds a new org-mode link for a search in Web of Science.
;;; and an org-mode link for a link to an Accession number.

(require 'org)
(require 's)
(require 'org-ref-utils)

;;; Code:
(org-link-set-parameters "wos"
			 :follow (lambda (accession-number)
				   (browse-url
				    (concat
				     "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:ut/"
				     accession-number)))
			 :export (lambda (accession-number desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:ut/%s\">%s</a>"
					     accession-number
					     (or desc (concat "wos:" accession-number)))))))

(org-link-set-parameters "wos-search"
			 :follow (lambda (path)
				   (browse-url
				    (format  "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
					     (s-join "+" (split-string path)))))
			 :export (lambda (link desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"%s\">%s</a>"
					     (format  "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
						      (s-join "+" (split-string link)))
					     (or desc link))))))

;;;###autoload
(defun wos-search ()
  "Open the word at point or selection in Web of Science as a topic query."
  ;; the url was derived from this page: http://wokinfo.com/webtools/searchbox/
  (interactive)
  (browse-url
   (format "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
           (if (region-active-p)
               (mapconcat 'identity (split-string
                                     (buffer-substring (region-beginning)
                                                       (region-end))) "+")
             (thing-at-point 'word)))))


;;;###autoload
(defun wos ()
  "Open Web of Science search page in a browser."
  (interactive)
  (browse-url "http://apps.webofknowledge.com"))

;;* Accession numbers
;; see http://kitchingroup.cheme.cmu.edu/blog/2015/06/08/Getting-a-WOS-Accession-number-from-a-DOI/
(defvar *wos-redirect* nil
  "Holds the redirect from a `url-retrieve' callback function.")

(defvar *wos-waiting* nil
  "Non-nil when waiting for a `url-retrieve' redirect.")


(defun wos-get-wos-redirect (url)
  "Return final redirect URL for open-url."
  (setq *wos-waiting* t)
  (url-retrieve
   url
   (lambda (status)
     (setq *wos-redirect* (car (last status)))
     (setq *wos-waiting* nil)))
  (while *wos-waiting* (sleep-for 0.1))
  (url-unhex-string *wos-redirect*))


(defun wos-doi-to-accession-number (doi)
  "Return a WOS Accession number for a DOI."
  (let* ((open-url (concat "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/" doi))
         (redirect (wos-get-wos-redirect open-url)))
    (message redirect)
    (string-match "&KeyUT=WOS:\\([^&]*\\)&" redirect)
    (match-string 1 redirect)))

(provide 'org-ref-wos)
;;; org-ref-wos.el ends here
