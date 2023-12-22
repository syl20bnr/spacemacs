;;; org-ref-sci-id.el --- org-mode links for scientific IDs  -*- lexical-binding: t; -*-

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

;; New org-mode links for scientific IDs like orcid (http://orcid.org) and researcherid (http://www.researcherid.com)
;; orcid:0000-0003-2625-9232
;; researcherid:A-2363-2010

;;; Code:

(require 'org)
(require 'org-ref-utils)

(org-link-set-parameters "orcid"
			 :follow (lambda
				   (link-string)
				   (browse-url
				    (format "http://orcid.org/%s" link-string)))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"http://orcid.org/%s\">orcid:%s</a>" keyword (or desc keyword))))))

(org-link-set-parameters "researcherid"
			 :follow (lambda
				   (link-string)
				   (browse-url
				    (format "http://www.researcherid.com/rid/%s" link-string)))
			 :export (lambda (keyword desc format)
				   (cond
				    ((eq format 'html)
				     (format "<a href=\"http://www.researcherid.com/rid/%s\">ResearcherID:%s</a>"
					     keyword (or desc keyword))))))

(provide 'org-ref-sci-id)
;;; org-ref-sci-id.el ends here
