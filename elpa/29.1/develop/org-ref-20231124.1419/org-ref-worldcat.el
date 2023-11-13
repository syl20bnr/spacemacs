;;; org-ref-worldcat.el --- Worldcat library

;;; Commentary:
;; Provides a function to query world cat
;; See:
;; https://www.worldcat.org/wcpa/content/affiliate/default.jsp
;; https://www.worldcat.org/affiliate/tools?atype=text

;;; Code:

(defun worldcat-query-all (query)
  "Open browser to Worldcat QUERY."
  (browse-url
   (format
    "http://www.worldcat.org/search?qt=worldcat_org_all&q=%s"
    query)))

(provide 'org-ref-worldcat)

;;; org-ref-worldcat.el ends here
