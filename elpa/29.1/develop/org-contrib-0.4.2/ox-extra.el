;;; ox-extra.el --- Convenience functions for org export

;; Copyright (C) 2014, 2021  Aaron Ecay

;; Author: Aaron Ecay <aaronecay@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains some convenience functions for org export, which
;; are not part of org's core.  Call `ox-extras-activate' passing a
;; list of symbols naming extras, which will be installed globally in
;; your org session.
;;
;; For example, you could include the following in your .emacs file:
;;
;;    (require 'ox-extra)
;;    (ox-extras-activate '(latex-header-blocks ignore-headlines))
;;

;; Currently available extras:

;; - `latex-header-blocks' -- allow the use of latex blocks, the
;; contents of which which will be interpreted as #+latex_header lines
;; for export.  These blocks should be tagged with #+header: :header
;; yes.  For example:
;; #+header: :header yes
;; #+begin_export latex
;;   ...
;; #+end_export

;; - `ignore-headlines' -- allow a headline (but not its children) to
;; be ignored.  Any headline tagged with the 'ignore' tag will be
;; ignored (i.e. will not be included in the export), but any child
;; headlines will not be ignored (unless explicitly tagged to be
;; ignored), and will instead have their levels promoted by one.

;; TODO:
;; - add a function to org-mode-hook that looks for a ox-extras local
;;   variable and activates the specified extras buffer-locally
;; - allow specification of desired extras to be activated via
;;   customize

;;; Code:

(require 'ox)
(require 'cl-lib)

(defun org-latex-header-blocks-filter (backend)
  (when (org-export-derived-backend-p backend 'latex)
    (let ((positions
	   (org-element-map (org-element-parse-buffer 'greater-element nil) 'export-block
	     (lambda (block)
	       (when (and (string= (org-element-property :type block) "LATEX")
			  (string= (org-export-read-attribute
				    :header block :header)
				   "yes"))
		 (list (org-element-property :begin block)
		       (org-element-property :end block)
		       (org-element-property :post-affiliated block)))))))
      (mapc (lambda (pos)
	      (goto-char (nth 2 pos))
	      (cl-destructuring-bind
		  (beg end &rest ignore)
		  ;; FIXME: `org-edit-src-find-region-and-lang' was
		  ;; removed in 9c06f8cce (2014-11-11).
		  (org-edit-src-find-region-and-lang)
		(let ((contents-lines (split-string
				       (buffer-substring-no-properties beg end)
				       "\n")))
		  (delete-region (nth 0 pos) (nth 1 pos))
		  (dolist (line contents-lines)
		    (insert (concat "#+latex_header: "
				    (replace-regexp-in-string "\\` *" "" line)
				    "\n"))))))
	    ;; go in reverse, to avoid wrecking the numeric positions
	    ;; earlier in the file
	    (reverse positions)))))


;; During export headlines which have the "ignore" tag are removed
;; from the parse tree.  Their contents are retained (leading to a
;; possibly invalid parse tree, which nevertheless appears to function
;; correctly with most export backends) all children headlines are
;; retained and are promoted to the level of the ignored parent
;; headline.
;;
;; This makes it possible to add structure to the original Org-mode
;; document which does not effect the exported version, such as in the
;; following examples.
;;
;; Wrapping an abstract in a headline
;;
;;     * Abstract                        :ignore:
;;     #+LaTeX: \begin{abstract}
;;     #+HTML: <div id="abstract">
;;
;;     ...
;;
;;     #+HTML: </div>
;;     #+LaTeX: \end{abstract}
;;
;; Placing References under a headline (using ox-bibtex in contrib)
;;
;;     * References                     :ignore:
;;     #+BIBLIOGRAPHY: dissertation plain
;;
;; Inserting an appendix for LaTeX using the appendix package.
;;
;;     * Appendix                       :ignore:
;;     #+LaTeX: \begin{appendices}
;;     ** Reproduction
;;     ...
;;     ** Definitions
;;     #+LaTeX: \end{appendices}
;;
(defun org-export-ignore-headlines (data backend info)
  "Remove headlines tagged \"ignore\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
  (org-element-map data 'headline
    (lambda (object)
      (when (member "ignore" (org-element-property :tags object))
        (let ((level-top (org-element-property :level object))
              level-diff)
          (mapc (lambda (el)
                  ;; recursively promote all nested headlines
                  (org-element-map el 'headline
                    (lambda (el)
                      (when (equal 'headline (org-element-type el))
                        (unless level-diff
                          (setq level-diff (- (org-element-property :level el)
                                              level-top)))
                        (org-element-put-property el
                          :level (- (org-element-property :level el)
                                    level-diff)))))
                  ;; insert back into parse tree
                  (org-element-insert-before el object))
                (org-element-contents object)))
        (org-element-extract-element object)))
    info nil)
  (org-extra--merge-sections data backend info)
  data)

(defun org-extra--merge-sections (data _backend info)
  (org-element-map data 'headline
    (lambda (hl)
      (let ((sections
             (cl-loop
              for el in (org-element-map (org-element-contents hl)
                            '(headline section) #'identity info)
              until (eq (org-element-type el) 'headline)
              collect el)))
        (when (and sections
                   (> (length sections) 1))
          (apply #'org-element-adopt-elements
                 (car sections)
                 (cl-mapcan (lambda (s) (org-element-contents s))
                            (cdr sections)))
          (mapc #'org-element-extract-element (cdr sections)))))
    info))

(defconst ox-extras
  '((latex-header-blocks org-latex-header-blocks-filter org-export-before-parsing-hook)
    (ignore-headlines org-export-ignore-headlines org-export-filter-parse-tree-functions))
  "A list of org export extras that can be enabled.

Should be a list of items of the form (NAME FN HOOK).  NAME is a
symbol, which can be passed to `ox-extras-activate'.  FN is a
function which will be added to HOOK.")

(defun ox-extras-activate (extras)
  "Activate certain org export extras.

EXTRAS should be a list of extras (defined in `ox-extras') which
should be activated."
  (dolist (extra extras)
    (let* ((lst (assq extra ox-extras))
	   (fn (nth 1 lst))
	   (hook (nth 2 lst)))
      (when (and fn hook)
	(add-hook hook fn)))))

(defun ox-extras-deactivate (extras)
  "Deactivate certain org export extras.

This function is the opposite of `ox-extras-activate'.  EXTRAS
should be a list of extras (defined in `ox-extras') which should
be activated."
  (dolist (extra extras)
    (let* ((lst (assq extra ox-extras))
	   (fn (nth 1 lst))
	   (hook (nth 2 lst)))
      (when (and fn hook)
	(remove-hook hook fn)))))

(provide 'ox-extra)
;;; ox-extra.el ends here
