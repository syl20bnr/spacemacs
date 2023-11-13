;; citeproc-test-human.el --- support tests in CSL suite format -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions to create ERT tests from tests in CSL test suite format. The
;; official tests can be found at
;; <https://github.com/citation-style-language/test-suite>.

;;; Code:

(require 'f)
(require 's)
(require 'json)
(require 'ert)
(require 'string-inflection)

(require 'citeproc)

(defvar citeproc-test-human--locale-dir "./test/locales")

(defun citeproc-test-human--parse-testfile (file)
  "Return a parsed form of CSL test FILE."
  (let (result
	(json-array-type 'list)
	(json-key-type 'symbol))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char 1)
      (setq result (list (cons 'YEAR-SUFF
			       (re-search-forward "variable=\"year-suffix\"" nil t))))
      (goto-char 1)
      (while (re-search-forward ">>=+ \\{1,2\\}\\([[:graph:]]+\\) =+>>" nil t)
	(let ((section (intern (buffer-substring (nth 2 (match-data))
						 (nth 3 (match-data)))))
	      (start (1+ (point)))
	      end)
	  (re-search-forward "<<=" nil t)
	  (setq end (- (car (match-data)) 1))
	  (push (cons section (pcase section
				('OUTPUT-FORMAT
				 (intern (buffer-substring-no-properties start end)))
				('CSL (buffer-substring-no-properties start end))
				((or 'INPUT 'CITATIONS 'CITATION-ITEMS)
				 (goto-char (- start 1)) (json-read))
				(_ (buffer-substring start end))))
		result))))
    result))

(defun citeproc-test-human--create-getter (items)
  "Return a getter function for ITEMS.
ITEMS is the parsed representation of the `INPUT' section of a
CSL test."
  (lambda (itemids)
    (let (result)
     (dolist (item items result)
       (let ((id (citeproc-s-from-num-or-s (alist-get 'id item))))
	 (when (member id itemids)
	   (push (cons id item) result)))))))

(defun citeproc-test-human--proc-from-style (style parsed-input)
  "Create a processor from STYLE and PARSED-INPUT."
  (citeproc-create style
		   (citeproc-test-human--create-getter parsed-input)
		   (citeproc-locale-getter-from-dir citeproc-test-human--locale-dir)))

(defun citeproc-test-human--proc-from-testfile (file)
  "Create an (itemless) processor from a test FILE."
  (let ((style-string (alist-get 'CSL (citeproc-test-human--parse-testfile file)))
	(locale-getter (citeproc-locale-getter-from-dir citeproc-test-human--locale-dir)))
    (citeproc-create style-string nil locale-getter)))

(defun citeproc-test-human--parse-citation (ct-desc &optional cites-only)
  "Parse test citations description CT-DESC.
Return a list of citation structures. If CITES-ONLY is non-nil
then the input is list of cites."
  (if cites-only
      (citeproc-citation-create
       :cites (-map #'citeproc-test-human--normalize-cite ct-desc))
    (let ((citation-info (car ct-desc)))
      (let-alist (alist-get 'properties citation-info)
	(citeproc-citation-create
	 :cites (-map #'citeproc-test-human--normalize-cite
		      (alist-get 'citationItems citation-info))
	 :note-index .noteIndex
	 :mode (citeproc-lib-intern .mode)
	 :capitalize-first .capitalize-first
	 :suppress-affixes .suppress-affixes
	 :ignore-et-al .ignore-et-al)))))

(defun citeproc-test-human--normalize-cite (cite)
  "Normalize a test CITE."
  (--map (let ((val (cdr it)))
	   (if (numberp val) (cons (car it) (number-to-string val)) it))
	 cite))

(defun citeproc-test-human--run-parsed (parsed)
  "Run the parsed CSL test PARSED.
Return the resulting output."
  (-let* (((&alist 'CSL style
		   'INPUT input
		   'MODE mode
		   'CITATION-ITEMS citation-items
		   'CITATIONS citations
		   'OUTPUT-FORMAT output-format)
	   parsed)
	  (output-format (or output-format 'csl-test))
	  (proc (citeproc-test-human--proc-from-style style input)))
    (--each input
      (citeproc-proc-put-item-by-id proc
				    (citeproc-s-from-num-or-s (alist-get 'id it))))
    (when (string= mode "citation")
      (cond
       (citation-items
	(citeproc-append-citations (--map (citeproc-test-human--parse-citation it t)
					  citation-items)
				   proc))
       (citations
	(citeproc-append-citations (mapcar #'citeproc-test-human--parse-citation citations)
				   proc))
       (t (citeproc-append-citations (list (citeproc-test-human--parse-citation input t))
				     proc))))
    (let ((output (if (string= mode "citation")
		      (citeproc-render-citations proc output-format
						 (when (eq 'csl-test output-format)
						   'no-links))
		    (car (citeproc-render-bib proc output-format 'no-links)))))
      (if (string= mode "citation") (s-join "\n" output) output))))

(defun citeproc-test-human--expected-from-parsed (parsed)
  "Return the expected output of parsed CSL test PARSED."
  (let ((expected (alist-get 'RESULT parsed)))
    (if (or (string= (s-left 5 expected) "..[0]")
	    (string= (s-left 5 expected) ">>[0]"))
	(s-join "\n" (--map (substring it 6)
			    (split-string expected "\n")))
      expected)))

(defun citeproc-test-human-create-from-file (file expected-fails &optional name-prefix)
  "Create an ERT test from a CSL test FILE.
If optional NAME-PREFIX is non-nil then it is added the name of
the created test after the obligatory `citeproc'."
  (let* ((parsed (citeproc-test-human--parse-testfile file))
	 (expected (citeproc-test-human--expected-from-parsed parsed))
	 (file-name (f-filename file))
	 (test-name (intern
		     (concat "citeproc-"
			     (if name-prefix  (concat name-prefix "-") "")
			     (string-inflection-kebab-case-function
			      (substring file-name 0 -4)))))
	 (expected-fail (memq test-name expected-fails)))
    (eval `(ert-deftest ,test-name ()
	     :expected-result ,(if expected-fail :failed :passed)
	     (let ((citeproc-disambiguation-cite-pos 'subsequent))
	       (should (string=
			,expected
			(citeproc-test-human--run-parsed ',parsed))))))))

(defun citeproc-test-human---read-expected-fails (expected-fails-file)
  "Read the list of tests expected to fail from EXPECTED-FAILS-FILE."
  (let* ((list-as-str (with-temp-buffer
			(insert-file-contents expected-fails-file)
			(buffer-string)))
	 (split (split-string list-as-str "\n")))
    (--map (intern it) (butlast split))))

(defun citeproc-test-human-create-from-dir (dir &optional
						expected-fails-file name-prefix)
  "Create all CSL tests from DIR.
Each file in DIR having the `txt' extension is read as a
human-readable CSL test, and a corresponding ERT test is created.
The created test's name will be constructed by prefixing the
test's filename (without the extension) with `citeproc-'. If the
optional EXPECTED-FAILS-FILE is non-nil then read that file as a
list of tests whose failure is expected. If optional NAME-PREFIX
is non-nil then it is added the names of the created tests after
the obligatory `citeproc'. The file should contain one test-name
per line (together with the `citeproc-' prefix)."
  (let ((expected-fails
	 (if expected-fails-file
	     (citeproc-test-human---read-expected-fails expected-fails-file)
	   nil)))
    (dolist (test-file (f-glob (concat dir "/*.txt")))
      (citeproc-test-human-create-from-file test-file expected-fails
					    name-prefix))))

(provide 'citeproc-test-human)

;;; citeproc-test-human.el ends here
