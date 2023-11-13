;;; org-ref-utils.el --- Utility functions for org-ref  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  John Kitchin

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

;;

(eval-when-compile (require 'cl-lib))

(require 'org)
(eval-and-compile (require 'org-macs))


(defvar org-ref-cite-types)
(defvar pdftotext-executable)

(declare-function 'org-ref-get-bibtex-key-and-file "org-ref-core.el")
(declare-function 'org-ref-find-bibliography "org-ref-core.el")
(declare-function 'org-ref-bib-citation "org-ref-core.el")
(declare-function 'org-ref-get-bibtex-key-under-cursor "org-ref-core.el")

;;; Code:
;;;###autoload
(defun org-ref-version ()
  "Provide a version string for org-ref.
Copies the string to the clipboard."
  (interactive)
  ;; version in the el file.
  (let* ((org-ref-el (concat
		      (file-name-sans-extension
		       (locate-library "org-ref"))
		      ".el"))
	 (org-ref-dir (file-name-directory org-ref-el))
	 org-version
	 git-commit
	 version-string)

    (setq org-version (with-temp-buffer
			(insert-file-contents org-ref-el)
			(goto-char (point-min))
			(re-search-forward ";; Version:")
			(s-trim (buffer-substring (point)
						  (line-end-position)))))

    (setq git-commit
	  ;; If in git, get current commit
	  (let ((default-directory org-ref-dir))
	    (when (and
		   ;; this is tricky, as a submodule, .git is a file
		   (or (file-directory-p ".git") (file-exists-p ".git"))
		   (= 0 (shell-command "git rev-parse --git-dir")))
	      (format "%s in %s"
		      (s-trim (shell-command-to-string "git rev-parse HEAD"))
		      (s-trim (shell-command-to-string "git rev-parse --show-toplevel"))))))

    (setq version-string (format "org-ref: Version %s%s"
				 org-version
				 (if git-commit
				     (format " (git-commit %s)" git-commit)
				   "")))
    (kill-new version-string)
    (message version-string)))


(defun org-ref-report-issue ()
  "Report an issue in org-ref.
Opens https://github.com/jkitchin/org-ref/issues/new."
  (save-window-excursion
    (org-ref-debug)
    (kill-new (buffer-string)))
  (message "org-ref-debug has been run. You can paste the results in the issue website if you like.")
  (browse-url "https://github.com/jkitchin/org-ref/issues/new"))


;;* Debug
;; this is just for me to have a nicer debug statement.
(defmacro ords (&rest body)
  "Evaluate BODY and return a string."
  `(format "%s" (progn ,@body)))


;;;###autoload
(defun org-ref-debug ()
  "Print some debug information to a buffer."
  (interactive)
  (switch-to-buffer "*org-ref-debug*")
  (erase-buffer)
  (org-mode)
  (insert
   (s-format "#+TITLE: org-ref debug

${org-ref-version}

* System

- system-type :: ${system}
- system-configuration :: ${system-configuration}
- window system :: ${window-system}
- Emacs :: ${emacs-version}
- org-version :: ${org-version}

* about org-ref

org-ref installed in [[${org-ref-location}]].

* org-ref setup

-  org-ref-insert-link-function :: ${org-ref-insert-link-function}
-  org-ref-insert-cite-function :: ${org-ref-insert-cite-function}
-  org-ref-insert-label-function :: ${org-ref-insert-label-function}
-  org-ref-insert-ref-function :: ${org-ref-insert-ref-function}
-  org-ref-cite-onclick-function :: ${org-ref-cite-onclick-function}

* org-ref libraries

** org-ref-helm (loaded: ${org-ref-helm-p})
** org-ref-ivy  (loaded: ${org-ref-ivy-p})
** org-ref-pdf (loaded: ${org-ref-pdf-p})

- system pdftotext :: ${pdftotext}

You set =pdftotext-executable= to ${pdftotext-executable} (exists: ${pdftotext-executable-p})

** org-ref-url-utils (loaded: ${org-ref-url-p})

* export variables

- org-latex-pdf-process :: ${org-latex-pdf-process}
"
	     'aget
	     `(("org-ref-version" . ,(org-ref-version))
	       ("org-latex-pdf-process" . ,(format "%S" org-latex-pdf-process))
	       ("org-ref-location" . ,(format "%s" (locate-library "org-ref")))

	       ("system" . ,(format "System: %s" system-type))
	       ("system-configuration" . ,(ords system-configuration))
	       ("window-system" . ,(format "Window system: %s" window-system))
	       ("emacs-version" . ,(ords (emacs-version)))
	       ("org-version" . ,(org-version))

	       ("org-ref-pdf-p" . ,(ords (featurep 'org-ref-pdf)))
	       ("org-ref-helm-p" . ,(ords (featurep 'org-ref-helm)))
	       ("org-ref-ivy-p" . ,(ords (featurep 'org-ref-ivy)))

	       ("pdftotext" . ,(ords (if (featurep 'org-ref-pdf)
					 (executable-find "pdftotext")
				       "org-ref-pdf not loaded")))
	       ("pdftotext-executable" . ,(ords (if (featurep 'org-ref-pdf)
						    pdftotext-executable
						  "org-ref-pdf not loaded")))
	       ("pdftotext-executable-p" . ,(ords (if (boundp 'pdftotext-executable)
						      (or
						       (executable-find pdftotext-executable)
						       (file-exists-p pdftotext-executable))
						    "pdftotext-executable is not bound")))
	       ("org-ref-url-p" . ,(ords (featurep 'org-ref-url)))
	       ("org-ref-insert-link-function" . ,org-ref-insert-link-function)
	       ("org-ref-insert-cite-function" . ,org-ref-insert-cite-function)
	       ("org-ref-insert-label-function" . ,org-ref-insert-label-function)
	       ("org-ref-insert-ref-function" . ,org-ref-insert-ref-function)
	       ("org-ref-cite-onclick-function" . ,org-ref-cite-onclick-function)))))


(defun org-ref-get-bibtex-entry-citation (key)
  "Return a string for the bibliography entry corresponding to KEY."
  (bibtex-completion-apa-format-reference key))


(defun org-ref-get-bibtex-entry (key)
  "Return the bibtex entry as a string."
  (save-window-excursion
    (bibtex-completion-show-entry (list key))
    (bibtex-copy-entry-as-kill)
    (pop bibtex-entry-kill-ring)))

(defun org-ref-library-path ()
  "return the library path"
  (cond
   ((stringp bibtex-completion-library-path)
    bibtex-completion-library-path)
   ((and (listp bibtex-completion-library-path)
	 (= 1 (length bibtex-completion-library-path)))
    (car bibtex-completion-library-path))
   (t
    (completing-read "Dir: " bibtex-completion-library-path))))

;;*** key at point functions
(defun org-ref-get-pdf-filename (key)
  "Return the pdf filename associated with a bibtex KEY.
This searches for the pattern KEY*.pdf. If one result is found it
is returned, but if multiple results are found, e.g. there are
related files to the KEY you are prompted for which one you want."
  (let ((results (bibtex-completion-find-pdf-in-library key)))
    (cond
     ((null results)
      nil)
     ((= 1 (length results))
      (car results))
     (t
      (completing-read "PDF: " results)))))

;; TODO: do we even need this?
(defun org-ref-get-mendeley-filename (key)
  "Return the pdf filename indicated by mendeley file field.
Falls back to `org-ref-get-pdf-filename' if file field does not exist.
Contributed by https://github.com/autosquid.
Argument KEY is the bibtex key."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (let ((e (bibtex-autokey-get-field "file")))
	(if (> (length e) 4)
            (let ((clean-field (replace-regexp-in-string "{\\|}\\|\\\\" "" e)))
              (let ((first-file (car (split-string clean-field ";" t))))
		(format "/%s" (substring first-file 1
					 (- (length first-file) 4)))))
          (expand-file-name (format "%s.pdf" key) (cond
						   ((stringp bibtex-completion-library-path)
						    bibtex-completion-library-path)
						   ((= 1 (length bibtex-completion-library-path))
						    (car bibtex-completion-library-path))
						   (t
						    (completing-read "PDF dir: " bibtex-completion-library-path)))))))))


(defun org-ref-get-pdf-filename-bibtex-completion (key)
  "Use bibtex-completion to retrieve a PDF filename for KEY.
bibtex-completion looks in both the configured directory
`bibtex-completion-library-path' and in the fields of the bibtex
item for a filename. It understands file fields exported by
Jabref, Mendeley and Zotero. See `bibtex-completion-find-pdf'."
  (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
    (or (car (bibtex-completion-find-pdf key)) "")))


;;;###autoload
(defun org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (bibtex-completion-find-pdf key t)))
    (pcase (length pdf-file)
      (0
       (message "no pdf found for %s" key))
      (1
       (funcall bibtex-completion-pdf-open-function (car pdf-file)))
      (_
       (funcall bibtex-completion-pdf-open-function
		(completing-read "pdf: " pdf-file))))))


;;;###autoload
(defun org-ref-add-pdf-at-point (&optional prefix)
  "Add the pdf for bibtex key under point if it exists.

Similar to org-ref-bibtex-assoc-pdf-with-entry prompt for pdf
associated with bibtex key at point and rename it.  Check whether a
pdf already exists in `bibtex-completion-library' with the name
'[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it
in a directory. Optional PREFIX argument toggles between
`rename-file' and `copy-file'."

  (interactive)
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (bibtex-completion-find-pdf-in-library key)))
    (if pdf-file
        (message "PDF for key [%s] already exists %s" key pdf-file)
      (let* (
             (source-file-name (read-file-name (format "Select pdf file associated with key [%s]: " key)
		                   org-ref-bibtex-pdf-download-dir))
             (dest-file-name (expand-file-name (format "%s.pdf" key) (org-ref-library-path)))
             (file-move-func (org-ref-bibtex-get-file-move-func prefix))
             )
        (progn
          (funcall file-move-func source-file-name dest-file-name)
          (message "added file %s to key %s" dest-file-name key))))))

;;;###autoload
(defun org-ref-open-url-at-point ()
  "Open the url for bibtex key under point."
  (interactive)
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results)))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key)
        ;; I like this better than bibtex-url which does not always find
        ;; the urls
        (catch 'done
          (let ((url (s-trim (bibtex-autokey-get-field "url"))))
            (unless (s-blank? url)
              (browse-url url)
              (throw 'done nil)))

          (let ((doi (s-trim (bibtex-autokey-get-field "doi"))))
            (unless (s-blank? doi)
              (if (string-match "^http" doi)
                  (browse-url doi)
                (browse-url (format "http://dx.doi.org/%s" doi)))
              (throw 'done nil))))))))


;;;###autoload
(defun org-ref-open-notes-at-point (&optional thekey)
  "Open the notes for bibtex key under point in a cite link in a buffer.
Can also be called with THEKEY in a program."
  (interactive)
  (org-mark-ring-push)
  (when (null thekey)
    (setq thekey (org-ref-get-bibtex-key-under-cursor)))
  (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
    (bibtex-completion-edit-notes (list thekey))))


;;;###autoload
(defun org-ref-open-citation-at-point ()
  "Open bibtex file to key at point."
  (interactive)
  (org-mark-ring-push)
  (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
    (bibtex-completion-show-entry (list (org-ref-get-bibtex-key-under-cursor)))))


;;*** cite menu

;;;###autoload
(defun org-ref-copy-entry-as-summary ()
  "Copy the bibtex entry for the citation at point as a summary."
  (interactive)
  (kill-new (org-ref-bib-citation)))


(defun org-ref-get-doi-at-point ()
  "Get doi for key at point."
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (bibfile (cdr results))
         doi)
    (save-excursion
      (with-temp-buffer
        (insert-file-contents bibfile)
        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
        (bibtex-search-entry key)
        (setq doi (bibtex-autokey-get-field "doi"))
        ;; in case doi is a url, remove the url part.
        (replace-regexp-in-string "^http://dx.doi.org/" "" doi)))))


;;**** functions that operate on key at point for click menu

;;;###autoload
(defun org-ref-ads-at-point ()
  "Open the doi in ADS for bibtex key under point."
  (interactive)
  (doi-utils-ads (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-wos-at-point ()
  "Open the doi in wos for bibtex key under point."
  (interactive)
  (doi-utils-wos (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-wos-citing-at-point ()
  "Open the doi in wos citing articles for bibtex key under point."
  (interactive)
  (doi-utils-wos-citing (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-wos-related-at-point ()
  "Open the doi in wos related articles for bibtex key under point."
  (interactive)
  (doi-utils-wos-related (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-google-scholar-at-point ()
  "Search google scholar for bibtex key under point using the title."
  (interactive)
  (browse-url
   (url-encode-url
    (format
     "http://scholar.google.com/scholar?q=%s"
     (bibtex-completion-get-value
      "title"
      (bibtex-completion-get-entry (org-ref-get-bibtex-key-under-cursor)))))))


;;;###autoload
(defun org-ref-biblio-at-point ()
  "Do a biblio search for bibtex key under point using the title."
  (interactive)
  (biblio-lookup
   nil
   (bibtex-completion-get-value
    "title"
    (bibtex-completion-get-entry (org-ref-get-bibtex-key-under-cursor)))))


;;;###autoload
(defun org-ref-pubmed-at-point ()
  "Open the doi in pubmed for bibtex key under point."
  (interactive)
  (doi-utils-pubmed (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-crossref-at-point ()
  "Open the doi in crossref for bibtex key under point."
  (interactive)
  (doi-utils-crossref (org-ref-get-doi-at-point)))


;;;###autoload
(defun org-ref-email-at-point ()
  "Email the citation(s) at point."
  (interactive)
  (let* ((cite (org-element-context))
	 (keys ()))
    (compose-mail)
    (message-goto-body)
    (cl-loop for ref in (plist-get
			 (org-ref-parse-cite-path (org-element-property :path cite))
			 :references)
	     do
	     (let* ((key (plist-get ref :key))
		    (entry (save-window-excursion
			     (bibtex-completion-show-entry (list key))
			     (bibtex-copy-entry-as-kill)
			     (pop bibtex-entry-kill-ring)))
		    (pdfs (bibtex-completion-find-pdf key)))
	       (setq keys (append keys (list key)))
	       (insert entry)
	       (cl-loop for pdf in pdfs do (mml-attach-file pdf))))

    (message-goto-subject)
    (insert "References: " (string-join keys ","))))


;;* General org-ref utilities


(defun org-ref-get-bibtex-keys (&optional sort)
  "Return a list of unique keys in the buffer.
Use SORT to specify alphabetical order by key."
  (let ((keys '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (assoc (plist-get plist ':type) org-ref-cite-types)
	    (setq keys (append keys (cl-loop for ref in
					     (plist-get (org-ref-parse-cite-path
							 (org-element-property :path link))
							:references)
					     collect (plist-get ref :key)))))))
      ;; set with-affiliated to get keys in captions
      nil nil nil t)
    (when sort
      ;; Sort keys alphabetically
      (setq keys (cl-sort keys 'string-lessp :key 'downcase)))
    (delete-dups keys)))


(defun org-ref-pdf-p (filename)
  "Check if FILENAME is PDF file.

From the PDF specification 1.7:

    The first line of a PDF file shall be a header consisting of
    the 5 characters %PDF- followed by a version number of the
    form 1.N, where N is a digit between 0 and 7."
  (let* ((header (with-temp-buffer
		   (set-buffer-multibyte nil)
		   (insert-file-contents-literally filename nil 0 5)
		   (buffer-string)))
	 (valid (string-equal (encode-coding-string header 'utf-8) "%PDF-")))
    (if valid
	valid
      (message "Invalid pdf. Header = %s" header)
      nil)))




;; This section creates some code that should speed up org-ref for large files.
;; I use org-element-parse-buffer a lot for getting information about labels
;; etc. However, it gets called a lot, and this is slow in large documents. Here
;; we try to use a cache that helps speed this up at least on loading. Some
;; notes for the future: on loading, it seems like fontification triggers buffer
;; changes, so here we only consider char changes. I am not sure this is the
;; best strategy overall. It is faster to use regexps for finding this
;; information, but those are substantially more difficult to debug in my
;; experience. There is an unfortunate number of ways to reference things in
;; org-mode, and so far this has been most reliable. An alternative might be to
;; leveralge what happens in font-lock somehow to update local variables
;; containing org-ref labels, refs, cites, etc. That would miss some #+names
;; though, and maybe some other things like custom-ids.

(defvar-local org-ref-char-change-tick nil
  "Local variable to track character changes.")


(defvar-local org-ref-parse-buffer-cache nil
  "Local variable to store parse buffer data.")


(defun org-ref-parse-buffer (&optional force)
  "This is a thin wrapper around `org-element-parse-buffer'.
The idea is to cache the data, and return it unless we can tell
the buffer has been modified since the last time we ran it.
if FORCE is non-nil reparse the buffer no matter what."
  (if force
      (progn
      	(message "Forcing update.")
      	(setq-local org-ref-char-change-tick (buffer-chars-modified-tick))
      	(setq-local org-ref-parse-buffer-cache (org-element-parse-buffer)))

    (cond
     ((null org-ref-parse-buffer-cache)
      ;; (message "First parse.")
      (setq-local org-ref-char-change-tick (buffer-chars-modified-tick))
      (setq-local org-ref-parse-buffer-cache (org-element-parse-buffer)))

     ((not (eq org-ref-char-change-tick (buffer-chars-modified-tick)))
      ;; (message "Updating from a char change in the buffer.")
      (setq-local org-ref-char-change-tick (buffer-chars-modified-tick))
      (setq-local org-ref-parse-buffer-cache (org-element-parse-buffer)))

     (t
      ;; (message "Using cache.")
      org-ref-parse-buffer-cache))))



;;** bad citations, labels, refs and files in orgfile
;;  These are used i
(defvar bibtex-files)
(defvar bibtex-file-path)
(defun org-ref-bad-cite-candidates ()
  "Return a list of conses (key . marker) where key does not exist in the known bibliography files, and marker points to the key."
  (let* ((cp (point))			; save to return to later
         (bibtex-files (cl-loop for f in (org-ref-find-bibliography)
				if (file-exists-p f)
				collect (file-truename f)))
         (bibtex-file-path (mapconcat
                            (lambda (x)
                              (file-name-directory (file-truename x)))
                            bibtex-files ":"))
         (bibtex-keys (mapcar (lambda (x) (car x))
                              (bibtex-global-key-alist)))
         (bad-citations '()))

    (org-element-map (org-ref-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
          (when (assoc (plist-get plist :type) org-ref-cite-types)
	    (when (not (string= "*" (plist-get plist :path)))
	      (cl-loop for ref in (plist-get (org-ref-parse-cite-path (plist-get plist :path)) :references)
		       do
		       (when (not (member (plist-get ref :key) bibtex-keys))
			 (goto-char (plist-get plist :begin))
			 (re-search-forward (plist-get ref :key))
			 (push (cons (plist-get ref :key) (point-marker)) bad-citations)))))))
      ;; add with-affiliates to get cites in caption
      nil nil nil t)
    (goto-char cp)
    bad-citations))


(defun org-ref-bad-ref-candidates ()
  "Return a list of conses (ref . marker) where ref is a ref link that does not point to anything (i.e. a label)."
  ;; first get a list of legitimate labels
  (let ((cp (point))
        (labels (mapcar 'car (org-ref-get-labels)))
        (bad-refs '()))
    ;; now loop over ref links
    (goto-char (point-min))
    (org-element-map (org-ref-parse-buffer) 'link
      (lambda (link)
        (let ((plist (nth 1 link)))
	  (when (assoc (plist-get plist ':type)  org-ref-ref-types)
	    (cl-loop for label in (split-string (plist-get plist :path) ",")
		     do
		     (unless (-contains? labels label)
		       (goto-char (plist-get plist :begin))
		       (add-to-list
			'bad-refs
			(cons label (point-marker)))))))))
    (goto-char cp)
    bad-refs))


(defun org-ref-count-labels (label)
  "Return the number of times LABEL appears in the buffer."
  (let ((rx (string-join org-ref-ref-label-regexps "\\|"))
	(labels '()))
    (save-excursion
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward rx nil t)
	 (cl-pushnew (match-string-no-properties 1) labels))))
    (-count (lambda (x) (and (stringp x) (string= x label))) labels)))


(defun org-ref-bad-label-candidates ()
  "Return a list of labels where label is multiply defined."
  (let ((labels (mapcar 'car (org-ref-get-labels)))
        (multiple-labels '()))
    ;; labels should be a unique list.
    (dolist (label labels)
      (when (> (org-ref-count-labels label) 1)
	(let ((cp (point)))
          (goto-char (point-min))
	  ;; regular org label:tag links
          (while (re-search-forward
                  (format  "[^#+]label:%s\\s-" label) nil t)
            (cl-pushnew (cons label (point-marker)) multiple-labels
			:test (lambda (a b)
				(and (string= (car a) (car b))
				     (= (marker-position (cdr a))
					(marker-position (cdr b)))))))

          (goto-char (point-min))
	  ;; latex style
          (while (re-search-forward
                  (format  "\\label{%s}\\s-?" label) nil t)
            (cl-pushnew (cons label (point-marker)) multiple-labels
			:test (lambda (a b)
				(and (string= (car a) (car b))
				     (= (marker-position (cdr a))
					(marker-position (cdr b)))))))

	  ;; keyword style
          (goto-char (point-min))
          (while (re-search-forward
                  (format  "^\\( \\)*#\\+label:\\s-*%s" label) nil t)
            (cl-pushnew (cons label (point-marker)) multiple-labels
			:test (lambda (a b)
				(and (string= (car a) (car b))
				     (= (marker-position (cdr a))
					(marker-position (cdr b)))))))

          (goto-char (point-min))
          (while (re-search-forward
                  (format "^\\( \\)*#\\+tblname:\\s-*%s" label) nil t)
            (cl-pushnew (cons label (point-marker)) multiple-labels
			:test (lambda (a b)
				(and (string= (car a) (car b))
				     (= (marker-position (cdr a))
					(marker-position (cdr b)))))))
          (goto-char cp))))
    multiple-labels))


(defun org-ref-bad-file-link-candidates ()
  "Return list of conses (link . marker) where the file in the link does not exist."
  (let* ((bad-files '()))
    (org-element-map (org-ref-parse-buffer) 'link
      (lambda (link)
        (let ((type (org-element-property :type link)))
          (when (or  (string= "file" type)
                     (string= "attachfile" type))
            (unless (file-exists-p (org-element-property :path link))
              (add-to-list 'bad-files
                           (cons (org-element-property :path link)
                                 (save-excursion
                                   (goto-char
                                    (org-element-property :begin link))
                                   (point-marker)))))))))
    ;; Let us also check \attachfile{fname}
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\\attachfile{\\([^}]*\\)}" nil t)
        (unless (file-exists-p (match-string 1))
          (push  (cons (match-string 1) (point-marker)) bad-files))))
    bad-files))


;; * org-ref command
(defun org-ref ()
  "Check the current org-buffer for potential issues."
  (interactive)
  (let* ((buf (get-buffer-create "*org-ref*"))
	 (cb (current-buffer))
	 (fname (buffer-file-name))
	 ;; Check if elc is ok before anything else because if it is not, it
	 ;; causes problems in org-ref.
	 (elc-ok (let* ((org-ref-el (concat
				     (file-name-sans-extension
				      (locate-library "org-ref"))
				     ".el"))
			(orel-mod)
			(org-ref-elc (concat
				      (file-name-sans-extension
				       (locate-library "org-ref"))
				      ".elc"))
			(orelc-mod)
			(elc-version))
		   (when (file-exists-p org-ref-el)
		     (setq orel-mod (file-attribute-modification-time (file-attributes org-ref-el))))
		   (when (file-exists-p org-ref-elc)
		     (setq orelc-mod (file-attribute-modification-time (file-attributes org-ref-elc))))

		   (with-current-buffer buf
		     (read-only-mode -1)
		     (erase-buffer)
		     (org-mode)
		     (insert (format "#+title: org-ref report on [[%s][%s]]\n\n" (buffer-file-name cb) (buffer-name cb)))
		     (insert (format "org-ref called from %s\n\n" (buffer-file-name cb)))

		     (unless (time-less-p orel-mod orelc-mod)
		       (insert (format "org-ref.elc (%s) is older than org-ref.el (%s). That is probably not right. Please delete %s.\n"
				       (format-time-string "%Y-%m-%d %H:%M:%S" orelc-mod)
				       (format-time-string "%Y-%m-%d %H:%M:%S" orel-mod)
				       org-ref-elc))
		       (insert (format "- load-prefer-newer = %s\n" load-prefer-newer))
		       (insert (format  "  consider
- deleting %s
- [[elisp:(delete-file \"%s\")]]
- add (setq load-prefer-newer t) to your init files
- using https://github.com/emacscollective/auto-compile.\n" org-ref-elc org-ref-elc))

		       ;; Check for byte-compiling compatibility with current emacs
		       (when (and org-ref-elc
				  (file-exists-p org-ref-elc))
			 (setq elc-version (with-temp-buffer
					     (insert-file-contents org-ref-elc)
					     (goto-char (point-min))
					     (when (re-search-forward ";;; in Emacs version \\([0-9]\\{2\\}\\.[0-9]+\\)"
								      nil t)
					       (match-string 1))))
			 (unless (string= elc-version
					  (format "%s.%s" emacs-major-version emacs-minor-version))
			   (insert (format "%s compiled with Emacs %s but you are running %s. That could be a problem.\n"
					   elc-version emacs-major-version emacs-minor-version))))))))
	 (bad-citations (org-ref-bad-cite-candidates))
	 (bad-refs (org-ref-bad-ref-candidates))
	 (bad-labels (org-ref-bad-label-candidates))
	 (bad-files (org-ref-bad-file-link-candidates))
	 (bib-candidates '())
	 (unreferenced-labels '())
	 natbib-required
	 natbib-used
	 cleveref-required
	 cleveref-used
	 biblatex-required
	 biblatex-used
	 mbuffer
	 mchar
	 (org-latex-prefer-user-labels (and (boundp 'org-latex-prefer-user-labels)
					    org-latex-prefer-user-labels)))

    (when elc-ok nil)  		; this is to silence a compiler error
					; about elc-ok not being used. I use it
					; as a side-effect above

    ;; See if natbib, biblatex or cleveref are required
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(when (member (org-element-property :type link) org-ref-natbib-types)
	  (setq natbib-required t))
	(when (member (org-element-property :type link) org-ref-biblatex-types)
	  (setq biblatex-required t))
	(when (member (org-element-property :type link) '("cref" "Cref"))
	  (setq cleveref-required t)))
      nil t)

    ;; See if natbib is probably used. This will miss a case where natbib is included somehow.
    (setq natbib-used
	  (or
	   (member "natbib" (mapcar (lambda (x) (when (listp x) (nth 1 x))) org-latex-default-packages-alist))
	   (member "natbib" (mapcar (lambda (x) (when (listp x) (nth 1 x))) org-latex-packages-alist))
	   ;; see of something like \usepackage{natbib} exists.
	   (save-excursion
	     (goto-char (point-min))
	     (re-search-forward "{natbib}" nil t))))

    (setq biblatex-used
	  (or
	   (member "biblatex" (mapcar (lambda (x) (when (listp x) (nth 1 x))) org-latex-default-packages-alist))
	   (member "biblatex" (mapcar (lambda (x) (when (listp x) (nth 1 x))) org-latex-packages-alist))
	   ;; see of something like \usepackage{biblatex} exists.
	   (save-excursion
	     (goto-char (point-min))
	     (re-search-forward "{biblatex}" nil t))))

    (setq cleveref-used
	  (or
	   (member "cleveref" (mapcar (lambda (x) (when (listp x) (nth 1 x))) org-latex-default-packages-alist))
	   (member "cleveref" (mapcar (lambda (x) (when (listp x) (nth 1 x))) org-latex-packages-alist))
	   ;; see of something like \usepackage{cleveref} exists.
	   (save-excursion
	     (goto-char (point-min))
	     (re-search-forward  "{cleveref}" nil t))))

    ;; setup bib-candidates. This checks a variety of things in the
    ;; bibliography, bibtex files. check for which bibliographies are used

    (cl-loop for bibfile in (org-ref-find-bibliography)
	     do
	     (let ((bibdialect))
	       (with-current-buffer (find-file-noselect bibfile)
		 (setq bibdialect bibtex-dialect))
	       (cl-pushnew
		(format "[[%s]] (dialect = %s)\n" bibfile bibdialect)
		bib-candidates)))


    ;; Check bibliography style exists
    (save-excursion
      (goto-char 0)
      (unless (re-search-forward "bibliographystyle:\\|\\\\bibliographystyle{" nil t)
	(cl-pushnew
	 "No bibliography style found. This may be ok, if your latex class style sets that up, but if not this is an error. Try adding something like:
    bibliographystyle:unsrt
    at the end of your file.\n"
	 bib-candidates)))

    ;; Check if latex knows of the bibliographystyle. We only check links here.
    ;;  I also assume this style exists as a bst file that kpsewhich can find.
    (save-excursion
      (goto-char 0)
      (when (re-search-forward "bibliographystyle:" nil t)
	;; on a link. get style
	(let ((path (org-element-property :path (org-element-context))))
          (unless (= 0 (shell-command (format "kpsewhich %s.bst" path)))
            (cl-pushnew
	     (format "bibliographystyle \"%s\" may be unknown" path)
	     bib-candidates)))))

    ;; check for multiple bibliography links
    (let* ((bib-links (-filter
                       (lambda (el)
			 (string= (org-element-property :type el) "bibliography"))
                       (org-element-map (org-element-parse-buffer) 'link 'identity)))
           (n-bib-links (length bib-links)))

      (when (> n-bib-links 1)
	(mapc (lambda (link)
		(setq
		 bib-candidates
		 (append
                  bib-candidates
                  (list (format  "Multiple bibliography link: %s"
				 (org-element-property :raw-link link))))))
              bib-links)))

    ;; Check for bibliography files existence.
    (mapc (lambda (bibfile)
            (unless (file-exists-p bibfile)
              (cl-pushnew
	       (format "%s does not exist." bibfile)
	       bib-candidates)))
          (org-ref-find-bibliography))

    ;; check for spaces in bibliography
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc (lambda (bibfile)
              (when (string-match " " bibfile)
		(cl-pushnew
		 (format "One or more spaces found in path to %s. No spaces are allowed in bibtex file paths. We recommend replacing them with -. Underscores usually cause other problems." bibfile)
		 bib-candidates)))
            bibfiles))

    ;; validate bibtex files
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc
       (lambda (bibfile)
	 (unless (with-current-buffer
                     (find-file-noselect bibfile)
                   (bibtex-validate))
           (cl-pushnew
	    (format  "Invalid bibtex file found. [[file:%s]]\n" bibfile)
	    bib-candidates)))
       bibfiles)
      ;; check types
      (mapc
       (lambda (bibfile)
	 (with-current-buffer
             (find-file-noselect bibfile)
	   (goto-char (point-min))
	   (while (re-search-forward "^@\\(.*?\\)[({]" nil t)
	     (when (and (not (string= "string" (downcase (match-string-no-properties 1))))
			(not (member (s-trim (downcase (match-string-no-properties 1)))
				     (cdr (assoc bibtex-dialect
						 (list
						  (cons 'BibTeX (mapcar (lambda (e) (downcase (car e)))
									bibtex-BibTeX-entry-alist))
						  (cons 'biblatex (mapcar (lambda (e) (downcase (car e)))
									  bibtex-biblatex-entry-alist))))))))
	       (cl-pushnew
		(format  "Invalid bibtex entry type (%s) found in [[file:%s::%s]]\n" (match-string-no-properties 1)
			 bibfile (line-number-at-pos))
		bib-candidates)))))
       bibfiles))

    ;; unreferenced labels
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((matches '()))
	  ;; these are the org-ref label:stuff  kinds
	  (while (re-search-forward
		  "[^#+]label:\\([a-zA-Z0-9:-]*\\)" nil t)
	    (cl-pushnew (cons
			 (match-string-no-properties 1)
			 (point))
			matches))
	  ;; now add all the other kinds of labels.
	  ;; #+label:
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "^#\\+label:\\s-+\\(.*\\)\\b" nil t)
	      ;; do not do this for tables. We get those in `org-ref-get-tblnames'.
	      ;; who would have thought you have save match data here? Trust me. When
	      ;; I wrote this, you did.
	      (unless (save-match-data  (equal (car (org-element-at-point)) 'table))
		(cl-pushnew (cons (match-string-no-properties 1) (point)) matches))))

	  ;; \label{}
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "\\\\label{\\([a-zA-Z0-9:-]*\\)}"
				      nil t)
	      (cl-pushnew (cons (match-string-no-properties 1) (point)) matches)))

	  ;; #+tblname: and actually #+label
	  (cl-loop for cell in (org-element-map (org-element-parse-buffer 'element) 'table
				 (lambda (table)
				   (cons (org-element-property :name table)
					 (org-element-property :begin table))))
		   do
		   (cl-pushnew cell matches))

	  ;; CUSTOM_IDs
	  (org-map-entries
	   (lambda ()
	     (let ((custom_id (org-entry-get (point) "CUSTOM_ID")))
	       (when (not (null custom_id))
		 (cl-pushnew (cons custom_id (point)) matches)))))

	  (goto-char (point-min))
	  (while (re-search-forward "^#\\+name:\\s-+\\(.*\\)" nil t)
	    (cl-pushnew (cons (match-string 1) (point)) matches))


	  ;; unreference labels
	  (let ((refs (org-element-map (org-element-parse-buffer) 'link
			(lambda (el)
			  (when (or (string= "ref" (org-element-property :type el))
				    (string= "eqref" (org-element-property :type el))
				    (string= "pageref" (org-element-property :type el))
				    (string= "nameref" (org-element-property :type el))
				    (string= "autoref" (org-element-property :type el))
				    (string= "cref" (org-element-property :type el))
				    (string= "Cref" (org-element-property :type el)))
			    (org-element-property :path el))))))
	    (cl-loop for (label . p) in matches
		     do
		     (when (and label (not (-contains? refs label)))
		       (cl-pushnew
			(cons label (set-marker (make-marker) p))
			unreferenced-labels)))))))


    (with-current-buffer buf
      (when bad-citations
	(insert "\n* Bad citations\n")
	(cl-loop for (key . marker) in bad-citations
		 do
		 (setq mbuffer (buffer-name (marker-buffer marker))
		       mchar (marker-position marker))
		 (insert (format "- [[elisp:(progn (switch-to-buffer %S) (goto-char %S)(org-show-entry))][%s]]\n"
				 mbuffer mchar key))))
      (when bad-refs
	(insert "\n* Bad ref links\n")
	(cl-loop for (key . marker) in bad-refs
		 do
		 (setq mbuffer (buffer-name (marker-buffer marker))
		       mchar (marker-position marker))
		 (insert (format "- [[elisp:(progn (switch-to-buffer %S) (goto-char %S)(org-show-entry))][%s]]\n"
				 mbuffer mchar key))))

      (when bad-labels
	(insert "\n* Multiply defined label links\n")
	(cl-loop for (key . marker) in bad-labels
		 do
		 (setq mbuffer (buffer-name (marker-buffer marker))
		       mchar (marker-position marker))
		 (insert (format "- [[elisp:(progn (switch-to-buffer %S) (goto-char %S)(org-show-entry))][%s]]\n"
				 mbuffer mchar key))))

      (when bad-files
	(insert "\n* Bad files\n")
	(cl-loop for (fname pos) in bad-files do
		 (insert (format "- [[elisp:(goto-char %s)][%s]]\n" pos fname))))

      (when unreferenced-labels
	(insert "\n* Unreferenced label links\n")
	(cl-loop for (key . marker) in unreferenced-labels
		 when (not (string= key ""))
		 do
		 (setq mbuffer (buffer-name (marker-buffer marker))
		       mchar (marker-position marker))
		 (insert (format "- [[elisp:(progn (switch-to-buffer %S) (goto-char %S)(org-show-entry))][%s]]\n"
				 mbuffer mchar key))))

      (when bib-candidates
	(insert "\n* Bibliography\n\n")
	(cl-loop for candidate in bib-candidates
		 do
		 (insert (format "- %s" candidate))))

      (insert "\n* Miscellaneous\n\n")
      (cl-loop for s in `(,(format "org-latex-prefer-user-labels = %s"
				   org-latex-prefer-user-labels)
			  ,(format "bibtex-dialect = %s" bibtex-dialect)
			  ,(format "biblatex is%srequired." (if biblatex-required " " " not "))
			  ,(format "biblatex is%sused." (if biblatex-used " " " not "))
			  ,(format "emacs-version = %s" (emacs-version))
			  ,(format "org-version = %s" (org-version))
			  ,(org-ref-version)
			  ,(format "org-ref.el installed at %s" (concat
								 (file-name-sans-extension
								  (locate-library "org-ref"))
								 ".el"))
			  ,(format "org-ref-insert-cite-function = %s" org-ref-insert-cite-function)
			  ,(format "org-ref-insert-label-function = %s" org-ref-insert-label-function)
			  ,(format "org-ref-insert-ref-function = %s" org-ref-insert-ref-function)
			  ,(format "org-ref-cite-onclick-function = %s" org-ref-cite-onclick-function)
			  ,(format "org-latex-pdf-process is defined as %s" org-latex-pdf-process)
			  ,(format "natbib is%srequired." (if natbib-required " " " not "))
			  ,(format "natbib is%sin %s or %s."
				   (if natbib-used " " " not ")
				   (propertize "org-latex-default-packages-alist"
					       'help-echo (format "%S" (mapconcat
									(lambda (s)
									  (format "%s" s))
									org-latex-default-packages-alist
									"\n"))
					       'font-lock-face '(:foreground "red3"))
				   (propertize "org-latex-packages-alist"
					       'help-echo (format "%S" (mapconcat
									(lambda (s)
									  (format "%s" s))
									org-latex-packages-alist
									"\n"))
					       'font-lock-face '(:foreground "red3")))
			  ,(format "cleveref is%srequired." (if cleveref-required " " " not "))
			  ,(format "cleveref is%sin %s or %s."
				   (if cleveref-used " " " not ")
				   (propertize "org-latex-default-packages-alist"
					       'help-echo (format "%S" (mapconcat
									(lambda (s)
									  (format "%s" s))
									org-latex-default-packages-alist
									"\n"))
					       'font-lock-face '(:foreground "red3"))
				   (propertize "org-latex-packages-alist"
					       'help-echo (format "%S" (mapconcat
									(lambda (s)
									  (format "%s" s))
									org-latex-packages-alist
									"\n"))
					       'font-lock-face '(:foreground "red3")))
			  ,(format "bibtex-completion installed = %s" (featurep 'bibtex-completion))
			  ,(format "bibtex-completion loaded = %s" (fboundp 'bibtex-completion-candidates)))
	       do
	       (insert "- " s "\n"))
      (insert (format "- org-latex-default-packages-alist\n"))
      (cl-loop for el in org-latex-default-packages-alist
	       do
	       (insert (format "  %S\n" el)))

      (if (null org-latex-packages-alist)
	  (insert "-  org-latex-packages-alist is nil\n")
	(insert "-  org-latex-packages-alist\n")
	(cl-loop for el in org-latex-packages-alist
		 do
		 (insert (format "  %S\n" el))))


      (insert (format "- ox-bibtex loaded = %s\n" (featurep 'ox-bibtex)))
      (insert (format "- ox-bibtex loaded after org-ref = %s\n"
		      (let ((org-ref-i (seq-position load-history (assoc (locate-library "org-ref") load-history)) )
			    (ox-bibtex-i (seq-position load-history (assoc (locate-library "ox-bibtex") load-history))))
			(and org-ref-i ox-bibtex-i
			     (> org-ref-i ox-bibtex-i)))))

      (insert (format "- ebib loaded = %s\n" (featurep 'ebib)))
      (insert (format "- ebib loaded after org-ref = %s\n"
		      (let ((org-ref-i (seq-position load-history (assoc (locate-library "org-ref") load-history)) )
			    (ebib-i (seq-position load-history (assoc (locate-library "ebib") load-history))))
			(and org-ref-i ebib-i
			     (> org-ref-i ebib-i)))))



      (insert "- cite link definition:\n" (with-temp-buffer
					    (insert (format "%S" (assoc "cite" org-link-parameters)))
					    (pp-buffer)
					    (buffer-string)))

      (insert "\n* LaTeX setup\n\n")
      (cl-loop for executable in '("latex" "pdflatex" "bibtex" "biblatex"
				   "makeindex" "makeglossaries")
	       do
	       (insert (format "%s is installed at %s\n" executable (executable-find executable))))

      (insert "\n* Warnings\n")
      (if (get-buffer "*Warnings*")
	  (cl-loop for line in (s-split "\n" (with-current-buffer "*Warnings*"
					       (buffer-string)))
		   if (s-starts-with?  "Warning (org-ref):" line)
		   do
		   (insert " - " line "\n"))
	(insert "- No (org-ref) Warnings found."))


      (insert (format  "\n* Utilities

- [[elisp:(progn (find-file %S) (ispell))][Spell check document]]
- [[elisp:(progn (find-file %S) (org-ref))][recheck document with org-ref]]
" fname fname))
      (goto-char (point-min))

      ;; (setq header-line-format "Press q to quit.")
      ;; (local-set-key "q"
      ;; 		     #'(lambda ()
      ;; 			 (interactive)
      ;; 			 (delete-window)))
      (read-only-mode))

    (display-buffer-in-side-window buf '((side . right)))))



;;** Find non-ascii characters
;;;###autoload
(defun org-ref-find-non-ascii-characters ()
  "Find non-ascii characters in the buffer.  Useful for cleaning up bibtex files."
  (interactive)
  (occur "[^[:ascii:]]"))


;;* Utilities

;;** Extract bibtex entries in org-file

;;;###autoload
(defun org-ref-extract-bibtex-to-file (bibfile &optional clobber)
  "Extract all bibtex entries for citations buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u),
which will CLOBBER the file."
  (interactive
   (list (read-file-name "Bibfile: " nil nil nil
			 (file-name-nondirectory
			  (concat (file-name-sans-extension
				   (buffer-file-name))
				  ".bib")))
	 current-prefix-arg))

  (let* ((bibtex-files (org-ref-find-bibliography))
	 (keys (reverse (org-ref-get-bibtex-keys)))
	 (bibtex-entry-kill-ring-max (length keys))
	 (bibtex-entry-kill-ring '())
	 (kill-cb (not (find-buffer-visiting bibfile)))
	 (cb (find-file-noselect bibfile))
	 (current-bib-entries (with-current-buffer cb
				(prog1
				    (buffer-string)
				  (when kill-cb (kill-buffer cb))))))

    (save-window-excursion
      (cl-loop for key in keys
	       do
	       (bibtex-search-entry key t)
	       (bibtex-kill-entry t)))

    (with-temp-file bibfile
      (unless clobber (insert current-bib-entries))
      (insert (mapconcat
	       'identity
	       bibtex-entry-kill-ring
	       "\n\n")))))


;;;###autoload
(defun org-ref-extract-bibtex-entries ()
  "Extract the bibtex entries in the current buffer into a bibtex src block."
  (interactive)
  (let* ((bibtex-files (org-ref-find-bibliography))
	 (keys (reverse (org-ref-get-bibtex-keys)))
	 (bibtex-entry-kill-ring-max (length keys))
	 (bibtex-entry-kill-ring '()))

    (save-window-excursion
      (cl-loop for key in keys
	       do
	       (bibtex-search-entry key t)
	       (bibtex-kill-entry t)))

    (goto-char (point-max))
    (insert "\n\n")
    (org-insert-heading)
    (insert (format " Bibtex entries

#+BEGIN_SRC bibtex :tangle %s
%s
#+END_SRC"
		    (let ((bibfile (concat (file-name-base
					    (or (buffer-file-name) "references"))
					   ".bib")))
		      (if (file-exists-p bibfile)
			  (file-name-nondirectory
			   (read-file-name "Bibfile: " nil nil nil bibfile))
			bibfile))
		    (mapconcat
		     'identity
		     bibtex-entry-kill-ring
		     "\n\n")))))


;;** Extract cited pdfs
;;;###autoload
(defun org-ref-extract-cited-pdfs (newdir)
  "Copy PDFs in citations in current buffer to NEWDIR."
  (interactive (list (read-directory-name "Copy to: ")))
  ;; Make sure newdir exists
  (unless (file-directory-p newdir)
    (mkdir newdir t))
  
  (cl-loop for key in (org-ref-get-bibtex-keys) do
	   (let ((pdf (org-ref-get-pdf-filename key)))
	     (if (file-exists-p pdf)
		 (progn
		   (message "Copying %s to %s." pdf newdir)
		   (copy-file pdf newdir t))
	       (message "%s not found" pdf)))))


(provide 'org-ref-utils)
;;; org-ref-utils.el ends here
