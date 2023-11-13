;;; ebib.el --- a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2023 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2003
;; Version: 2.39
;; Keywords: text bibtex
;; URL: http://joostkremers.github.io/ebib/
;; Package-Requires: ((parsebib "4.0") (emacs "26.1") (compat "29.1.4.3"))

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Ebib is a BibTeX database manager that runs in GNU Emacs.  With Ebib, you
;; can create and manage .bib-files, all within Emacs.  It supports @String
;; and @Preamble definitions, multi-line field values, searching, and
;; integration with Emacs' (La)TeX mode, Org mode and others.

;; See the Ebib manual for usage and installation instructions.

;; The latest release version of Ebib, contact information and mailing list
;; can be found at <http://joostkremers.github.io/ebib>.  Development
;; sources can be found at <https://github.com/joostkremers/ebib>.

;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'bibtex)
(require 'seq)
(require 'crm)
(require 'pp)
(require 'hl-line)
(require 'mule-util)
(require 'compat)  ; for `pos-bol', `pos-eol'.
(require 'parsebib)
(require 'button)
(require 'ebib-utils)
(require 'ebib-db)
(require 'ebib-filters)
(require 'ebib-keywords)
(require 'ebib-notes)
(require 'ebib-reading-list)

;; Make sure `ivy' & `helm' are loaded during compilation.
(require 'ivy nil 'noerror)
(require 'helm nil 'noerror)

;;; Silence the byte-compiler.
(defvar pandoc-mode)
(defvar selectrum-minibuffer-map)
(defvar ivy-minibuffer-map)
(defvar ivy-sort-max-size)
(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function pandoc--get "ext:pandoc-mode-utils.el" (option &optional buffer))
(declare-function ivy-read "ext:ivy.el" (prompt collection &rest args))
(declare-function helm-marked-candidates "ext:helm.el" (&optional with-wildcard all-sources))
(declare-function helm-build-sync-source "ext:helm-source.el" (name &rest args))
(declare-function helm "ext:helm.el" (&rest plist))

;;; Helper functions

(defun ebib--display-buffer-reuse-window (buffer _)
  "Display BUFFER in an existing Ebib window.
If BUFFER is the index buffer, simply switch to the window
displaying it.  (This function should not be called if there is a
chance the index buffer is not visible.) For any other buffer,
find a window displaying an Ebib buffer other than the index
buffer, switch to that window and display BUFFER.  If no window
can be found, return nil."
  (let (window)
    (cond
     ;; The index buffer can only be displayed in its dedicated window.
     ((eq buffer (ebib--buffer 'index))
      (setq window (get-buffer-window buffer)))
     ;; If `ebib-layout' isn't `full', the multiline buffer should not be
     ;; displayed in an Ebib buffer.
     ((and (memq buffer ebib--multiline-buffer-list)
           (not (eq ebib-layout 'full)))
      (setq window nil))
     ;; Find a buffer other than the index buffer that's being displayed.
     (t (setq window (let ((b (cdr (seq-find (lambda (elt) (and (not (eq (car elt) 'index))
                                                                (get-buffer-window (cdr elt))))
                                             ebib--buffer-alist))))
                       (if b (get-buffer-window b))))))
    (when window
      (with-ebib-window-nondedicated (selected-window)
        (set-window-buffer window buffer))
      window)))

(defun ebib--display-buffer-largest-window (buffer _)
  "Display BUFFER in the largest non-dedicated window."
  (unless ebib-popup-entry-window
    (let ((window (get-largest-window)))
      (set-window-buffer window buffer)
      window)))

(defun ebib--pop-to-buffer (buffer)
  "Select or create a window to display BUFFER and display it.
If the index buffer isn't visible, this function does nothing.
Otherwise, if BUFFER is the index buffer, simply switch to its
window.  For any other buffer, if there is a visible Ebib buffer
other than the index buffer, switch to its window and display
BUFFER.  If there is no Ebib window, use the largest non-dedicated
window or, if `ebib-layout' is set to `index-only', pop up a new
window.  If all else fails, pop up a new frame."
  ;; If the index buffer isn't visible, do nothing.
  (unless (not (get-buffer-window (ebib--buffer 'index)))
    (pop-to-buffer buffer
                   '((ebib--display-buffer-reuse-window
                      ebib--display-buffer-largest-window
                      display-buffer-pop-up-window
                      display-buffer-pop-up-frame))
                   t)))

(defun ebib--display-entry-key (key &optional mark)
  "Display BibTeX item designated by KEY in the index buffer at POINT.
Included in the display are the data in the fields specified in
`ebib-index-columns'.  The item is given the text property
`ebib-key' with KEY as value.  If MARK is t, `ebib-marked-face'
is applied to the item."
  (let ((data (ebib--get-tabulated-data key))
        (n 0)
        (max (1- (length ebib-index-columns)))
        (ellipsis-width (length truncate-string-ellipsis)))
    (with-current-ebib-buffer 'index
      (while (< n max)
        (let* ((width (cadr (nth n ebib-index-columns)))
               (item (nth n (cadr data)))
               ;; Strings that are too long for their column are truncated, but
               ;; if the column is too narrow, we prefer not to display
               ;; anything.
               (display-item (if (and (< width (+ 5 ellipsis-width))
                                      (< width (length item)))
                                 ""
                               (truncate-string-to-width item width nil nil t))))
          (insert (bidi-string-mark-left-to-right
                   (format (concat "%-" (int-to-string width) "s")
                           display-item))
                  ebib-index-column-separator))
        (cl-incf n))
      ;; The last item isn't truncated.
      (insert (nth n (cadr data)))
      ;; Add a text property to identify the entry.
      (add-text-properties (pos-bol) (point) `(ebib-key ,key))
      (insert "\n")
      (when mark
        (add-text-properties (pos-bol 0) (point) '(face ebib-marked-face))))))

(defun ebib--goto-entry-in-index (key)
  "Move point to the entry designated by KEY.
Point is placed at the beginning of the line.  If there is no
entry with KEY in the buffer, point is not moved."
  (with-current-ebib-buffer 'index
    (let ((p (point)))
      (goto-char (point-min))
      (while (not (or (string= key (get-text-property (point) 'ebib-key))
                      (eobp)))
        (forward-line 1))
      (if (eobp)
          (goto-char p)
        (set-window-point (get-buffer-window) (point))))))

(defun ebib--get-tabulated-data (key)
  "Get data for KEY.
Return value is a list consisting of KEY and a list of the
values of the fields listed in `ebib-index-columns'."
  (list key (mapcar (lambda (elt)
                      (ebib--first-line (ebib--get-field-value-for-display (car elt) key ebib--cur-db)))
                    ebib-index-columns)))

(defun ebib--insert-entry-in-index-sorted (key &optional move-point mark)
  "Insert KEY in the index buffer obeying the sort order.
Unless MOVE-POINT is non-nil, this function does not move point.
If MARK is non-nil, `ebib-mark-face' is applied to the entry."
  (with-current-ebib-buffer 'index
    (let ((inhibit-read-only t))
      (let* ((keys-list (ebib--sort-keys-list (ebib-db-list-keys ebib--cur-db) ebib--cur-db))
             (pos (seq-position keys-list key #'string=))
             (new-pos (save-excursion
                        (goto-char (point-min))
                        (forward-line pos)
                        (ebib--display-entry-key key mark)
                        (point))))
        (when move-point
          (goto-char new-pos)
          (forward-line -1)
          (set-window-point (get-buffer-window) (point))
          (hl-line-highlight))))))

(defun ebib--redisplay-field (field)
  "Redisplay the contents of FIELD in the current buffer."
  (with-current-ebib-buffer 'entry
    ;; If the `=type=', `crossref' or `xdata' field has changed, we need to redisplay the
    ;; entire entry.
    (if (member-ignore-case field '("=type=" "crossref" "xdata"))
        (progn
          (ebib--update-entry-buffer)
          (re-search-forward (rx-to-string `(seq bol ,(string-trim field "=" "=")) t))) ; Remove =-signs from `=type='.
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(re-search-forward (format "^%s" field))
        (delete-region (pos-bol) (next-single-property-change (point) 'ebib-field-end))
	(save-excursion
          (insert (format "%-17s %s"
                          (propertize field 'face 'ebib-field-face)
                          (ebib--get-field-highlighted field (ebib--get-key-at-point)))))
        (beginning-of-line)))))

(defun ebib--redisplay-current-field ()
  "Redisplay the contents of the current field in the entry buffer."
  (ebib--redisplay-field (ebib--current-field)))

(defun ebib--generate-string-display (string)
  "Get a formatted string for displaying abbrev STRING."
  (let* ((def (ebib-get-string string ebib--cur-db 'noerror nil))
	 (rawp (ebib-unbraced-p def))
	 (unbraced-str (ebib-unbrace def))
	 (multilinep (ebib--multiline-p unbraced-str))
	 (str (if multilinep
		  (ebib--first-line unbraced-str)
		unbraced-str))
	 (flags (concat (if rawp "*" "")
			(if multilinep "+" "")))
	 (expansion (if rawp (concat "[" (ebib-get-string string ebib--cur-db 'noerror 'unbraced 'expand) "]") "")))
    (format "%-18s %2s%s  %s"
	    string	 ;; Abbreviation
	    flags	 ;; Raw/multiline indicators
	    str		 ;; Definition (presented without unbraced)
	    expansion))) ;; Full expansion when unbraced

(defun ebib--redisplay-strings-buffer ()
  "Redisplay all strings in strings buffer."
  (with-current-ebib-buffer 'strings
    (let ((inhibit-read-only t)
	  (string (ebib--current-string)))
      (erase-buffer)
      (ebib--fill-strings-buffer)
      (re-search-forward
       (rx-to-string `(: line-start ,string (syntax -)) t)))))

(defun ebib--convert-multiline-to-string (multilines)
  "Convert MULTILINES to a single multiline string.
MULTILINES is a list of strings.  The resulting string is
suitable for display in the entry buffer: each string in
MULTILINES corresponds to a line in the resulting string, and all
lines except the first one are prepended with 19 spaces."
  (let ((first-line (car multilines))
        (rest-lines (mapcar (lambda (line)
                              (concat (make-string 19 ?\s) (string-trim-left line)))
                            (cdr multilines))))
    (concat first-line
            (if rest-lines
                (concat "\n" (string-join rest-lines "\n"))))))

(defun ebib--display-multiline-field (string matched)
  "Return a string for multiline field values to display in the entry buffer.
STRING is the text to display.  MATCHED indicates whether a
search string match was found.  If the text is longer than
`ebib-multiline-display-max-lines' lines, it is truncated and a
continuation marker \"[...]\" is added.  If MATCHED is non-nil,
this continuation marker is highlighted.  Empty lines from the
beginning and end of STRING are removed.

This function calls the function in
`ebib-multiline-display-function' to convert the text to a list
of strings."
  (let ((multilines (funcall ebib-multiline-display-function string))
        (truncated nil))
    (when (> (length multilines) ebib-multiline-display-max-lines)
      (setq multilines (seq-subseq multilines 0 ebib-multiline-display-max-lines))
      (setq truncated t))
    (setq multilines (thread-last multilines
                       (seq-drop-while (lambda (elt) (string= elt "")))
                       (reverse)
                       (seq-drop-while (lambda (elt) (string= elt "")))
                       (reverse)))
    (cl-values (ebib--convert-multiline-to-string multilines)
               (if truncated
                   (concat "\n" (make-string 19 ?\s)
                           (if matched
                               (propertize "[...]" 'face 'highlight)
                             "[...]"))))))

(defun ebib--display-file-field (file-field)
  "Return a string for FILE-FIELD to display in the entry buffer."
  (let ((files (ebib--split-files file-field)))
    (ebib--convert-multiline-to-string (mapcar (lambda (file)
						 (propertize file
							     'face 'button
							     'font-lock-face 'button
							     'mouse-face 'highlight
							     'help-echo "mouse-1: open this file"
							     'button t
							     'follow-link t
							     'category t
							     'button-data file
							     'keymap button-map
							     'action 'ebib--call-file-viewer))
                                               files))))

(defun ebib--display-xdata-field (xdata-field)
  "Return a string for XDATA-FIELD to display in the entry buffer.
Separate values by commas and put each on a separate line.  For
each value, test if there is an entry with that key, and that it
is actually an `@XData' entry.  If either test fails, propertize
the value with `ebib-warning-face' and an information help-echo
message."
  (let ((keys (split-string xdata-field ",[[:space:]]*")))
    (ebib--convert-multiline-to-string
     (mapcar
      (lambda (key)
	(if-let ((type (ebib-db-get-field-value "=type=" key ebib--cur-db 'noerror)))
	    (if (cl-equalp "xdata" type)
		key
	      (propertize key
			  'face 'ebib-warning-face
			  'help-echo (format "%s is not an @XData entry." key)))
	  (propertize key
		      'face 'ebib-warning-face
		      'help-echo (format "No entry with key `%s'" key))))
      keys))))

(defun ebib--display-related-field (related-field)
  "Return a string for RELATED-FIELD to display in the entry buffer.
Separate values by commas and put each on a separate line.  For
each value, test if there is an entry with that key.  If not,
propertize the value with `ebib-warning-face' and an information
help-echo message."
  (ebib--convert-multiline-to-string
   (mapcar
    (lambda (key)
      (if (ebib-db-has-key key ebib--cur-db)
	  key
	(propertize key
		    'face 'ebib-warning-face
		    'help-echo (format "No entry with key `%s'" key))))
    (split-string related-field ",[[:space:]]*"))))

(defun ebib--display-doi-field (doi-field)
  "Return a string for DOI-FIELD to display in the entry buffer."
  (propertize doi-field
	      'face 'button
	      'font-lock-face 'button
	      'mouse-face 'highlight
	      'help-echo "mouse-1: follow this doi"
	      'button t
	      'follow-link t
	      'category t
	      'button-data (concat "https://dx.doi.org/" doi-field)
	      'keymap button-map
	      'action 'ebib--call-browser))

(defun ebib--display-url-field (url-field)
  "Return a string for URL-FIELD to display in the entry buffer."
  (let ((urls (ebib--split-urls url-field)))
    (ebib--convert-multiline-to-string
     (mapcar (lambda (url)
	       (propertize url
			   'face 'button
			   'font-lock-face 'button
			   'mouse-face 'highlight
			   'help-echo "mouse-1: follow this url"
			   'button t
			   'follow-link t
			   'category t
			   'button-data url
			   'keymap button-map
			   'action 'ebib--call-browser))
             urls))))

(defun ebib--display-crossref-field (crossref)
  "Return a string for CROSSREF to display in the entry buffer."
  (propertize crossref
	      'face 'button
	      'font-lock-face 'button
	      'mouse-face 'highlight
	      'help-echo "mouse-1: follow crossref"
	      'button t
	      'follow-link t
	      'category t
	      'button-data nil
	      'keymap button-map
	      'action (lambda (_) (ebib-follow-crossref))))

(defun ebib--extract-note-text (key &optional truncate)
  "Extract the text of the note for entry KEY.
This function simply calls `ebib-notes-extract-text-function'.
KEY and TRUNCATE are passed on unchanged.  For their meaning, see
the doc string of `ebib-extract-note-text-default'."
  (funcall ebib-notes-extract-text-function key truncate))

(defun ebib-extract-note-text-default (key truncate)
  "Extract the text of the note for KEY.
The note must be an Org entry under its own headline.

If TRUNCATE is non-nil, the note is truncated at
`ebib-notes-display-max-lines' lines.  If the original text is
longer than that, an ellipsis marker \"[...]\" is added.

The return value is a list of strings, each a separate line,
which can be passed to `ebib--display-multiline-field'."
  (with-temp-buffer
    (cond
     ((eq ebib-notes-storage 'multiple-notes-per-file)
      (let* ((location (ebib--notes-goto-note key))
             (buffer (car location))
             (position (cdr location))
             beg end)
        (when location
          (with-current-buffer buffer
            (save-mark-and-excursion
              (goto-char position)
              (org-mark-subtree)
              (setq beg (region-beginning)
                    end (region-end))))
          (insert-buffer-substring buffer beg end))))
     ((eq ebib-notes-storage 'one-file-per-note)
      (let ((filename (expand-file-name (ebib--create-notes-file-name key))))
        (when (file-readable-p filename)
          (insert-file-contents filename)))))
    (let ((truncated nil)
	  string)
      ;; If appropriate, first reduce the size of the text we need to
      ;; pass to `org-element-parse-buffer', since this function can
      ;; be slow if the note is long.
      (when truncate
	(let ((max (progn
                     (goto-char (point-min))
                     (pos-bol (* 2 ebib-notes-display-max-lines)))))
	  (when (< max (point-max))
            (setq truncated t)
            (delete-region max (point-max)))))

      ;; Extract any property drawers.
      (let ((contents (org-element-parse-buffer)))
	(org-element-map contents 'property-drawer
          (lambda (drawer)
            (org-element-extract-element drawer)))
	(erase-buffer)
	(insert (org-element-interpret-data contents)))
      ;; Extract relevant lines
      (let* ((beg (progn
                    (goto-char (point-min))
                    (forward-line 1)	; Skip the headline.
                    (point)))
	     ;; If `truncate', then take the first
	     ;; `ebib-notes-display-max-lines' lines.
             (end (if truncate
		      (progn
			(goto-char (point-min))
			(forward-line (1+ ebib-notes-display-max-lines))
			(point))
		    ;; Otherwise take all lines
		    (point-max))))
        (setq string (buffer-substring-no-properties beg end))
	(if (or truncated
		(< end (point-max)))
            (setq string (concat string "[...]\n"))))
      (split-string string "\n"))))

(defun ebib--get-field-highlighted (field key &optional db match-str)
  "Return the contents of FIELD in entry KEY in DB with MATCH-STR highlighted."
  (or db (setq db ebib--cur-db))
  (let* ((case-fold-search t)
         (value (ebib-get-field-value field key db 'noerror nil 'xref 'expand-strings))
         (multiline "")
         (raw " ")
         (matched nil)
         (alias ""))
    ;; We have to do a couple of things now:
    ;; - Remove {} or "" around the value, if they're there.
    ;; - Search for `match-str'.
    ;; - Properly adjust the value if it's multiline.
    ;; But all this is not necessary if there was no value, so we test that first.
    (when value
      (if (get-text-property 0 'ebib--alias value)
          (setq alias (propertize (format "  [<== %s]" (cdr (assoc-string field ebib--field-aliases 'case-fold))) 'face 'ebib-alias-face)))
      (if (get-text-property 0 'ebib--expanded value)
          (setq value (propertize value 'face 'ebib-abbrev-face 'fontified t)))
      ;; Propertize any stretch of the value which has an `ebib--xref'
      ;; property with `ebib-crossref-face'
      (let* ((xref-lst (seq-filter (lambda (plist) (eq (caaddr plist) 'ebib--xref))
				   ;; This is a HACK, based on
				   ;; https://emacs.stackexchange.com/a/54181/34394
				   ;; FIXME Using `object-intervals' would be much quicker and more
				   ;; elegant here, but only once it's supported in a current
				   ;; version of Emacs!
				   (seq-partition (cdr-safe (read (substring (format "%S" value) 1))) 3)))
	     (int-lst (mapcar #'butlast xref-lst)))
	(mapc
	 (lambda (ints)
	   (add-text-properties
	    (car ints) (cadr ints)
	    `(face ebib-crossref-face
                   help-echo ,(format "Inherited from entry `%s'" (get-text-property (car ints) 'ebib--xref value)))
	    value))
	 int-lst))
      (if (and (member-ignore-case field '("crossref" "xref"))
               (not (ebib--find-db-for-key (ebib-unbrace value) ebib--cur-db)))
          (setq value (propertize value 'face 'ebib-warning-face)))
      (if (cl-equalp field "keywords")
          (let* ((keywords (ebib--keywords-to-list (ebib-unbrace value)))
                 (new-value (mapconcat (lambda (keyword)
                                         (if (not (member-ignore-case keyword ebib--keywords-completion-list))
                                             (propertize keyword 'face 'ebib-warning-face)
                                           keyword))
                                       keywords
                                       ebib-keywords-separator)))
            (setq value (if (ebib-unbraced-p value)
                            new-value
                          (ebib-brace new-value)))))
      (if (ebib-unbraced-p value)
          (setq raw "*")
        (setq value (ebib-unbrace value))) ; We have to make the value look nice.
      (when match-str
        (cl-multiple-value-setq (value matched) (ebib--match-all-in-string match-str value)))
      (when (ebib--multiline-p value)
        (cl-multiple-value-setq (value multiline) (ebib--display-multiline-field value matched)))
      (if (cl-equalp field "file")
          (setq value (ebib--display-file-field value)))
      (if (cl-equalp field "related")
	  (setq value (ebib--display-related-field value)))
      (if (cl-equalp field "xdata")
          (setq value (ebib--display-xdata-field value)))
      (if (cl-equalp field "url")
          (setq value (ebib--display-url-field value)))
      (if (cl-equalp field "doi")
          (setq value (ebib--display-doi-field value)))
      (if (cl-equalp field "crossref")
          (setq value (ebib--display-crossref-field value))))
    (concat raw value alias multiline)))

(defun ebib--display-fields (key &optional db match-str)
  "Display the fields of entry KEY in DB.
The fields are inserted in the current buffer with their values.
If MATCH-STR is provided, then when it is present in the value,
it is highlighted.  DB defaults to the current database."
  (or db
      (setq db ebib--cur-db))
  (let* ((dialect (ebib--get-dialect db))
         (entry (ebib-get-entry key db 'noerror 'xref))
         (entry-type (cdr (assoc "=type=" entry)))
         (req-fields (ebib--list-fields entry-type 'required dialect))
         (opt-fields (ebib--list-fields entry-type 'optional dialect))
         (extra-fields (ebib--list-fields entry-type 'extra dialect))
         (undef-fields (seq-remove #'ebib--special-field-p (mapcar #'car (ebib--list-undefined-fields entry dialect)))))
    (insert (format "%-18s %s%s"
                    (propertize "type" 'face 'ebib-field-face)
                    (if (assoc-string entry-type (ebib--list-entry-types dialect t) 'case-fold)
                        entry-type
                      (propertize entry-type 'face 'error))
                    (if (and (eq dialect 'biblatex)
                             (assoc-string entry-type ebib--type-aliases 'case-fold))
                        (propertize (format "  [==> %s]" (cdr (assoc-string entry-type ebib--type-aliases 'case-fold))) 'face 'ebib-alias-face)
                      ""))
            (propertize "\n" 'ebib-field-end t))
    (mapc (lambda (fields)
            (when fields ; If one of the sets is empty, we don't want an extra empty line.
              (insert "\n")
              (mapc (lambda (field)
                      (unless (and (not (ebib-get-field-value field key db 'noerror nil 'xref 'expand-strings)) ; If a field does not have a value,
                                   (member-ignore-case field ebib-hidden-fields)				; and is hidden, don't display it,
                                   ebib--hide-hidden-fields)							; unless the user wants to see all hidden fields.
                        (insert (format "%-17s %s"
                                        (propertize field 'face 'ebib-field-face)
                                        (ebib--get-field-highlighted field key db match-str))
                                ;; The final newline gets a special text
                                ;; property, so we can easily detect the end of
                                ;; a field.
                                (propertize "\n" 'ebib-field-end t))))
                    fields)))
          (list req-fields opt-fields extra-fields undef-fields))
    (when (and (eq ebib-notes-show-note-method 'top-lines)
               (ebib--notes-has-note key))
      (let ((note (ebib--extract-note-text key 'truncate)))
        (insert "\n"
                (format "%-18s %s"
                        (propertize "external note" 'face 'ebib-field-face)
                        (ebib--convert-multiline-to-string note))
                (propertize "\n" 'ebib-field-end t))))))

(defun ebib--key-in-index-p (key)
  "Return t if the entry for KEY is listed in the index buffer."
  (with-current-ebib-buffer 'index
    (goto-char (point-min))
    (while (not (or (string= key (ebib--get-key-at-point))
                    (eobp)))
      (forward-line 1))
    (not (eobp))))

(defun ebib--get-key-at-point ()
  "Return the key of the item at point.
If point is not on a BibTeX entry, return nil."
  (with-current-ebib-buffer 'index
    (get-text-property (point) 'ebib-key)))

(defun ebib--update-buffers (&optional no-index-refresh)
  "Redisplay the index and entry buffers.
The contents of both buffers are recreated, unless
NO-INDEX-REFRESH is non-nil, in which case the index buffer is
not recreated.  Instead, the index buffer of `ebib--cur-db' is
simply displayed.  If `ebib--cur-db' has no index buffer yet, one
is created.

Note that NO-INDEX-REFRESH does not affect the entry buffer: its
contents is recreated unconditionally."
  (ebib--update-index-buffer no-index-refresh)
  (ebib--update-entry-buffer))

(defun ebib--update-index-buffer (&optional no-refresh)
  "Recreate the contents of the index buffer using the keys of `ebib--cur-db'.
If `ebib--cur-db' is nil, the buffer is just erased and its name
set to \"Ebib (no file)\".  If NO-REFRESH is non-nil, display the
index buffer associated with `ebib--cur-db' but do not refresh
its contents, unless the buffer-local value of
`ebib--dirty-index-buffer' is non-nil.  If `ebib--cur-db' does
not have an associated index buffer, create one and fill it."
  (let ((window (get-buffer-window (ebib--buffer 'index)))
        (index-buffer (when ebib--cur-db
                        (ebib-db-get-buffer ebib--cur-db))))
    (when (not index-buffer)
      (setq index-buffer (ebib--get-or-create-index-buffer ebib--cur-db))
      (setq no-refresh nil)) ; We just created the index buffer, so we need to fill it.
    (if (buffer-local-value 'ebib--dirty-index-buffer index-buffer)
        (setq no-refresh nil))
    (setcdr (assq 'index ebib--buffer-alist) index-buffer)
    (when window ; Just to be sure, but `window' shouldn't be nil.
      (with-selected-window window
        (with-ebib-window-nondedicated (selected-window)
          (switch-to-buffer index-buffer))))
    (with-current-ebib-buffer 'index
      (let ((cur-entry (ebib--db-get-current-entry-key ebib--cur-db)))
        (unless no-refresh
          (let ((inhibit-read-only t))
            (erase-buffer)
            (when ebib--cur-db
              (let ((cur-keys-list (ebib--list-keys))
                    (marked-entries (ebib-db-list-marked-entries ebib--cur-db)))
                ;; We may call this function when there are no entries in the
                ;; database. If so, we don't need to do this:
                (unless (= 0 (ebib-db-count-entries ebib--cur-db))
                  ;; It may be that no entry satisfies the filter.
                  (if (not cur-keys-list)
                      (message "No entries matching the filter")
                    ;; Fill the buffer.
                    (dolist (entry cur-keys-list)
                      (ebib--display-entry-key entry (member entry marked-entries)))
                    ;; Make sure the current entry is among the visible entries.
                    (unless (member cur-entry cur-keys-list)
                      (setq cur-entry (car cur-keys-list)))))))
            (with-current-buffer index-buffer
              (setq ebib--dirty-index-buffer nil))))
        (if (and window cur-entry)
            (ebib--goto-entry-in-index cur-entry)
          (goto-char (point-min)))
        (hl-line-highlight))
      (ebib--rename-index-buffer))))

(defun ebib--rename-index-buffer ()
  "Rename the index buffer."
  (with-current-ebib-buffer 'index
    (rename-buffer (or (and ebib--cur-db
                            (concat (format " %d:" (1+ (- (length ebib--databases)
                                                          (length (member ebib--cur-db ebib--databases)))))
                                    (ebib-db-get-filename ebib--cur-db 'short)))
                       ebib--empty-index-buffer-name)
                   'unique)))

(defvar ebib--note-window nil "Window showing the current entry's note.")

(defun ebib--update-entry-buffer (&optional match-str)
  "Fill the entry buffer with the fields of the current entry.
MATCH-STR is a regexp that will be highlighted when it occurs in
the field contents."
  (when ebib--note-window
    (if (window-live-p ebib--note-window)
        (let ((buf (window-buffer ebib--note-window)))
          (delete-window ebib--note-window)
          (if (eq ebib-notes-storage 'one-file-per-note)
              (kill-buffer buf))
          (setq ebib--needs-update nil))) ; See below.
    (setq ebib--note-window nil))
  (with-current-ebib-buffer 'entry
    (let ((inhibit-read-only t)
          (key (ebib--get-key-at-point)))
      (erase-buffer)
      (when key   ; Are there entries being displayed?
        (ebib--display-fields key ebib--cur-db match-str)
        (goto-char (point-min))
        (if (and (get-buffer-window (ebib--buffer 'entry))
                 (eq ebib-notes-show-note-method 'all)
                 (eq ebib-layout 'full)
                 (ebib--notes-has-note key))
            (setq ebib--note-window (display-buffer (ebib-open-note (ebib--get-key-at-point)))
                  ;; If Ebib is lowered and then reopened, we need to redisplay
                  ;; the entry buffer, because otherwise the notes buffer isn't
                  ;; redisplayed. So we set the variable `ebib--needs-update' to
                  ;; t, which then causes the command `ebib' to redisplay the
                  ;; buffers. This is a hack, but the simplest way to do it.
                  ebib--needs-update t))))))

(defun ebib--maybe-update-entry-buffer ()
  "Update the entry buffer if it exists."
  (if (ebib--buffer 'entry)
      (ebib--update-entry-buffer)))

(defun ebib--set-modified (mod db &optional main dependents)
  "Set the modified flag MOD on database DB.
MOD must be either t or nil.  If DB is the current database, the
mode line is redisplayed, in order to correctly reflect the
database's modified status.

If MAIN is non-nil and DB is a dependent database, also set the
modified status of DB's main database to MOD.  If DEPENDENTS is
non-nil, it should be a list of dependent databases, whose
modified status is also set to MOD.  Note: it is ok to have MAIN
set to t if DB is not a dependent database: in such a case, MAIN
has no effect.  The DEPENDENTS are set to MOD unconditionally,
however, without checking to see if they are really dependents of
DB, or even dependents at all.

The return value is MOD."
  (unless db
    (setq db ebib--cur-db))
  (ebib-db-set-modified mod db)
  (when (and main (ebib-db-dependent-p db))
    (ebib-db-set-modified mod (ebib-db-get-main db)))
  (when dependents
    (mapc (lambda (dependent)
            (ebib-db-set-modified mod dependent))
          dependents))
  (when (eq db ebib--cur-db)
    (with-current-ebib-buffer 'index
      (force-mode-line-update)))
  mod)

(defun ebib--modified-p ()
  "Check if any of the databases in Ebib were modified.
Return the first modified database, or nil if none was modified."
  (seq-find (lambda (db)
              (ebib-db-modified-p db))
            ebib--databases))

(defun ebib--create-new-database (&optional main)
  "Create a new database instance and return it.
If MAIN is non-nil, create a dependent database for MAIN."
  (let ((new-db (ebib-db-new-database main)))
    (setq ebib--databases (append ebib--databases (list new-db)))
    new-db))

(defun ebib--list-keys (&optional db)
  "Return a list of entry keys in DB.
If a filter is active, only the keys of entries that match the
filter are returned.  The returned list is sorted.  DB defaults
to the current database."
  (or db (setq db ebib--cur-db))
  (let ((keys (if (ebib-db-get-filter db)
                  (ebib--filters-run-filter db)
                (ebib-db-list-keys db))))
    (ebib--sort-keys-list keys db)))

(defun ebib-read-database (prompt &optional databases)
  "Read the filename of a database, with completion.
The filenames of the databases in DATABASES are offered for
completion, the database associated with the selected filename is
returned.  DATABASES defaults to the databases
in`ebib--databases'.  PROMPT is the string used to prompt the
user."
  (or databases (setq databases ebib--databases))
  (ebib--get-db-from-filename (completing-read prompt (mapcar (lambda (s)
                                                                (ebib-db-get-filename s 'shortened))
                                                              databases)
                                               nil t)))

(defun ebib--completion-finish-key (command)
  "Return the key binding that finishes a completion command.
COMMAND is the command to finish, one of the symbols
`completing-read' or `read-file-name'."
  (cond
   ((and (boundp 'selectrum-mode) selectrum-mode) (key-description (where-is-internal 'selectrum-submit-exact-input (list selectrum-minibuffer-map) 'non-ascii)))
   ((and (boundp 'ivy-mode) ivy-mode) (key-description (where-is-internal 'ivy-immediate-done (list ivy-minibuffer-map) 'non-ascii)))
   ((and (boundp 'helm-mode) helm-mode) (let ((map (symbol-value (alist-get command '((completing-read . helm-comp-read-map)
										      (read-file-name . helm-read-file-map))))))
					  (key-description (where-is-internal 'helm-cr-empty-string (list map) 'non-ascii))))
   (t (key-description [return]))))

;;; Main

;;;###autoload
(defun ebib (&optional file key)
  "Ebib, a BibTeX database manager.
Optional argument FILE is a file to load.  If FILE is already
loaded, switch to it.  If KEY is given, jump to it."
  (interactive)
  ;; Save the buffer from which Ebib is called.
  (setq ebib--buffer-before (current-buffer))
  ;; And set it as the buffer to push entries to.
  (setq ebib--push-buffer (current-buffer))
  ;; See if there are local databases.
  (ebib--get-local-bibfiles)
  ;; See if there's a key at point.
  (or key (setq key (ebib--read-string-at-point "][^\"@\\&$#%',={} \t\n\f")))
  ;; Initialize Ebib if required.
  (unless ebib--initialized
    (ebib-init)
    (setq ebib--needs-update t)
    (ebib-check-notes-config))
  ;; Set up the windows.
  (ebib--setup-windows)
  ;; See if we have a file.
  (when file
    (setq ebib--cur-db (ebib--load-bibtex-file-internal (ebib--locate-bibfile file (append ebib-bib-search-dirs (list default-directory)))))
    (setq ebib--needs-update t))
  ;; See if we have a key.
  (when (and key (ebib--find-and-set-key key (buffer-local-value 'ebib-local-bibfiles ebib--buffer-before)))
    (setq ebib--needs-update t))
  (when ebib--needs-update
    (setq ebib--needs-update nil)
    (ebib--update-buffers 'no-refresh)))

(defun ebib--find-and-set-key (key files)
  "Make KEY the current entry.
FILES is a list of BibTeX files in which KEY is searched,
provided they are open in Ebib.  If FILES is nil, only the
current database is searched."
  (when ebib--databases
    (if (null files)
        (unless (member key (ebib-db-list-keys ebib--cur-db))
          (setq key nil))
      (let ((database (catch 'found
                        (mapc (lambda (file)
                                (let ((db (ebib--get-db-from-filename file)))
                                  (if (and db (member key (ebib-db-list-keys db)))
                                      (throw 'found db))))
                              files)
                        nil))) ; We must return nil if the key wasn't found anywhere.
        (if (null database)
            (setq key nil)
          (setq ebib--cur-db database))))
    (if key
        (ebib-db-set-current-entry-key key ebib--cur-db))))

(defun ebib--read-string-at-point (chars)
  "Read a string at POINT delimited by CHARS and return it.
CHARS is a string of characters that should not occur in the string."
  (save-excursion
    (skip-chars-backward (concat "^" chars))
    (let ((beg (point)))
      (ebib--looking-at-goto-end (concat "[^" chars "]*"))
      (buffer-substring-no-properties beg (point)))))

;;;###autoload
(defun ebib-init ()
  "Initialise Ebib.
This function sets all variables to their initial values, creates
the buffers and loads the files in `ebib-preload-bib-files'.

This function can be used to open Ebib in the background, e.g.,
in the user's init file."
  (setq ebib--saved-window-config nil)
  (ebib--create-buffers)
  (if ebib-keywords
      (ebib--keywords-load-canonical-list))
  (ebib--filters-load-file ebib-filters-default-file)
  (add-hook 'kill-emacs-query-functions 'ebib--kill-emacs-query-function)
  (add-hook 'kill-buffer-query-functions 'ebib--kill-multiline-query-function)
  (when ebib-preload-bib-files
    (mapc (lambda (file)
            (ebib--load-bibtex-file-internal (or (locate-file file ebib-bib-search-dirs)
                                                 (expand-file-name file))))
          ebib-preload-bib-files)
    (setq ebib--cur-db (car ebib--databases))) ; Display the first database in the list.
  (setq ebib--initialized t
        ebib--needs-update t))

(defun ebib--setup-windows ()
  "Create Ebib's window configuration.
If the index buffer is already visible in some frame, select its
window and make the frame active,"
  (let ((index-window (get-buffer-window (ebib--buffer 'index) t))
        (old-frame (selected-frame)))
    (if index-window
        (progn (select-window index-window t)
               (unless (eq (window-frame) old-frame)
                 (select-frame-set-input-focus (window-frame))
                 (setq ebib--frame-before old-frame)))
      (setq ebib--saved-window-config (current-window-configuration))
      (setq ebib--frame-before nil)
      (cond
       ((eq ebib-layout 'full)
        (delete-other-windows))
       ((eq ebib-layout 'custom)
        (setq ebib--window-before (selected-window))
        (delete-other-windows)
        (let ((width (cond
                      ((integerp ebib-width)
                       (- (window-total-width) ebib-width))
                      ((floatp ebib-width)
                       (- (window-total-width) (truncate (* (window-total-width) ebib-width)))))))
          (select-window (split-window (selected-window) width t)))))
      (let* ((index-window (selected-window))
             (entry-window (split-window index-window ebib-index-window-size
                                         ebib-window-vertical-split)))
        (switch-to-buffer (ebib--buffer 'index))
        (unless (eq ebib-layout 'index-only)
          (set-window-buffer entry-window (ebib--buffer 'entry)))
        (set-window-dedicated-p index-window t)
        (if (eq ebib-layout 'custom)
            (set-window-dedicated-p entry-window t)))))
  (if (buffer-local-value 'ebib--dirty-index-buffer (ebib--buffer 'index))
      (setq ebib--needs-update t)))

(defun ebib--create-buffers ()
  "Create the buffers for Ebib."
  ;; First we create a buffer to hold the fields of the current entry.
  (push (cons 'entry (get-buffer-create "*Ebib-entry*")) ebib--buffer-alist)
  (with-current-ebib-buffer 'entry
    (ebib-entry-mode)
    (buffer-disable-undo))
  ;; Then we create a buffer to hold the @String definitions.
  (push (cons 'strings (get-buffer-create "*Ebib-strings*")) ebib--buffer-alist)
  (with-current-ebib-buffer 'strings
    (ebib-strings-mode)
    (buffer-disable-undo))
  ;; The log buffer.
  (push (cons 'log (get-buffer-create "*Ebib-log*")) ebib--buffer-alist)
  (with-current-ebib-buffer 'log
    (erase-buffer)
    (insert "Ebib log messages\n\n(Press C-v or SPACE to scroll down, M-v or `b' to scroll up, `q' to quit.)\n\n")
    (ebib-log-mode)
    (buffer-disable-undo))
  ;; And lastly we create a buffer for the entry keys.
  (push (cons 'index (ebib--get-or-create-index-buffer)) ebib--buffer-alist))

(defun ebib--get-or-create-index-buffer (&optional db)
  "Create an index buffer for DB.
If DB already has an index buffer, return it instead.  If DB is
nil, get or create a buffer named \" Ebib (no file)\".  Return
the new buffer."
  (let ((buffer (if db
                    (or (ebib-db-get-buffer db)
                        (generate-new-buffer (ebib-db-get-filename db 'short)))
                  (get-buffer-create ebib--empty-index-buffer-name))))
    (with-current-buffer buffer
      (ebib-index-mode))
    (when db
      (ebib-db-set-buffer buffer db))
    buffer))

(defun ebib-check-notes-config ()
  "Check the user's notes configuration and adjust if necessary.
If `ebib-notes-file' is set, `ebib-notes-locations' is set to
`multiple-notes-per-file' and a warning is issued."
  (when (and (bound-and-true-p ebib-notes-file))
    (setq ebib-notes-storage 'multiple-notes-per-file)
    (ebib--log 'warning "Ebib's handling of external notes has changed.  Please visit the manual and update your configuration.")))

(defun ebib-force-quit ()
  "Force quit Ebib by call `ebib-quit'."
  (interactive)
  (ebib-quit t))

(defun ebib-quit (&optional force-quit)
  "Quit Ebib.
The Ebib buffers are killed, all variables except the keymaps are
set to nil.  If optional argument FORCE-QUIT is non-nil, do not
ask for confirmation."
  (interactive)
  ;; Kill any multiline buffers first. This will ask for confirmation if
  ;; any of them haven't been saved yet.
  (mapc #'kill-buffer ebib--multiline-buffer-list)
  (when (if (ebib--modified-p)
            (yes-or-no-p "There are modified databases.  Quit anyway? ")
          (or force-quit (y-or-n-p "Quit Ebib? ")))
    (if (and ebib-keywords
             (get 'ebib--keywords-completion-list :modified)
             (or (eq ebib-keywords-save-on-exit 'always)
                 (and (eq ebib-keywords-save-on-exit 'ask)
                      (y-or-n-p "Canonical keywords list changed.  Save? "))))
        (ebib--keywords-save-canonical-list))
    (ebib--filters-update-filters-file)
    (mapc (lambda (x)
            (kill-buffer (cdr x)))
          ebib--buffer-alist)
    (mapc (lambda (db)
            (let ((buf (ebib-db-get-buffer db)))
              (when buf (kill-buffer buf))))
          ebib--databases)
    (if (buffer-live-p (get-buffer ebib--empty-index-buffer-name))
        (kill-buffer (get-buffer ebib--empty-index-buffer-name)))
    (setq ebib--databases nil
          ebib--cur-db nil
          ebib--buffer-alist nil
          ebib--multiline-buffer-list nil
          ebib--initialized nil
          ebib--export-filename nil
          ebib--window-before nil
          ebib--buffer-before nil
          ebib--notes-list nil
          ebib--keywords-completion-list nil
          ebib--filters-alist nil
          ebib--filters-modified nil)
    (set-window-configuration ebib--saved-window-config)
    (remove-hook 'kill-emacs-query-functions 'ebib--kill-emacs-query-function)
    (remove-hook 'kill-buffer-query-functions 'ebib--kill-multiline-query-function)
    (message "")))

(defun ebib--kill-emacs-query-function ()
  "Function to run if Emacs is killed.
Ask if the user wants to save any modified databases and added
keywords before Emacs is killed."
  (when (or (not (ebib--modified-p))
            (if (y-or-n-p "[Ebib] Save all unsaved databases? ")
                (progn
                  (ebib-save-all-databases)
                  t)
              (yes-or-no-p "[Ebib] There are modified databases.  Kill anyway? ")))
    (when (and ebib-keywords
               (get 'ebib--keywords-completion-list :modified)
               (y-or-n-p "[Ebib] Save modified keywords list? "))
      (ebib--keywords-save-canonical-list))
    t))

(defun ebib--kill-multiline-query-function ()
  "Function to call when killing a multiline edit buffer."
  (if (and (with-no-warnings ; `ebib-multiline-mode' is not defined yet.
             ebib-multiline-mode)
           (buffer-modified-p))
      (yes-or-no-p (format "[Ebib] Multiline edit buffer `%s' not saved.  Quit anyway? " (buffer-name)))
    t))

;;; index-mode

(defvar ebib-index-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map 'no-digits)
    (define-key map [up] #'ebib-prev-entry)
    (define-key map [down] #'ebib-next-entry)
    (define-key map [right] #'ebib-next-database)
    (define-key map [left] #'ebib-prev-database)
    (define-key map [prior] #'ebib-index-scroll-down)
    (define-key map [next] #'ebib-index-scroll-up)
    (define-key map [home] #'ebib-goto-first-entry)
    (define-key map [end] #'ebib-goto-last-entry)
    (define-key map [return] #'ebib-select-and-popup-entry)
    (define-key map " " #'ebib-index-scroll-up)
    (define-key map "/" #'ebib-search)
    (define-key map "&" #'ebib-filters-logical-and)
    (define-key map "|" #'ebib-filters-logical-or)
    (define-key map "~" #'ebib-filters-logical-not)
    (define-key map "?" #'ebib-search-next)
    (define-key map "!" #'ebib-generate-autokey)
    (define-key map "<" #'ebib-index-sort-ascending)
    (define-key map ">" #'ebib-index-sort-descending)
    (define-key map "=" #'ebib-index-default-sort)
    (define-key map "a" #'ebib-add-entry)
    (define-key map "A" #'ebib-show-annotation)
    (define-key map "b" #'ebib-index-scroll-down)
    (define-key map "c" #'ebib-index-c)
    (define-key map "C" 'ebib-copy-map)
    (define-key map "d" #'ebib-delete-entry) ; prefix
    (define-key map "e" #'ebib-edit-entry)
    (define-key map "E" #'ebib-edit-keyname)
    (define-key map "f" #'ebib-view-file)
    (define-key map "F" 'ebib-filters-map)
    (define-key map "g" #'ebib-goto-first-entry)
    (define-key map "G" #'ebib-goto-last-entry)
    (define-key map "h" #'ebib-index-help)
    (define-key map "H" #'ebib-toggle-hidden)
    (define-key map "i" #'ebib-push-citation) ; prefix
    (define-key map "I" #'ebib-browse-doi)
    (define-key map "j" #'ebib-jump-to-entry)
    (define-key map "J" #'ebib-switch-to-database-nth)
    (define-key map "k" #'ebib-kill-entry)
    (define-key map "K" 'ebib-keywords-map)
    (define-key map "l" #'ebib-show-log)
    (define-key map "m" #'ebib-mark-entry) ; prefix
    (define-key map "M" 'ebib-dependent-map)
    (define-key map "n" #'ebib-next-entry)
    (define-key map "N" #'ebib-popup-note)
    (define-key map [(control n)] #'ebib-next-entry)
    (define-key map [(meta n)] #'ebib-index-scroll-up)
    (define-key map "o" #'ebib-open-bibtex-file)
    (define-key map "p" #'ebib-prev-entry)
    (define-key map [(control p)] #'ebib-prev-entry)
    (define-key map [(meta p)] #'ebib-index-scroll-down)
    (define-key map "P" #'ebib-edit-preamble)
    (define-key map "q" #'ebib-quit)
    (define-key map "r" #'ebib-reload-current-database)
    (define-key map "R" 'ebib-reading-list-map)
    (define-key map "s" #'ebib-save-current-database)
    (define-key map "S" #'ebib-edit-strings)
    (define-key map "u" #'ebib-browse-url)
    (define-key map "w" #'ebib-write-database)
    (define-key map "x" #'ebib-export-entries) ; prefix
    (define-key map "\C-xb" #'ebib-leave-ebib-windows)
    (define-key map "\C-xk" #'ebib-quit)
    (define-key map "\C-x\C-s" #'ebib-save-current-database)
    (define-key map "X" #'ebib-export-preamble)
    (define-key map "y" #'ebib-yank-entry)
    (define-key map "z" #'ebib-leave-ebib-windows)
    (define-key map "Z" #'ebib-lower)
    map)
  "Keymap for the ebib index buffer.")

(defun ebib-switch-to-database-key (n)
  "Switch to database N.
This function is meant to be bound to the keys 1-9.  N is a
character ?1-?9, which is converted to the corresponding number."
  (interactive (list last-command-event))
  (ebib-switch-to-database-nth (- n 48)))

(mapc (lambda (key)
        (define-key ebib-index-mode-map (format "%d" key)
          'ebib-switch-to-database-key))
      '(1 2 3 4 5 6 7 8 9))

(defun ebib-set-default-dir ()
  "Set the default directory of the current buffer.
The default directory is set according to `ebib-default-directory', see
there for details."
  (cond
   ((eq ebib-default-directory 'first-bib-dir)
    (setq default-directory (car ebib-bib-search-dirs)))
   ((stringp ebib-default-directory) ; No check is made if `ebib-default-directory' really is a directory and whether it exists.
    (setq default-directory ebib-default-directory))
   (t nil))) ; Leave the default directory as is.

(define-derived-mode ebib-index-mode
  fundamental-mode "Ebib-index"
  "Major mode for the Ebib index buffer."
  (setq buffer-read-only t)
  (if ebib-hide-cursor
      (setq cursor-type nil))
  (if ebib-index-mode-line
      (setq mode-line-format ebib-index-mode-line))
  (setq truncate-lines t)
  (ebib-set-default-dir)
  (setq ebib--dirty-index-buffer nil)
  (set (make-local-variable 'hl-line-face) 'ebib-highlight-extend-face)
  (hl-line-mode 1))

(easy-menu-define ebib-index-menu ebib-index-mode-map "Ebib index menu."
  `("Ebib"
    ("Database"
     ["Open..." ebib-open-bibtex-file t]
     ["Close" ebib-index-c :active (and ebib--cur-db (not (ebib-db-filtered-p ebib--cur-db))) :keys "\\[ebib-index-c]"]
     ["Merge..." ebib-merge-bibtex-file (and ebib--cur-db (not (ebib-db-get-filter ebib--cur-db)))]
     ["Reload" ebib-reload-current-database ebib--cur-db]
     ["Reload All" ebib-reload-all-databases ebib--cur-db]
     ["Save" ebib-save-current-database (and ebib--cur-db
                                             (ebib-db-modified-p ebib--cur-db))]
     ["Save All" ebib-save-all-databases (ebib--modified-p)]
     ["Save As..." ebib-write-database ebib--cur-db])

    ("Main/Dependent"
     ["Create Dependent" ebib-dependent-create-dependent (and ebib--cur-db (not (ebib-db-dependent-p ebib--cur-db)) (not (ebib-db-filtered-p ebib--cur-db)))]
     ["Add Entry To Dependent" ebib-dependent-add-entry (and ebib--cur-db (ebib-db-has-entries ebib--cur-db))]
     ["Remove Entry From Dependent" ebib-dependent-delete-entry (and ebib--cur-db (ebib-db-dependent-p ebib--cur-db) (ebib-db-has-entries ebib--cur-db))]
     ["Switch To Main" ebib-dependent-switch-to-main (and ebib--cur-db (ebib-db-dependent-p ebib--cur-db))])

    ("Entry"
     ["Add" ebib-add-entry (and ebib--cur-db (not (ebib-db-get-filter ebib--cur-db)))]
     ["Edit" ebib-edit-entry (ebib--get-key-at-point)]
     ["Kill" ebib-kill-entry (ebib--get-key-at-point)]
     ["Yank From Kill Ring" ebib-yank-entry ebib--cur-db]
     ["Fetch By DOI" ebib-biblio-import-doi :visible (fboundp 'ebib-biblio-import-doi)]
     ["Delete" ebib-delete-entry (and ebib--cur-db
                                      (ebib--get-key-at-point)
                                      (not (ebib-db-get-filter ebib--cur-db)))]
     "--"
     ["Jump to Entry" ebib-jump-to-entry (and ebib--cur-db (ebib--get-key-at-point))]
     ["Follow Cross-Reference" ebib-follow-crossref (and ebib--cur-db (ebib--get-key-at-point))]
     "--"
     ["Push Citation To Buffer" ebib-push-citation (ebib--get-key-at-point)]
     "--"
     ["Mark" ebib-mark-entry (ebib--get-key-at-point)]
     ["Mark/Unmark All" ebib-mark-all-entries :active (ebib--get-key-at-point) :keys "\\[universal-argument] \\[ebib-mark-entry]"]
     "--"
     ["Edit Key" ebib-edit-keyname (ebib--get-key-at-point)]
     ["Autogenerate Key" ebib-generate-autokey (ebib--get-key-at-point)]
     "--"
     ["Open Note" ebib-popup-note (ebib--get-key-at-point)]
     ["Show Annotation" ebib-show-annotation (ebib--get-key-at-point)]
     ["Follow Crossref" ebib-follow-crossref (ebib-db-get-field-value "crossref" (ebib--get-key-at-point) ebib--cur-db 'noerror)])

    ["Edit Strings" ebib-edit-strings (and ebib--cur-db (not (ebib-db-get-filter ebib--cur-db)))]
    ["Edit Preamble" ebib-edit-preamble (and ebib--cur-db (not (ebib-db-get-filter ebib--cur-db)))]
    ["Search" ebib-search (ebib--get-key-at-point)]

    ("View"
     ["Sort Ascending" ebib-index-sort-ascending ebib--cur-db]
     ["Sort Descending" ebib-index-sort-descending ebib--cur-db]
     ["Default Sort" ebib-index-default-sort ebib--cur-db])

    ("Copy"
     ["Key" ebib-copy-key-as-kill (ebib--get-key-at-point)]
     ["Entry" ebib-copy-entry-as-kill (ebib--get-key-at-point)]
     ["Citation" ebib-copy-citation-as-kill (ebib--get-key-at-point)]
     ["Reference" ebib-copy-reference-as-kill (ebib--get-key-at-point)])

    ("Export"
     ["Export Entries To Database" ebib-export-entries (ebib--get-key-at-point)]
     ["Export Entries To File" (ebib-export-entries t) :active (ebib--get-key-at-point) :keys "\\[universal-argument] \\[ebib-export-entries]"]
     ["Export Preamble To Database" ebib-export-preamble (ebib-db-get-preamble ebib--cur-db)]
     ["Export Preamble To File" (ebib-export-preamble t) :active (ebib-db-get-preamble ebib--cur-db) :keys "\\[universal-argument] \\[ebib-export-preamble]"])
    "--"

    ("Attachments"
     ["View File" ebib-view-file (and ebib--cur-db (ebib-get-field-value "file" (ebib--get-key-at-point) ebib--cur-db 'noerror))]
     ["Import Local File" ebib-import-file (ebib--get-key-at-point)]
     ["Import File From URL" ebib-download-url (ebib--get-key-at-point)]
     ["Create Entries For Local Files" ebib-add-file-entry ebib--cur-db])
    ("Links"
     ["Open URL" ebib-browse-url (and ebib--cur-db (ebib-get-field-value "url" (ebib--get-key-at-point) ebib--cur-db 'noerror))]
     ["Open DOI" ebib-browse-doi (and ebib--cur-db (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror))])
    "--"

    ("Filters"
     ["Create Filter" ebib-filters-logical-and (ebib--get-key-at-point)]
     ["Rerun Current Filter" ebib-filters-reapply-filter (ebib-db-filtered-p)]
     ["Apply Saved Filter" ebib-filters-apply-filter (ebib--get-key-at-point)]
     ["Reapply Last Filter" ebib-filters-reapply-last-filter (ebib--get-key-at-point)]
     ["Cancel Current Filter" ebib-filters-cancel-filter (ebib-db-filtered-p ebib--cur-db)]
     "--"
     ["Store Current Filter" ebib-filters-store-filter (or (ebib-db-get-filter ebib--cur-db) ebib--filters-last-filter)]
     ["Delete Filter" ebib-filters-delete-filter ebib--filters-alist]
     ["Delete All Filters" ebib-filters-delete-all-filters ebib--filters-alist]
     ["Rename Filter" ebib-filters-rename-filter ebib--filters-alist]
     "--"
     ["Save Filters To Filters File" ebib-filters-save-filters ebib--filters-modified]
     ["Save Filters To Alternate File" ebib-filters-write-to-file ebib--filters-alist]
     ["Load Filters From File" ebib-filters-load-from-file ebib--cur-db]
     "--"
     ["View Filters" ebib-filters-view-all-filters ebib--filters-alist])

    ("Reading List"
     ["Create Reading List" ebib-create-reading-list (not ebib-reading-list-file)]
     ["Add Current Entry" ebib-add-reading-list-item (and ebib-reading-list-file (ebib--get-key-at-point))]
     ["Mark Current Entry As Done" ebib-mark-reading-list-item-as-done (ebib--reading-list-item-p (ebib--get-key-at-point))]
     ["View Reading List" ebib-view-reading-list ebib-reading-list-file])

    ("Keywords"
     ["Add Keywords To Current Entry" ebib-add-keywords-to-entry (ebib--get-key-at-point)]
     ["Add Keyword To Canonical List" ebib-add-canonical-keyword t]
     ["Add All Keywords To Canonical List" ebib-add-all-keywords-to-canonical-list t]
     ["Purge Keywords Field" ebib-purge-keywords-field t]
     ["Save New Keywords" ebib-save-canonical-keywords-list (get 'ebib--keywords-completion-list :modified)])

    ("Print Entries"
     ["As Bibliography" ebib-latex-entries (and ebib--cur-db (not (ebib-db-get-filter ebib--cur-db)))]
     ["As Index Cards" ebib-print-entries ebib--cur-db]
     ["Print Multiline Fields" ebib-toggle-print-multiline :enable t
      :style toggle :selected ebib-print-multiline]
     ["Print Cards on Separate Pages" ebib-toggle-print-newpage :enable t
      :style toggle :selected ebib-print-newpage])
    "--"

    ("Options"
     ,(append (list "BibTeX Dialect")
              (mapcar (lambda (d)
                        (vector (format "%s" d) `(ebib-set-dialect (quote ,d))
                                :active 'ebib--cur-db
                                :style 'radio
                                :selected `(and ebib--cur-db
                                                (eq (ebib-db-get-dialect ebib--cur-db) (quote ,d)))))
                      bibtex-dialect-list)
              (list ["Default" (ebib-set-dialect nil)
                     :active ebib--cur-db :style radio :selected (and ebib--cur-db (not (ebib-db-get-dialect ebib--cur-db)))]))
     ["Show Hidden Fields" ebib-toggle-hidden :enable t
      :style toggle :selected (not ebib--hide-hidden-fields)]
     ["Use Timestamp" ebib-toggle-timestamp :enable t
      :style toggle :selected ebib-use-timestamp]
     ["Save Cross-Referenced Entries First" ebib-toggle-xrefs-first :enable t
      :style toggle :selected ebib-save-xrefs-first]
     ["Allow Identical Fields" ebib-toggle-identical-fields :enable t
      :style toggle :selected ebib-allow-identical-fields]
     ["Full Layout" ebib-toggle-layout :enable t
      :style toggle :selected (eq ebib-layout 'full)]
     ["Customize Ebib" ebib-customize t])
    ["View Log Buffer" ebib-show-log t]
    ["Lower Ebib" ebib-lower t]
    ["Quit Ebib" ebib-quit t]
    ["Help on Ebib" ebib-info t]))

(defun ebib-customize ()
  "Switch to Ebib's customisation group."
  (interactive)
  (ebib-lower)
  (customize-group 'ebib))

(defun ebib-open-bibtex-file (&optional file)
  "Open the BibTeX file FILE.
Make the new file the current database and update the buffers.

This function is for interactive use only.  To load a BibTeX file
in the background, use `ebib--load-bibtex-file-internal'."
  (interactive)
  (unless file
    (setq file (ebib--ensure-extension (expand-file-name (read-file-name "File to open: ")) (car ebib-bibtex-extensions))))
  (setq ebib--cur-db (ebib--load-bibtex-file-internal file))
  (ebib--update-buffers))

(defun ebib--load-bibtex-file-internal (file)
  "Load BibTeX file FILE.
FILE must be a fully expanded filename.  The new database is
returned, but it is not made current and the buffers are not
updated.  If FILE is already open, just return the database.

This function can be used to open a BibTeX file in the
background.  Use `ebib-open-bibtex-file' to open a BibTeX file
interactively."
  (or (ebib--get-db-from-filename file) ; FILE is already open in Ebib.
      (let ((new-db (ebib--create-new-database))
            (ebib--log-error nil)) ; We haven't found any errors yet.
        (ebib-db-set-filename file new-db)
        (ebib--log 'log "Opening file `%s'" file)
        (if (file-exists-p file)
            (progn
              (ebib--bib-read-entries file new-db)
              (ebib-db-set-backup t new-db)
              (ebib-db-set-modified nil new-db))
          ;; If the file does not exist, we need to issue a message.
          (ebib--log 'message "(New file)"))
        new-db)))

(defun ebib-reload-current-database ()
  "Reload the current database from disk."
  (interactive)
  (ebib--execute-when
    (entries
     (when (or (and (ebib-db-modified-p ebib--cur-db)
                    (yes-or-no-p "Database modified.  Really reload from file? "))
               (y-or-n-p "Reload current database from file? "))
       (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
       (ebib--reload-database ebib--cur-db)
       (ebib--set-modified nil ebib--cur-db)
       (ebib--update-buffers)
       (message "Database reloaded")))
    (default
      (beep))))

(defun ebib-reload-all-databases ()
  "Reload all databases from disk."
  (interactive)
  (when (y-or-n-p "Reload all databases from file? ")
    (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
    (dolist (db ebib--databases)
      (when (or (not (ebib-db-modified-p db))
                (yes-or-no-p (format "Database `%s' modified.  Really reload from file? " (ebib-db-get-filename db))))
        (ebib--reload-database db)
        (ebib--set-modified nil db)))
    (ebib--update-buffers)))

(defun ebib--reload-database (db)
  "Reload database DB from disk."
  (let ((file (ebib-db-get-filename db))
        (cur-key (ebib--db-get-current-entry-key db)))
    ;; First clear out the database.  Note that this does not destroy DB's
    ;; index buffer, which is good, because we want to re-use it.
    (let ((buffer (ebib-db-get-buffer db)))
      (ebib-db-clear-database db)
      (ebib-db-set-buffer buffer db))
    ;; Then load the file.
    (ebib--log 'log "Reloading file `%s'" file)
    (ebib-db-set-filename file db)
    (ebib--bib-read-entries file db)
    ;; If the user makes any changes, we'll want to create a back-up.
    (ebib-db-set-backup t ebib--cur-db)
    (ebib-db-set-current-entry-key cur-key db)))

(defun ebib-merge-bibtex-file ()
  "Merge a BibTeX file into the current database."
  (interactive)
  (ebib--execute-when
    ((or dependent-db filtered-db) (error "[Ebib] Cannot merge into a filtered or a dependent database"))
    (real-db
     (let ((file (expand-file-name (read-file-name "File to merge: ")))
           (ebib--log-error nil))       ; We haven't found any errors yet.
       (if (not (file-readable-p file))
           (error "[Ebib] No such file: %s" file)
         (ebib--log 'log "Merging file `%s'" (ebib-db-get-filename ebib--cur-db))
         (ebib--bib-read-entries file ebib--cur-db 'ignore-modtime 'not-as-dependent)
         (ebib--update-buffers)
         (ebib--set-modified t ebib--cur-db))))
    (default (beep))))

(defun ebib--bib-read-entries (file db &optional ignore-modtime not-as-dependent)
  "Load BibTeX entries from FILE into DB.
If FILE specifies a BibTeX dialect and no dialect is set for DB,
also set DB's dialect.  FILE's modification time is stored in DB,
unless IGNORE-MODTIME is non-nil.  If NOT-AS-DEPENDENT is
non-nil, load FILE as a normal database, even if it is a
dependent database."
  (with-temp-buffer
    (insert-file-contents file)
    (unless ignore-modtime
      (ebib-db-set-modtime (ebib--get-file-modtime file) db))
    (if (and (not not-as-dependent)
             (ebib--bib-find-main db))
        (let ((result (ebib--bib-find-bibtex-entries db nil)))
          (ebib--log 'message "Loaded %d entries into dependent database." (car result)))
      ;; Opening a non-dependent database.
      (unless (ebib-db-get-dialect db)
        (ebib-db-set-dialect (parsebib-find-bibtex-dialect) db))
      (let ((result (ebib--bib-find-bibtex-entries db nil)))
        (ebib--log 'message "%d entries, %d @Strings and %s @Preamble found in file."
                   (car result)
                   (cadr result)
                   (if (nth 2 result) "a" "no"))))
    (when ebib--log-error
      (message "%s found! Press `l' to check Ebib log buffer." (nth ebib--log-error '("Warnings" "Errors"))))))

(defun ebib--bib-find-main (db)
  "Find the \"main database\" declaration in the current buffer.
If a main database is found, make sure it is loaded, set it as
DB's main and return it.  If no main database is found, return
nil.  If a main database is found but it cannot be opened, log an
error and return nil."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (re-search-forward "@Comment{\n[[:space:]]*ebib-\\(?:main\\|master\\)-file: \\(.*\\)\n}" nil t)
          (let* ((main-file (match-string 1))
                 (main (ebib--get-or-open-db main-file)))
            (if main
                (ebib-db-set-main main db)
              (ebib--log 'error "Could not find main database `%s'" main-file))))))) ; This returns nil.

(defun ebib--bib-find-bibtex-entries (db timestamp)
  "Find the BibTeX entries in the current buffer.
The search is started at the beginnig of the buffer.  All entries
found are stored in DB.  Return value is a three-element list: the
first element is the number of entries found, the second the
number of @String definitions, and the third is t or nil,
indicating whether a @Preamble was found.

TIMESTAMP indicates whether a timestamp is to be added to each
entry.  Note that a timestamp is only added if `ebib-use-timestamp'
is set to t."
  (let ((n-entries 0)
        (n-strings 0)
        (preamble nil)
        (entry-list (ebib--list-entry-types (ebib--get-dialect db))))
    (goto-char (point-min))
    (cl-loop for entry-type = (ebib--bib-find-next-bibtex-item)
             while entry-type do
             (cond
              ((cl-equalp entry-type "string") ; `cl-equalp' compares strings case-insensitively.
               (if (ebib--bib-read-string db)
                   (setq n-strings (1+ n-strings))))
              ((cl-equalp entry-type "preamble")
               (when (ebib--bib-read-preamble db)
                 (setq preamble t)))
              ((cl-equalp entry-type "comment")
               (ebib--bib-read-comment db))
              ((stringp entry-type)
               (when (ebib--bib-read-entry entry-type db timestamp)
                 (setq n-entries (1+ n-entries))
                 (unless (assoc-string entry-type entry-list 'case-fold)
                   (ebib--log 'warning "Line %d: Unknown entry type `%s'." (line-number-at-pos) entry-type))))))
    (list n-entries n-strings preamble)))

(defun ebib--bib-find-next-bibtex-item ()
  "Search for the next BibTeX item in the current buffer.
A BibTeX item is an entry, or a @Preamble, @String or @Comment
definition.  If an item is found, point is placed right after it
and the entry type is returned.  If no item is found, point is
left at the end of the buffer and nil is returned.  If something
is found that appears to be an entry (essentially, an `@' at the
start of a line), but does not consist of a valid BibTeX
identifier, an error is logged and t is returned."
  (condition-case err
      (parsebib-find-next-item)
    (parsebib-entry-type-error (ebib--log 'error "Error: illegal entry type at line %d. Skipping" (line-number-at-pos (cadr err)))
                               t))) ; Return t so that searching continues in ebib--bib-find-bibtex-entries.

(defun ebib--bib-read-comment (db)
  "Read an @Comment entry and store it in DB.
If the @Comment is a local variable list, store it as such in DB.
If the @Comment defines a main database, do not store the
comment."
  (let ((comment (parsebib-read-comment)))
    (when comment
      (or
       ;; Local variables.
       (let ((lvars (ebib--local-vars-to-list comment)))
         (if lvars
             (ebib-db-set-local-vars lvars db)))
       ;; Main file: do nothing (the main file was dealt with in `ebib--bib-read-entries'.
       (string-match-p "^[[:space:]]*ebib-\\(?:main\\|master\\)-file: \\(.*\\)$" comment)
       ;; Otherwise, store the comment.
       (ebib-db-set-comment comment db)))))

(defun ebib--bib-read-string (db)
  "Read an @String definition and store it in DB.
Return value is the string if one was read, nil otherwise."
  (unless (ebib-db-dependent-p db) ; @Strings are not stored in dependent files.
    (let* ((def (parsebib-read-string))
           (abbr (car def))
           (string (cdr def)))
      (if def
          (if (ebib-set-string abbr string db nil 'as-is)
              string
            (ebib--log 'warning (format "Line %d: @String definition `%s' duplicated. Skipping."
                                        (line-number-at-pos) abbr)))
        (ebib--log 'error "Error: illegal string identifier at line %d. Skipping" (line-number-at-pos))))))

(defun ebib--bib-read-preamble (db)
  "Read a @Preamble definition and store it in DB.
If there was already another @Preamble definition, the new one is
added to the existing one with a hash sign `#' between them."
  (unless (ebib-db-dependent-p db) ; @Preamble is not stored in dependent files.
    (let ((preamble (substring (parsebib-read-preamble) 1 -1))) ; We need to remove the braces around the text.
      (if preamble
          (ebib-db-set-preamble preamble db 'append)))))

(defun ebib--bib-read-entry (entry-type db &optional timestamp)
  "Read a BibTeX entry with type ENTRY-TYPE and store it in DB.
Return the entry key if an entry was found and could be stored,
nil otherwise.  Optional argument TIMESTAMP indicates whether a
timestamp is to be added.  (Whether a timestamp is actually added
also depends on `ebib-use-timestamp'.)"
  (let* ((beg (point)) ; Save the start of the entry in case something goes wrong.
         (entry (parsebib-read-entry entry-type)))
    (if entry
        (let ((entry-key (cdr (assoc-string "=key=" entry))))
          (if (ebib-db-dependent-p db)
              (if (ebib-db-has-key entry-key (ebib-db-get-main db))
                  (ebib-db-add-entries-to-dependent entry-key db)
                (ebib--log 'error "Entry key `%s' not found in main database" entry-key))
            (when (string= entry-key "")
              (setq entry-key (ebib--generate-tempkey db))
              (ebib--log 'warning "Line %d: Temporary key generated for entry." (line-number-at-pos beg)))
            (setq entry-key (ebib--store-entry entry-key entry db timestamp (if ebib-uniquify-keys 'uniquify 'noerror)))
            (when (and entry-key (not ebib-keywords))
              (if-let ((keywords (ebib-unbrace (cdr (assoc-string "keywords" entry 'case-fold)))))
                  (mapc #'ebib--keywords-add-to-completion-list (ebib--keywords-to-list keywords))))
            (unless entry-key
              (ebib--log 'warning "Line %d: Entry `%s' duplicated. Skipping." (line-number-at-pos beg) entry-key))
            entry-key)) ; Return the entry key, or nil if no entry could be stored.
      (ebib--log 'warning "Line %d: Could not read a valid entry." (line-number-at-pos beg))
      nil))) ; Make sure to return nil upon failure.

(defun ebib-leave-ebib-windows ()
  "Leave the Ebib windows, lowering them if necessary."
  (interactive)
  (ebib-lower t))

(defun ebib-lower (&optional soft)
  "Hide the Ebib windows.
If optional argument SOFT is non-nil, just switch to a non-Ebib
buffer if Ebib is not occupying the entire frame."
  (interactive)
  (if ebib--cur-db
      (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db))
  (unless (member (window-buffer) (mapcar #'cdr ebib--buffer-alist))
    (error "Ebib is not active "))
  (cond
   ((and soft (eq ebib-layout 'custom))
    (select-window ebib--window-before))
   ((and soft (eq ebib-layout 'index-only))
    (other-window 1)
    (if (member (current-buffer) (mapcar #'cdr ebib--buffer-alist))
        (switch-to-buffer nil)))
   ((and ebib--frame-before
         (not (eq (window-frame) ebib--frame-before)))
    (if (frame-live-p ebib--frame-before)
        (select-frame-set-input-focus ebib--frame-before)
      (setq ebib--frame-before nil)
      (select-window (get-buffer-window (ebib--buffer 'index))) ; Just to be sure.
      (delete-other-windows)
      (set-window-dedicated-p (selected-window) nil)
      (switch-to-buffer nil)))
   (t (set-window-configuration ebib--saved-window-config)))
  (mapc (lambda (buffer)
          (bury-buffer buffer))
        (mapcar #'cdr ebib--buffer-alist)))

(defun ebib-prev-entry ()
  "Move to the previous BibTeX entry."
  (interactive)
  (ebib--execute-when
    (entries
     (if (bobp)                         ; If we're on the first entry,
         (beep)                         ; just beep.
       (forward-line -1)
       (ebib--update-entry-buffer)))
    (default
      (beep))))

(defun ebib-next-entry (&optional pfx)
  "Move to the next BibTeX entry.
The argument PFX is used to determine if the command was called
interactively."
  (interactive "p")
  (ebib--execute-when
    (entries
     (forward-line 1)
     (if (eobp)
         (progn
           (forward-line -1)
           (if pfx (beep)))
       (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
       (ebib--update-entry-buffer)))
    (default
      (beep))))

(defun ebib-show-annotation ()
  "Show the contents of the `annote' or `annotation' field in a *Help* window."
  (interactive)
  (let ((help-window-select t) ; Make sure the help window is selected.
        (field (cdr (assq (ebib--get-dialect ebib--cur-db) '((BibTeX . "annote") (biblatex . "annotation"))))))
    (with-help-window (help-buffer)
      (princ (propertize (format "Annotation for `%s' [%s]" (ebib--get-key-at-point) (ebib-db-get-filename ebib--cur-db 'shortened)) 'face '(:weight bold)))
      (princ "\n\n")
      (let ((contents (ebib-get-field-value field (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced)))
        (if contents
            (princ contents)
          (princ "[No annotation]"))))))

(defun ebib-open-note (key)
  "Open the note for KEY and return its buffer.
If `ebib-notes-storage' is set to `multiple-notes-per-file', this
function runs `ebib-notes-open-note-after-hook'."
  (let ((buf (ebib--notes-goto-note key)))
    (when buf
      (with-current-buffer (car buf)
        (goto-char (cdr buf))
        (if (eq ebib-notes-storage 'multiple-notes-per-file)
            (run-hooks 'ebib-notes-open-note-after-hook)))
      (car buf))))

(defun ebib-popup-note (key)
  "Open the note for KEY or create a new one if none exists.
KEY defaults to the current entry's key.  Display and select the
buffer containing the entry using `pop-to-buffer'.

If `ebib-notes-storage' is set to `multiple-notes-per-file', this
function runs `ebib-notes-open-note-after-hook' for an existing
note or `ebib-notes-new-note-hook' for a new note."
  (interactive (list (ebib--get-key-at-point)))
  (ebib--execute-when
    (entries
     (let ((buf (ebib--notes-goto-note key))
           (hook 'ebib-notes-open-note-after-hook))
       (when (not buf) ; We need to create a new note.
         (setq buf (ebib--notes-create-new-note key ebib--cur-db)
               hook 'ebib-notes-new-note-hook))
       ;; If `ebib-notes-use-org-capture' is non-nil, we don't need to pop up
       ;; the buffer.  `ebib--notes-create-new-note' returns nil in this case,
       ;; so we can simply check the value of `buf':
       (when buf
         (with-current-buffer (car buf)
           (goto-char (cdr buf))
           (when (eq ebib-notes-storage 'multiple-notes-per-file)
             (run-hooks hook)))
         (pop-to-buffer (car buf)))))
    (default
      (beep))))

(defun ebib-org-capture (&optional goto keys)
  "Call `org-capture' from within Ebib.
GOTO and KEYS are passed on to `org-capture'.  This function sets
`ebib--org-current-key' to the current key before calling
`org-capture', so that `ebib-notes-create-org-template' can
function properly."
  (interactive "P")
  (ebib--execute-when
    (entries
     (let ((ebib--org-current-key (ebib--get-key-at-point)))
       (org-capture goto keys)))
    (default
      (beep))))

(defun ebib--add-entry-stub (&optional entry db)
  "Add ENTRY to DB in the form of a stub.
Return the database key of the created entry.  ENTRY is an
optional alist consisting of (FIELD . VALUE) pairs.  The alist is
converted into a BibTeX entry stub and added to DB, which
defaults to the current database.  If an entry alist doesn't
contain the `=type=' field, the entry type is set to the value of
`ebib-default-entry-type'.  If it doesn't contain a `=key='
field, a key is created of the form \"<new-entry%d>\", where %d
is replaced with a number in ascending sequence."
  (unless db
    (setq db ebib--cur-db))
  (let ((fields ())
        entry-key)
    (cl-dolist (props entry)
      ;; Aggregate properties, some require special handling.
      (cond
       ((string= (car props) "=key=")
        (setq entry-key (cdr props)))
       ((string= (car props) "=type=")   ; The =type= field should not be braced.
        (push props fields))
       ((cl-equalp (car props) "file")
        (let ((short-file (ebib--file-relative-name (expand-file-name (cdr props)))))
          (push (cons "file" (ebib-brace short-file)) fields)))
       (t
        (push (cons (car props) (ebib-brace (cdr props))) fields))))
    ;; Check for required.
    (unless entry-key
      (setq entry-key (ebib--generate-tempkey db)))
    (unless (assoc "=type=" fields)
      (push (cons "=type=" ebib-default-entry-type) fields))
    ;; Insert.
    (ebib--store-entry entry-key fields db t ebib-uniquify-keys)
    (ebib--set-modified t ebib--cur-db t)
    entry-key))

(defun ebib-add-entry ()
  "Interactively add a new entry to the database."
  (interactive)
  (ebib--execute-when
    (dependent-db (ebib-dependent-add-entry))
    (real-db
     (let ((entry-alist (list)))
       (unless ebib-autogenerate-keys
         (push (cons '=key= (read-string "New entry key: " nil 'ebib--key-history)) entry-alist))
       (let ((new-key (ebib--add-entry-stub entry-alist ebib--cur-db)))
         (ebib-db-set-current-entry-key new-key ebib--cur-db)
         (ebib--insert-entry-in-index-sorted new-key t)
         (ebib--update-entry-buffer))
       (ebib--edit-entry-internal)))
    (no-database
     (error "[Ebib] No database open.  Use `o' to open a database"))
    (default
      (beep))))

(defun ebib-add-file-entry (&optional filepath db disable-prompt allow-duplicates)
  "Add an entry stub for an optional FILEPATH to DB.
If FILEPATH is a list, add entries for each file contained
within.  If FILEPATH is a directory, add entries for all its
contents.  And if FILEPATH is not given, prompt the user to
browse in the minibuffer, unless DISABLE-PROMPT is T.  If a
FILEPATH is already referenced by an entry in the DB, then it is
ignored by default, unless ALLOW-DUPLICATES is true, in which
case add new entry stubs for each file anyway."
  (interactive)
  (or db
      (setq db ebib--cur-db))
  (let (all-entry-files)
    (cl-labels
        ((file-exists-in-db-p (fp)
                              (if (member (locate-file fp ebib-file-search-dirs) all-entry-files)
                                  t))
         (add-file-entry (fp)
                         (cond
                          ((listp fp)
                           (cl-dolist (file fp) (add-file-entry file)))
                          ((file-directory-p fp)
                           (add-file-entry (directory-files fp t "^\\([^.]\\)"))) ; Ignore hidden.
                          ((file-exists-p fp)
                           (if (and (null allow-duplicates) (file-exists-in-db-p fp))
                               (message "File %s already exists in db, skipping" fp)
                             (ebib--add-entry-stub (list (cons "file" fp)) db)
                             (message "Adding file %s" fp)))
                          (t
                           (error "[Ebib] Invalid file %s" fp)))))
      ;; Prompt for file.
      (if (and (null filepath) (null disable-prompt))
          (setq filepath (read-file-name "Add file or directory: ")))
      ;; Collect all file paths from db entries into single list.
      (unless allow-duplicates
        (cl-dolist (entry-key (ebib-db-list-keys db))
          (let ((entry-files (ebib-get-field-value "file" entry-key db 'noerror 'unbraced)))
            (if entry-files
                (cl-dolist (fp (split-string entry-files (regexp-quote ebib-filename-separator)))
                  (push (locate-file fp ebib-file-search-dirs) all-entry-files))))))
      (add-file-entry filepath)
      (ebib-db-set-current-entry-key nil ebib--cur-db)
      (ebib--update-buffers))))

(defun ebib-generate-autokey ()
  "Automatically generate a key for the current entry.
This function uses the function BIBTEX-GENERATE-AUTOKEY to
generate the key, see that function's documentation for details."
  (interactive)
  (ebib--execute-when
    ((and real-db entries)
     (let ((new-key
            (with-temp-buffer
              ;; We sort the entry fields when formatting, because if both
              ;; `author' and `editor' fields are present,
              ;; `bibtex-generate-autokey' will simply use the first one it
              ;; finds.  By sorting we make sure it's always the author.
              (ebib--format-entry (ebib--get-key-at-point) ebib--cur-db nil 'sort)
              (let ((x-ref (or (ebib-get-field-value "xdata" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced)
			       (ebib-get-field-value "crossref" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced))))
                (if x-ref
                    (ebib--format-entry x-ref ebib--cur-db nil 'sort)))
              (goto-char (point-min))
              (bibtex-set-dialect (ebib--get-dialect ebib--cur-db) 'local)
              (bibtex-generate-autokey))))
       (if (string= new-key "")
           (error (format "[Ebib] Cannot create key"))
         (ebib--update-keyname new-key))))
    (default
      (beep))))

(defun ebib--generate-tempkey (&optional db)
  "Generate a unique temp key in DB or the current database.
Keys are in the form: <new-entry1>, <new-entry2>, ..."
  (unless db
    (setq db ebib--cur-db))
  (let ((key-list (ebib-db-list-keys db))
        (entry-key "<new-entry1>")
        (key-count 2))
    (while (member entry-key key-list)
      (setq entry-key (format "<new-entry%d>" key-count))
      (setq key-count (1+ key-count)))
    entry-key))

(defun ebib-index-c ()
  "Helper function for the `c' key in the index buffer."
  (interactive)
  (if (ebib-db-filtered-p ebib--cur-db)
      (ebib-filters-cancel-filter)
    (ebib-close-database)))

(defun ebib-close-database ()
  "Close the current BibTeX database."
  (interactive)
  (ebib--execute-when
    (database
     (catch 'return
       (unless (if (ebib-db-modified-p ebib--cur-db)
                   (yes-or-no-p "Database modified.  Close it anyway? ")
                 (y-or-n-p "Close database? "))
         (throw 'return nil))

       ;; Kill associated multiline edit buffers.  This asks for confirmation if
       ;; there are unsaved modifications.
       (mapc (lambda (buffer)
               (unless (kill-buffer buffer)
                 (throw 'return nil)))
             (seq-filter (lambda (elt)
                           (string= (ebib-db-get-filename ebib--cur-db)
                                    (cl-second (buffer-local-value 'ebib--multiline-info elt))))
                         ebib--multiline-buffer-list))

       ;; Remove the database from `ebib--databases', redisplay and kill the index buffer.
       (let ((new-db (cadr (member ebib--cur-db ebib--databases)))
             (index-buffer (ebib-db-get-buffer ebib--cur-db)))
         (setq ebib--databases (delq ebib--cur-db ebib--databases))
         ;; If `new-db' is nil, we deleted the last database in the list.
         (setq ebib--cur-db (or new-db
                                (car ebib--databases)))
         ;; `ebib--cur-db' may be nil at this point, but that's handled correctly by `ebib--update-buffers'.
         (ebib--update-buffers 'no-refresh)
         (kill-buffer index-buffer)
         (message "Database closed."))))))

(defun ebib-index-sort-ascending (field)
  "Sort the entries in the index buffer in ascending order.
Sort key is FIELD, which must be one of the fields specified in
`ebib-index-columns'."
  (interactive (list (completing-read "Sort field (ascending): " (mapcar #'car
                                                                         (seq-filter (lambda (elt)
                                                                                       (nth 2 elt))
                                                                                     ebib-index-columns))
                                      nil t nil 'ebib--field-history)))
  (ebib--index-sort field 'ascend))

(defun ebib-index-sort-descending (field)
  "Sort the entries in the index buffer in descending order.
Sort key is FIELD, which must be one of the fields specified in
`ebib-index-columns'."
  (interactive (list (completing-read "Sort field (descending): " (mapcar #'car
                                                                          (seq-filter (lambda (elt)
                                                                                        (nth 2 elt))
                                                                                      ebib-index-columns))
                                      nil t nil 'ebib--field-history)))
  (ebib--index-sort field 'descend))

(defun ebib--index-sort (field order)
  "Sort the entries in the index buffer according to FIELD.
ORDER indicates the sort order and should be either `ascend' or
`descend'."
  (unless (string= field "")
    (ebib-db-set-sortinfo (cons field order) ebib--cur-db)
    (ebib--update-buffers)))

(defun ebib-index-default-sort ()
  "Sort the index buffer on the first column."
  (interactive)
  (ebib-db-set-sortinfo nil ebib--cur-db)
  (ebib--update-buffers))

(defun ebib-goto-first-entry ()
  "Move to the first BibTeX entry in the database."
  (interactive)
  (ebib--execute-when
    (entries
     (with-current-ebib-buffer 'index
       (goto-char (point-min))
       (ebib--update-entry-buffer)))
    (default
      (beep))))

(defun ebib-goto-last-entry ()
  "Move to the last entry in the BibTeX database."
  (interactive)
  (ebib--execute-when
    (entries
     (with-current-ebib-buffer 'index
       (goto-char (point-max))
       (forward-line -1)
       (ebib--update-entry-buffer)))
    (default
      (beep))))

(defun ebib-edit-entry ()
  "Edit the current BibTeX entry."
  (interactive)
  (ebib--execute-when
    (entries
     (ebib--edit-entry-internal))
    (default
      (beep))))

(defun ebib--edit-entry-internal ()
  "Helper function for `ebib-edit-entry'."
  (ebib--pop-to-buffer (ebib--buffer 'entry)))

(defun ebib-edit-keyname ()
  "Change the key of a BibTeX entry."
  (interactive)
  (ebib--execute-when
    ((and real-db entries)
     (let ((cur-keyname (ebib--get-key-at-point)))
       (ebib--ifstring (new-keyname (read-string (format "Change `%s' to: " cur-keyname)
                                                 cur-keyname
                                                 'ebib--key-history))
           (ebib--update-keyname new-keyname))))
    (default
      (beep))))

(defun ebib--update-keyname (new-key)
  "Change the key of the current BibTeX entry to NEW-KEY.
This function updates both the database and the buffer.  Note
that nothing is changed if NEW-KEY is equal to the existing key."
  (let ((cur-key (ebib--get-key-at-point)))
    (unless (string= new-key cur-key)
      (let ((marked (ebib-db-marked-p cur-key ebib--cur-db))
            (actual-new-key (ebib-db-change-key cur-key new-key ebib--cur-db (if ebib-uniquify-keys 'uniquify 'noerror))))
        (when actual-new-key
          (ebib-db-set-current-entry-key actual-new-key ebib--cur-db)
          (when marked
            (ebib-db-toggle-mark cur-key ebib--cur-db)
            (ebib-db-toggle-mark actual-new-key ebib--cur-db))
          (with-current-ebib-buffer 'index
	    (let ((inhibit-read-only t))
              (delete-region (pos-bol) (1+ (pos-eol)))))
          (ebib--insert-entry-in-index-sorted actual-new-key t marked)
          ;; Also update dependent databases.
          (let ((dependents (seq-filter (lambda (db)
                                          (ebib-db-has-key cur-key db))
                                        (ebib--list-dependents ebib--cur-db))))
            (mapc (lambda (dependent)
                    (ebib-db-remove-entries-from-dependent cur-key dependent)
                    (ebib-db-add-entries-to-dependent actual-new-key dependent)
                    (ebib--mark-index-dirty dependent)
                    (when (ebib-db-marked-p cur-key dependent)
                      (ebib-db-unmark-entry cur-key dependent)
                      (ebib-db-mark-entry actual-new-key dependent)))
                  dependents)
            (ebib--set-modified t ebib--cur-db nil dependents)))))))

(defun ebib-mark-entry (arg)
  "Mark or unmark the current entry.
With prefix argument ARG, mark all entries if none are marked, or
unmark all marked entries."
  (interactive "P")
  (if arg
      (ebib-mark-all-entries)
    (ebib--execute-when
      (entries
       (with-current-ebib-buffer 'index
         (let ((inhibit-read-only t)
               (cur-entry (ebib--get-key-at-point)))
           (ebib-db-toggle-mark cur-entry ebib--cur-db)
           (ebib--display-mark (ebib-db-marked-p cur-entry ebib--cur-db)))
         (ebib-next-entry)))
      (default
        (beep)))))

(defun ebib--display-mark (mark)
  "Highlight/unhighlight the entry at point.
If MARK is t, `ebib-marked-face is added, if nil, it is removed."
  (let ((beg (pos-bol))
        (end (1+ (pos-eol))))
    (if mark
        (add-text-properties beg end '(face ebib-marked-face))
      (remove-text-properties beg end '(face ebib-marked-face)))))

(defun ebib-mark-all-entries ()
  "Mark or unark all entries.
If there are marked entries, all entries are unmarked.
Otherwise, all entries are marked.  If the database is filtered,
only the visible entries are marked or unmarked."
  (interactive)
  (ebib--execute-when
    (marked-entries
     (ebib-db-unmark-entry (ebib--list-keys ebib--cur-db) ebib--cur-db)
     (ebib--update-index-buffer)
     (message "All entries unmarked"))
    (entries
     (ebib-db-mark-entry (ebib--list-keys ebib--cur-db) ebib--cur-db)
     (ebib--update-index-buffer)
     (message "All entries marked"))
    (default
      (beep))))

(defun ebib-index-scroll-down ()
  "Move one page up in the database."
  (interactive)
  (ebib--execute-when
    (entries
     (scroll-down)
     (ebib--update-entry-buffer))
    (default
      (beep))))

(defun ebib-index-scroll-up ()
  "Move one page down in the database."
  (interactive)
  (ebib--execute-when
    (entries
     (scroll-up)
     (ebib--update-entry-buffer))
    (default
      (beep))))

(defun ebib--make-backup (file)
  "Create a backup of FILE.
Honour `ebib-create-backups' and BACKUP-DIRECTORY-ALIST."
  (when ebib-create-backups
    (let ((backup-file (make-backup-file-name file)))
      (if (file-writable-p backup-file)
          (copy-file file backup-file t)
        (ebib--log 'error "Could not create backup file `%s'" backup-file)))))

(defun ebib--format-database-as-bibtex (db)
  "Write database DB into the current buffer in BibTeX format."
  (ebib--format-comments db)
  (ebib--format-main db)
  (when (ebib-db-get-preamble db)
    (insert (format "@Preamble{%s}\n\n" (ebib-db-get-preamble db))))
  (ebib--format-strings db)
  ;; We define two comparison functions for `sort'.  These must simply
  ;; return non-nil if the first element is to be sorted before the second.
  (cl-flet
      ;; The first one is simple: if X has a crossref field, it must be
      ;; sorted before Y (or at least *can* be, if Y also has a crossref
      ;; field).
      ((compare-xrefs (x _)
         (or (ebib-get-field-value "xdata" x db 'noerror)
	     (ebib-get-field-value "crossref" x db 'noerror)))
       ;; This one's a bit trickier.  We iterate over the lists of fields in
       ;; `ebib-sort-order'.  For each level, `ebib--get-sortstring' then returns the
       ;; string that can be used for sorting.  If all fails, sorting is done on
       ;; the basis of the entry key.
       (entry< (x y)
               (let (sortstring-x sortstring-y)
                 (cl-loop for sort-list in ebib-sort-order do
                          (setq sortstring-x (ebib--get-sortstring x sort-list db))
                          (setq sortstring-y (ebib--get-sortstring y sort-list db))
                          while (cl-equalp sortstring-x sortstring-y))
                 (if (and sortstring-x sortstring-y)
                     (string< sortstring-x sortstring-y)
                   (string< x y)))))    ; compare entry keys
    ;; Only entries in visible the index buffer are saved, in case we're writing
    ;; a filtered db to a new file.
    (let ((sorted-list (sort (ebib--list-keys ebib--cur-db) #'string<)))
      (cond
       (ebib-save-xrefs-first
        (setq sorted-list (sort sorted-list #'compare-xrefs)))
       (ebib-sort-order
        (setq sorted-list (sort sorted-list #'entry<))))
      (mapc (lambda (key) (ebib--format-entry key db nil)) sorted-list))
    (ebib--format-local-vars db)))

(defun ebib--save-database (db &optional force)
  "Save the database DB.
The FORCE argument is used as in `ebib-save-current-database'."
  ;; See if we need to make a backup.
  (when (and (ebib-db-backup-p db)
             (file-exists-p (ebib-db-get-filename db)))
    (ebib--make-backup (ebib-db-get-filename db))
    (ebib-db-set-backup nil db))

  ;; Check if the file has changed on disk.
  (let ((db-modtime (ebib-db-get-modtime db))
        (file-modtime (ebib--get-file-modtime (ebib-db-get-filename db))))
    ;; If the file to be saved has been newly created, both modtimes are nil.
    (when (and db-modtime file-modtime
               (time-less-p db-modtime file-modtime))
      (unless (or (and (listp force)
                       (eq 16 (car force)))
                  (yes-or-no-p (format "File `%s' changed on disk.  Overwrite? " (ebib-db-get-filename db))))
        (error "[Ebib] File not saved"))))

  ;; Now save the database.
  (with-temp-buffer
    (ebib--format-database-as-bibtex db)
    (write-region (point-min) (point-max) (ebib-db-get-filename db)))
  (ebib--set-modified nil db))

(defun ebib-write-database (force)
  "Write the current database to a different file.
If the current database is filtered, only the entries that match
the filter are saved.  The original file is not deleted.

FORCE is a prefix argument.  If called with a prefix
argument (any argument will do), the database is written
unconditionally, even if the new file already exists."
  (interactive "P")
  (ebib--execute-when
    (database
     (ebib--ifstring (new-filename (expand-file-name (read-file-name "Save to file: ")))
         (when (or force
                   (not (file-exists-p new-filename))
                   (y-or-n-p (format (format "File %s already exists; overwrite? " new-filename))))
           (with-temp-buffer
             (ebib--format-database-as-bibtex ebib--cur-db)
             (write-region (point-min) (point-max) new-filename nil nil nil))
           (if (ebib-db-get-filter ebib--cur-db)
               (message "Wrote filtered entries as new database to %s" new-filename)
             ;; If this wasn't a filtered db, we rename it.
             (ebib-db-set-filename new-filename ebib--cur-db 'overwrite)
             (rename-buffer (concat (format " %d:" (1+ (- (length ebib--databases)
                                                          (length (member ebib--cur-db ebib--databases)))))
                                    (file-name-nondirectory new-filename)))
             (ebib--set-modified nil ebib--cur-db)))))
    (default
      (beep))))

(defun ebib-save-current-database (force)
  "Save the current database.
FORCE, as a prefix argument, can indicate different levels of
force.  If called with \\[universal-argument], save the database even if there were
no modifications, but ask for confirmation if the file was
modified.  If called with \\[universal-argument] \\[universal-argument], save the database even if the
file was modified."
  (interactive "P")
  (ebib--execute-when
    ((or real-db dependent-db)
     (if (and (not force)
              (not (ebib-db-modified-p ebib--cur-db)))
         (message "No changes need to be saved.")
       (ebib--save-database ebib--cur-db force)
       (ebib-db-set-modtime (ebib--get-file-modtime (ebib-db-get-filename ebib--cur-db)) ebib--cur-db)))
    (filtered-db
     ;; Saving a filtered db would result in saving only the entries that
     ;; match the filter.
     (error "[Ebib] Cannot save a filtered database.  Use `w' to write to a file"))))

(defun ebib-save-all-databases ()
  "Save all currently open databases if they were modified."
  (interactive)
  (mapc (lambda (db)
          (when (ebib-db-modified-p db)
            (ebib--save-database db)))
        ebib--databases)
  (message "All databases saved."))

(defun ebib-print-filename ()
  "Display the filename of the current database in the minibuffer."
  (interactive)
  (message (ebib-db-get-filename ebib--cur-db)))

(defun ebib-toggle-hidden ()
  "Toggle viewing hidden fields."
  (interactive)
  (setq ebib--hide-hidden-fields (not ebib--hide-hidden-fields))
  (ebib--update-entry-buffer))

(defun ebib-toggle-timestamp ()
  "Toggle using timestamp for new entries."
  (interactive)
  (setq ebib-use-timestamp (not ebib-use-timestamp)))

(defun ebib-toggle-xrefs-first ()
  "Toggle saving of crossreferenced entries first."
  (interactive)
  (setq ebib-save-xrefs-first (not ebib-save-xrefs-first)))

(defun ebib-toggle-identical-fields ()
  "Toggle `ebib-allow-identical-fields'.
If t, Ebib handles entries with identical fields by concatenating
their contents into a single field."
  (interactive)
  (setq ebib-allow-identical-fields (not ebib-allow-identical-fields)))

(defun ebib-toggle-layout ()
  "Toggle the Ebib layout."
  (interactive)
  (if (eq ebib-layout 'full)
      (setq ebib-layout 'custom)
    (setq ebib-layout 'full))
  (ebib-lower)
  (ebib))

(defun ebib-toggle-print-newpage ()
  "Toggle whether index cards are printed with a newpage after each card."
  (interactive)
  (setq ebib-print-newpage (not ebib-print-newpage)))

(defun ebib-toggle-print-multiline ()
  "Toggle whether multiline fields are printed."
  (interactive)
  (setq ebib-print-multiline (not ebib-print-multiline)))

(defun ebib-delete-entry ()
  "Delete the current entry from the database.
If there are marked entries, ask the user if they want to delete
those instead.  If the answer is negative, delete the current
entry.

In a dependent database, call `ebib-dependent-delete-entry'
instead."
  (interactive)
  (ebib--execute-when
    (dependent-db (ebib-dependent-delete-entry))
    (entries
     (let* ((mark (point-marker))
            (marked-entries (ebib-db-list-marked-entries ebib--cur-db))
            (to-be-deleted (if (and marked-entries
                                    (y-or-n-p "Delete all marked entries? "))
                               marked-entries
                             (ebib--get-key-at-point))))
       (if (listp to-be-deleted)
           (progn (dolist (key marked-entries)
                    (ebib--goto-entry-in-index key)
                    (let ((inhibit-read-only t))
                      (delete-region (pos-bol) (1+ (pos-eol))))
                    (ebib-db-remove-entry key ebib--cur-db))
                  (ebib-db-unmark-entry to-be-deleted ebib--cur-db) ; This works even though we already removed the entries from the database.
                  (message "Marked entries deleted."))
         (when (y-or-n-p (format "Delete %s? " to-be-deleted))
           (let ((inhibit-read-only t))
             (delete-region (pos-bol) (1+ (pos-eol))))
           (ebib-db-remove-entry to-be-deleted ebib--cur-db)
           (message "Entry `%s' deleted." to-be-deleted)))
       (ebib--set-modified t ebib--cur-db)
       (goto-char mark)
       (if (eobp)
           (forward-line -1))
       (ebib--update-entry-buffer)
       ;; Update dependent databases.
       (mapc (lambda (dependent)
               (let ((n-entries (ebib-db-count-entries dependent)))
                 (ebib-db-remove-entries-from-dependent to-be-deleted dependent)
                 (when (> n-entries (ebib-db-count-entries dependent)) ; If any entries were removed.
                   (ebib-db-set-modified t dependent))))
             (ebib--list-dependents ebib--cur-db))))
    (default
     (beep))))

(defun ebib-kill-entry ()
  "Kill the current entry.
The entry is put in the kill ring.  In a dependent database, the
entry is not deleted from the main database."
  (interactive)
  (ebib--execute-when
    (entries
     (let ((key (ebib--get-key-at-point))
           (mark (point-marker)))
       (with-temp-buffer
         (ebib--format-entry key ebib--cur-db)
         (kill-new (buffer-substring-no-properties (point-min) (point-max))))
       (let ((inhibit-read-only t))
         (delete-region (pos-bol) (1+ (pos-eol))))
       (if (ebib-db-dependent-p ebib--cur-db)
           (ebib-db-remove-entries-from-dependent key ebib--cur-db)
         (ebib-db-remove-entry key ebib--cur-db)
         (mapc (lambda (dependent)
                 (when (ebib-db-has-key key dependent)
                   (ebib-db-remove-entries-from-dependent key dependent)
                   (ebib-db-set-modified t dependent)))
               (ebib--list-dependents ebib--cur-db)))
       (goto-char mark)
       (if (eobp)
           (forward-line -1))
       (message (format "Entry `%s' killed.  Use `y' to yank (or `C-y' outside Ebib)." key))
       (ebib--set-modified t ebib--cur-db)
       (ebib--update-entry-buffer)))
    (default
     (beep))))

(defun ebib-yank-entry (arg)
  "Yank the BibTeX entry at the front of the kill ring.
This function works by yanking the front of the kill ring to a
temporary buffer and trying to read a BibTeX entry from the
yanked text.  If an entry is found, it is added to the current
database.  If no entry was found, just rotate the kill ring.

This command can be repeated in order to yank the next element in
the kill ring, but note that each following yank does not replace
the entry that was added during the previous yank.  Repeating the
yank is primarily meant for yanking past kill ring entries that
do not constitute BibTeX items.

It is also possible to yank @Preamble, @String or @Comment
definitions.

The prefix argument ARG functions as with \\[yank] / \\[yank-pop]."
  (interactive "P")
  (ebib--execute-when
    (real-db
     (message "%s" last-command)
     (let ((entry (current-kill (cond
                                 ((listp arg)
                                  (if (eq last-command 'ebib-yank-entry) 1 0))
                                 ((eq arg '-) -2)
                                 (t (1- arg)))))
           (needs-update nil)
           (is-modified nil)
           entry-key)
       (with-temp-buffer
         (insert entry)
         (goto-char (point-min))
         (let ((entry-type (ebib--bib-find-next-bibtex-item)))
           (cond
            ((cl-equalp entry-type "string") ; `cl-equalp' compares strings case-insensitively.
             (when (ebib--bib-read-string ebib--cur-db)
               (setq is-modified t)
               (message "[Ebib] Yanked @String definition.")))
            ((cl-equalp entry-type "preamble")
             (when (ebib--bib-read-preamble ebib--cur-db)
               (setq is-modified t)
               (message "[Ebib] Yanked @Preamble definition.")))
            ((cl-equalp entry-type "comment")
             (when (ebib--bib-read-comment ebib--cur-db)
               (setq is-modified t)
               (message "[Ebib] Yanked @Comment.")))
            ((stringp entry-type)
             (setq entry-key (ebib--bib-read-entry entry-type ebib--cur-db t))
             (if entry-key
                 (progn (ebib-db-set-current-entry-key entry-key ebib--cur-db)
                        (setq is-modified t)
                        (setq needs-update t)
                        (if (assoc-string entry-type (ebib--list-entry-types (ebib--get-dialect ebib--cur-db) t) 'case-fold)
                            (message "[Ebib] Yanked entry.")
                          (message "[Ebib] Yanked unknown entry type `%s'." entry-type)))
               (message "[Ebib] Could not yank a valid entry")))
            (t (message "[Ebib] No entry in kill ring: `%s'." entry)))))
       (when is-modified
         (ebib--set-modified t ebib--cur-db t))
       (when needs-update
         (ebib--insert-entry-in-index-sorted entry-key t)
         (ebib--update-entry-buffer))))
    (default
      (beep))))

(defun ebib-select-and-popup-entry ()
  "Make the entry at point current and display it.
If `ebib-layout' is set to `index-only', also popup the entry
buffer and switch to it."
  (interactive)
  (ebib--execute-when
    (entries
     (ebib--update-entry-buffer)
     (when (eq ebib-layout 'index-only)
       ;; This makes the entry buffer visible but then switches to the
       ;; index buffer again.
       (ebib--pop-to-buffer (ebib--buffer 'entry))
       (ebib--pop-to-buffer (ebib--buffer 'index))))
    (default
      (beep))))

(defun ebib-jump-to-entry (arg)
  "Jump to an entry.
Select an entry from any open database using completion and jump
to it.  By default, completion is performed using only the entry
keys, but if either `selectrum', `ivy', `helm' or `ido' is
available, completion is more sophisticated, allowing to select
an entry using the author, year and title.

If prefix argument ARG is non-nil, only offer selection
candidates from the current database."
  (interactive "P")
  (ebib--execute-when
    (database
     (let* ((sources (if arg (list ebib--cur-db) ebib--databases))
            (entries (ebib-read-entry "Jump to entry: " sources))
            ;; The `ebib-read-entry-*' functions return a list of selected
            ;; entries. We can only jump to one of them, obviously. Jumping to
            ;; the last one makes the most sense.
            (key (caar (last entries)))
            (db (cdar (last entries))))
       (cond
        ((eq db ebib--cur-db)
         (if (ebib--goto-entry-in-index key)
             (ebib--update-entry-buffer)
           (error "[Ebib] Could not find entry key `%s'" key)))
        (entries
         (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db) ; Save the current entry for the database we're jumping away from.
         (ebib-db-set-current-entry-key key db) ; Set the current entry in the database we're jumping to.
         (setq ebib--cur-db db)
         (ebib--update-buffers 'no-refresh))
        (t (error "[Ebib] Could not jump to entry")))))
    (default
      (error "[Ebib] No database opened"))))

(defun ebib--create-completion-collection (databases &optional prepend-db)
  "Create a collection for use in `ebib-read-entry-*' functions.
The keys for the collection are taken from DATABASES.  Return
value is an alist with the completion strings as keys and a list
of entry key and database as values.  If PREPEND-DB is non-nil,
the database name is prepended to the candidate string.  This is
especially useful if helm or ivy is used as completion system.

If PREPEND-DB is nil, the database name is added to the string in
the text property `selectrum-candidate-display-right-margin'.
When selectrum is used as the completion system, the database is
displayed at the right margin.  If selectrum is not used, the key
is prepended to the completion candidates."
  (seq-reduce (lambda (coll db)
                (let ((file (propertize (ebib-db-get-filename db 'short) 'face 'ebib-display-bibfile-face)))
                  (append (mapcar (lambda (key)
                                    (let ((candidate (format "%s (%s) «%s»"
                                                             (ebib--get-field-value-for-display "Author/Editor" key db 'face 'ebib-display-author-face)
                                                             (ebib--get-field-value-for-display "Year" key db 'face 'ebib-display-year-face)
                                                             (ebib--get-field-value-for-display "Title" key db))))
                                      (setq candidate
                                            (cond
                                             (prepend-db
                                              (concat (format "%-20s  " file) "  " candidate))
                                             ((and (boundp 'selectrum-mode) selectrum-mode)
                                              (propertize candidate 'selectrum-candidate-display-right-margin file))
                                             (t (concat key "  " candidate))))
                                      (cons candidate (list key db))))
                                  (ebib-db-list-keys db))
                          coll)))
              databases nil))

(defun ebib-read-entry (prompt databases &optional multiple filter)
  "Read an entry from the user.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt string to use.  If MULTIPLE is
non-nil, multiple keys can be selected.

This function calls one of the `ebib-read-entry-*' functions
depending on the completion system in use.

If non-nil FILTER is a function applied to the entries of all
databases.  It is called for each entry with three arguments: the
key, the entry itself (as an alist) and the database.  It should
return non-nil for all entries which should be included in the
prompt."
  (let ((dbs
	 (if filter
	     (mapcar
	      (lambda (db)
		(let ((filtered-entries (make-hash-table)))
		  (maphash (lambda (k v)
			     (when (funcall filter k v db)
			       (puthash k v filtered-entries)))
			   (ebib-db-val 'entries db))
		  `((entries . ,filtered-entries) ,@(cdr db))))
	      databases)
	   databases)))
    (cond
     ((and (boundp 'ivy-mode) ivy-mode) (ebib-read-entry-ivy prompt dbs))
     ((and (boundp 'helm-mode) helm-mode) (ebib-read-entry-helm prompt dbs))
     ((and multiple ebib-citation-insert-multiple (ebib-read-entry-multiple prompt dbs)))
     ((and (boundp 'ido-mode) ido-mode) (ebib-read-entry-ido prompt dbs))
     (t (ebib-read-entry-single prompt dbs)))))

(defun ebib-read-entry-ivy (prompt databases)
  "Read an entry from the user using ivy.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

It is possible to select multiple entries either by using
`ivy-call' or by marking them with `ivy-mark'.

Return value is a list of cons cells of the selected keys and the
databases containing them."
  (let ((minibuffer-allow-text-properties t)
        (ivy-sort-max-size (expt 256 6))
        entries)
    (let ((collection (ebib--create-completion-collection databases t)))
      (if (not collection)
          (error "[Ebib] No entries found in database(s)")
        (ivy-read prompt collection
                  :action (lambda (item)
                            (let ((key (cadr item))
                                  (db (caddr item)))
                              (unless (cl-find key entries :key #'car)
                                (push (cons key db) entries))))
                  :history 'ebib--citation-history
                  :sort t)
        (nreverse entries)))))

(defun ebib-helm-action-function (_)
  "Return a list of the selected candidates.
Each element is a cons cell of a candidate and the database that
contain it."
  (mapcar (lambda (item)
            (cons (nth 0 item) (nth 1 item)))
          (helm-marked-candidates)))

(defun ebib-read-entry-helm (prompt databases)
  "Read an entry from the user using helm.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

It is possible to select multiple entries in the helm buffer.

Return value is a list of cons cells of the selected keys and the
databases containing them."
  (let ((sources (helm-build-sync-source prompt
                                         :candidates (ebib--create-completion-collection databases t)
                                         :action '(("Select entry" . ebib-helm-action-function)))))
    (helm :sources sources
          :sort t
          :buffer "*helm ebib*"
          :prompt "Select entry: ")))

(defun ebib-read-entry-ido (prompt databases)
  "Read an entry from the user using ido.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

For compatibility with the other `ebib-read-entry-*' functions,
the return value is a list with a single cons cell of the key and
the database containing the selected entry."
  (let ((collection (ebib--create-completion-collection databases)))
    (if collection
        (let* ((candidates (mapcar #'car collection))
               (entry (ido-completing-read prompt candidates nil t nil 'ebib--key-history))
               (key (cadr (assoc-string entry collection)))
               (db (caddr (assoc-string entry collection))))
          (list (cons key db)))
      (error "[Ebib] No BibTeX entries found"))))

(defun ebib-read-entry-single (prompt databases)
  "Read an entry from the user using default completion.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

For compatibility with the other `ebib-read-entry-*' functions,
the return value is a list with a single cons cell of the key and
the database containing the selected entry."
  (let ((collection (ebib--create-completion-collection databases)))
    (if collection
        (let* ((entry (completing-read prompt collection nil t nil 'ebib--key-history))
               (key (cadr (assoc-string entry collection)))
               (db (caddr (assoc-string entry collection))))
          (list (cons key db)))
      (error "[Ebib] No BibTeX entries found"))))

(defun ebib-read-entry-multiple (prompt databases)
  "Read a list of entries from the user using default completion.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

Return value is a list of cons cells of the selected keys and the
databases containing them."
  (let ((collection (ebib--create-completion-collection databases)))
    (if collection
        (let* ((crm-local-must-match-map (make-composed-keymap '(keymap (32)) crm-local-must-match-map))
               (crm-separator "\\s-*&\\s-*")
               (keys (completing-read-multiple prompt collection nil t nil 'ebib--key-history)))
          (mapcar (lambda (entry)
                    (cons (cadr (assoc-string entry collection))
                          (caddr (assoc-string entry collection))))
                  keys))
      (error "[Ebib] No BibTeX entries found"))))

(defun ebib-export-entries (prefix)
  "Export entries to another database.
With PREFIX argument, export entries to a file.  Otherwise export
entries to another open database.  The user is asked for the file
or database to export entries to.

This function operates on the marked entries or, if no entries
are marked, on the current entry."
  (interactive "P")
  (ebib--execute-when
    (entries
     (let ((entries (or (ebib-db-list-marked-entries ebib--cur-db)
                        (list (ebib--get-key-at-point)))))
       (if prefix
           (let ((filename (expand-file-name (read-file-name "File to export entries to: " nil nil nil ebib--export-filename))))
             (if (file-writable-p filename)
                 (ebib--export-entries-to-file entries filename ebib--cur-db)
               (error "[Ebib] Cannot write to file `%s'" filename)))
         (let* ((target-db (ebib-read-database "Export entries to database: ")))
           (if target-db
               (ebib--export-entries-to-db entries target-db ebib--cur-db)
             (error "[Ebib] Could not export entries"))))))
    (default (beep))))

(defun ebib--export-entries-to-db (entries target-db source-db)
  "Export ENTRIES from SOURCE-DB to TARGET-DB.
ENTRIES is a list of entry keys."
  (if (ebib-db-dependent-p target-db)
      (error "Cannot export entries to a dependent database ")
    (let ((target-keys-list (ebib-db-list-keys target-db))
          (modified nil))
      (mapc (lambda (key)
              (if (member key target-keys-list)
                  (ebib--log 'message "Entry key `%s' already exists in database %s" key (ebib-db-get-filename target-db 'short))
                (ebib--store-entry key (copy-tree (ebib-db-get-entry key source-db)) target-db t)
                (setq modified t)))
            entries)
      (when modified
        (ebib--mark-index-dirty target-db)
        (ebib-db-set-modified :database target-db)))))

(defun ebib--export-entries-to-file (entries filename source-db)
  "Export ENTRIES from SOURCE-DB to FILENAME.
ENTRIES is a list of entry keys."
  (with-temp-buffer
    (insert "\n")
    (mapc (lambda (key)
            (ebib--format-entry key source-db nil))
          entries)
    (append-to-file (point-min) (point-max) filename)
    (setq ebib--export-filename filename)))

(defvar ebib-search-map
  (let ((map (make-keymap)))
    (suppress-keymap map 'no-digits)
    (define-key map [return] #'ebib-search-next)
    (define-key map [left] #'ebib-search-prev-db)
    (define-key map [right] #'ebib-search-next-db)
    (define-key map "g" #'ebib-search-goto-first-entry)
    map)
  "Keymap that is active when a search is preformed.")

(defun ebib-search (arg)
  "Search the current Ebib database.
The search is conducted with `string-match-p' and can therefore be
a regexp.  Searching starts with the current entry.  In a
filtered database, only the visible entries are searched.

If prefix argument ARG is non-nil, do not ask for a search
string, search for the previous search string instead."
  (interactive "P")
  (ebib--execute-when
    (entries
     (ebib--ifstring (search-str (or (and arg ebib--search-string)
                                     (read-string "Search database for: ")))
         (progn (set-transient-map ebib-search-map t (lambda () (message "Search ended.  Use `C-u /' to resume.")))
                (setq ebib--search-string search-str)
                ;; First we search the current entry.
                (if (ebib--search-in-entry ebib--search-string
                                           (ebib-db-get-entry (ebib--get-key-at-point) ebib--cur-db))
                    (progn (ebib--update-entry-buffer ebib--search-string)
                           (message "Found search string in current entry.  RET for next match."))
                  ;; If the search string wasn't found in the current entry, we continue searching.
                  (ebib-search-next)))))
    (default
      (beep))))

(defun ebib-search-repeat-last ()
  "Continue the last search."
  (interactive)
  (ebib-search t))

(defun ebib-search-next ()
  "Search the next occurrence of `ebib--search-string'.
Searching starts at the entry following the current entry.  If a
match is found, the matching entry is shown and becomes the new
current entry.  If a filter is active, only the visible entries
are searched."
  (interactive)
  (ebib--execute-when
    (entries
     (if (null ebib--search-string)
         (message "No search string")
       (let ((cur-search-entry (cdr (member (ebib--get-key-at-point) (ebib--list-keys)))))
         (while (and cur-search-entry
                     (null (ebib--search-in-entry ebib--search-string
                                                  (ebib-db-get-entry (car cur-search-entry) ebib--cur-db 'noerror))))
           (setq cur-search-entry (cdr cur-search-entry)))
         (if (null cur-search-entry)
             (message (format "`%s' not found.  [g] to jump to top, [left]/[right] to search previous/next database." ebib--search-string))
           (ebib-db-set-current-entry-key (car cur-search-entry) ebib--cur-db)
           (ebib--goto-entry-in-index (car cur-search-entry))
           (message "Found search string in entry `%s'.  RET for next match." (ebib--get-key-at-point))
           (ebib--update-entry-buffer ebib--search-string)))))
    (default
      (beep))))

(defun ebib--search-in-entry (search-str entry &optional field)
  "Search for SEARCH-STR in ENTRY in the current database.
Return a list of fields in ENTRY that match the regexp
SEARCH-STR, or nil if no matches were found.  If FIELD is given,
only that field is searched.  ENTRY is an alist of (FIELD . VALUE)
pairs.

Normally, the `=type=' field, which stores the entry type, is not
searched, but it is possible to search for specific entry types by
specifying `=type=' for FIELD.  In that case, the search string
can still be a string, but only exact matches will return a
result."
  (let ((case-fold-search t)  ; We want to ensure a case-insensitive search.
        (result nil))
    (if field
        (let ((value (cdr (assoc-string field entry 'case-fold))))
          (when (and value
                     (or (and (string= field "=type=") ; The =type= field requires an exact match.
                              (cl-equalp search-str value))
                         (string-match-p search-str value)))
            (setq result (list field))))
      (mapc (lambda (f)
              (when (and (not (ebib--special-field-p (car f))) ; We exlude special fields here.
                         (stringp (cdr f))
                         (string-match-p search-str (cdr f)))
                (setq result (cons (car f) result))))
            entry))
    result))


(defcustom ebib-follow-current-field-crossref t
  "Consider only current crossref fields when following crossrefs.
If non-nil, `ebib-follow-crossref' will only consider keys
present in the value of the current field, when called with point
on a field which crossrefs other entries (`crossref', `xdata' or
`xref')."
  :group 'ebib
  :type 'boolean)

(defun ebib-follow-crossref (&optional arg)
  "Jump to an entry cross-referenced from the current entry.
Cross-referenced entries are those whose key appears in the
\"crossref\", \"xdata\" or \"xref\" fields of the current entry.

If the entry buffer is current, and the current field is one of
\"crossref\", \"xdata\" or \"xref\", and has a value, then only
consider keys in that value.  ARG forces considering all
cross-referencing keys.

If there is only one such entry, switch to it.  If there is more
than one, allow the user to choose one from a list.  If there are
none, search for the first entry with the current entry's key in
one of its cross-referencing fields."
  (interactive "P")
  (ebib--execute-when
    (entries
     (if-let ((all-xref-list (ebib--get-xref-alist (ebib--get-key-at-point) ebib--cur-db))
	      (xref-list (if (and ebib-follow-current-field-crossref (not arg)
				  (eq (current-buffer) (ebib--buffer 'entry))
				  (member (ebib--current-field) (mapcar #'car all-xref-list)))
			     (cl-remove-if-not
			      (lambda (x) (string= (car x) (ebib--current-field)))
			      all-xref-list)
			   all-xref-list))
	      ;; If there is only one crossref key, jump to it
	      (xref (if (cdr xref-list)
			;; Otherwise read a key from the user
			(ebib--read-ref-as-entry
			 "Crossref key" `(,ebib--cur-db) nil
			 ;; Filter presented keys for just those crossreffed in current entry
			 (lambda (key _ent _db) (member key (mapcar #'cdr xref-list))))
		      (cdar xref-list))))
	 ;; If the entry has a crossref, see if we can find the relevant entry.
         (let ((database (seq-find (lambda (db)
				     (ebib-db-get-entry xref db 'noerror))
                                   ebib--databases)))
           (unless database
	     (error "[Ebib] Entry `%s' not found in any open database" xref))
           ;; If the entry exists, switch to the relevant database and try to
           ;; show the entry.
           (ebib-switch-to-database database)
           (if (not (ebib--key-in-index-p xref))
               (error "[Ebib] Crossreference `%s' not visible due to active filter" xref)
	     (ebib--goto-entry-in-index xref)
	     (ebib--update-entry-buffer)))
       ;; If the entry has no crossref, we assume the user wants to search for
       ;; entries cross-referencing the current one.
       (setq ebib--search-string (ebib--get-key-at-point))
       (set-transient-map ebib-search-map t (lambda () (message "Search ended.  Use `C-u /' to resume.")))
       (ebib-search-next)))
    (default (beep))))

(defun ebib-search-goto-first-entry ()
  "Goto the first entry and issue a message that search is still active."
  (interactive)
  (ebib-goto-first-entry)
  (message "Jumped to first entry in database.  Continue searching with RET."))

(defun ebib-search-prev-db ()
  "Go to the previous database and issue a message that search is still active."
  (interactive)
  (ebib-prev-database t)
  (message "Switched to previous database.  Continue searching with RET."))

(defun ebib-search-next-db ()
  "Go to the next database and issue a message that search is still active."
  (interactive)
  (ebib-next-database t)
  (message "Switched to next database.  Continue searching with RET."))

(defun ebib-edit-strings ()
  "Edit the @String definitions in the database."
  (interactive)
  (ebib--execute-when
    (real-db
     (ebib--fill-strings-buffer)
     (ebib--pop-to-buffer (ebib--buffer 'strings))
     (goto-char (point-min)))
    (default
      (beep))))

(defun ebib-edit-preamble ()
  "Edit the @Preamble definition in the database."
  (interactive)
  (ebib--execute-when
    (real-db
     (ebib--multiline-edit (list 'preamble (ebib-db-get-filename ebib--cur-db)) (ebib-db-get-preamble ebib--cur-db)))
    (default
      (beep))))

(defun ebib-export-preamble (prefix)
  "Export the @Preamble definition.

With PREFIX argument, export the @Preamble to a file.  Otherwise export
the @Preamble to another open database.  The user is asked for the file
or database to export the @Preamble to.

If the goal database already has a preamble, the @Preamble is be
appended to it."
  (interactive "P")
  (ebib--execute-when
    (real-db
     (if (null (ebib-db-get-preamble ebib--cur-db))
         (error "[Ebib] No @Preamble defined"))
     (if prefix
         (let ((filename (expand-file-name (read-file-name "File to export @Preamble to: " nil nil nil ebib--export-filename))))
           (if (file-writable-p filename)
               (with-temp-buffer
                 (insert "\n")
                 (insert (format "\n@Preamble{%s}\n\n" (ebib-db-get-preamble ebib--cur-db)))
                 (append-to-file (point-min) (point-max) filename)
                 (setq ebib--export-filename filename))
             (error "[Ebib] Cannot write to file `%s'" filename)))
       (let ((target-db (ebib-read-database "Export @Preamble to database: ")))
         (unless target-db
           (error "[Ebib] Could not export @Preamble"))
         (if (not (ebib--real-database-p target-db))
             (error "[Ebib] Cannot export to filtered or dependent database"))
         (ebib-db-set-preamble (ebib-db-get-preamble ebib--cur-db) target-db 'append)
         (ebib-db-set-modified t target-db))))
    (default
      (beep))))

(defun ebib-print-entries ()
  "Create a LaTeX file listing the entries.
Either prints the entire database, or the marked entries."
  (interactive)
  (ebib--execute-when
    (entries
     (let ((entries (ebib--sort-keys-list (or (ebib-db-list-marked-entries ebib--cur-db)
                                              (ebib-db-list-keys ebib--cur-db))
                                          ebib--cur-db)))
       (ebib--ifstring (tempfile (if (not (string= "" ebib-print-tempfile))
                                     ebib-print-tempfile
                                   (read-file-name "Use temp file: ")))
           (progn
             (with-temp-buffer
               (when ebib-print-preamble
                 (mapc (lambda (string)
                         (insert (format "%s\n" string)))
                       ebib-print-preamble))
               (insert "\n\\begin{document}\n\n")
               (mapc (lambda (entry-key)
                       ;; First create a table.
                       (insert "\\begin{tabular}{p{0.2\\textwidth}p{0.8\\textwidth}}\n")
                       ;; Insert the entry type.
                       (let ((entry (ebib-db-get-entry entry-key ebib--cur-db)))
                         (insert (format "\\multicolumn{2}{l}{\\texttt{%s (%s)}}\\\\\n"
                                         entry-key (cdr (assoc "=type=" entry))))
                         (insert "\\hline\n")
                         ;; Then the other fields.
                         (mapc (lambda (field)
                                 (ebib--ifstring (value (cdr (assoc-string field entry 'case-fold)))
                                     (when (or (not (ebib--multiline-p value))
                                               ebib-print-multiline)
                                       (insert (format "%s: & %s\\\\\n"
                                                       field (ebib-unbrace value))))))
                               ;; Note: ebib--list-fields returns a list with `=type=' as its first element.
                               (cdr (ebib--list-fields (cdr (assoc "=type=" entry)) 'all (ebib--get-dialect ebib--cur-db)))))
                       (insert "\\end{tabular}\n\n")
                       (insert (if ebib-print-newpage
                                   "\\newpage\n\n"
                                 "\\bigskip\n\n")))
                     entries)
               (insert "\\end{document}\n")
               (write-region (point-min) (point-max) tempfile))
             (ebib-lower)
             (find-file tempfile)))))
    (default
      (beep))))

(defun ebib-latex-entries ()
  "Create a LaTeX file that \\nocites entries from the current database.
Operates either on all entries or on the marked entries."
  (interactive)
  (ebib--execute-when
    ((and real-db entries)
     (ebib--ifstring (tempfile (if (not (string= "" ebib-print-tempfile))
                                   ebib-print-tempfile
                                 (read-file-name "Use temp file: ")))
         (let* ((dialect (ebib--get-dialect ebib--cur-db))
                (preamble (alist-get dialect ebib-latex-preamble)))
           (with-temp-buffer
             (when preamble
               (mapc (lambda (line)
                       (insert (format "%s\n" line)))
                     preamble))
             (if (eq dialect 'biblatex)
                 (insert (format "\n\\addbibresource{%s}\n" (expand-file-name (ebib-db-get-filename ebib--cur-db)))))
             (insert "\n\\begin{document}\n\n")
             (if (ebib-db-marked-entries-p ebib--cur-db)
                 (dolist (entry (ebib--sort-keys-list (ebib-db-list-marked-entries ebib--cur-db) ebib--cur-db))
                   (insert (format "\\nocite{%s}\n" entry)))
               (insert "\\nocite{*}\n"))
             (pcase dialect
               ('biblatex (insert "\n\\printbibliography\n\n"))
               ('BibTeX (insert (format "\n\\bibliography{%s}\n\n" (expand-file-name (ebib-db-get-filename ebib--cur-db))))))
             (insert "\\end{document}\n")
             (write-region (point-min) (point-max) tempfile))
           (ebib-lower)
           (find-file tempfile))))
    (default
      (beep))))

(defun ebib-switch-to-database-nth (num)
  "Switch to database NUM."
  (interactive "NSwitch to database number: ")
  (let ((new-db (nth (1- num) ebib--databases)))
    (unless new-db
      (error "[Ebib] Database %d does not exist" num))
    (ebib-switch-to-database new-db)))

(defun ebib-switch-to-database (db)
  "Make DB the active database."
  ;; First save the current entry key of the still current database.
  (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
  (setq ebib--cur-db db)
  (ebib--update-buffers 'no-refresh))

(defun ebib-next-database (&optional arg)
  "Switch to the next database.
If ARG is non-nil, make the first entry the current entry in the
new database."
  (interactive "P")
  (ebib--execute-when
    (database
     (let ((new-db (if (eq ebib--cur-db (car (last ebib--databases)))
                       (car ebib--databases)
                     (cadr (member ebib--cur-db ebib--databases)))))
       (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
       (setq ebib--cur-db new-db)
       (when arg
         (ebib-db-set-current-entry-key nil ebib--cur-db))
       (ebib--update-buffers 'no-refresh)))))

(defun ebib-prev-database (&optional arg)
  "Switch to the preceding database.
If ARG is non-nil, make the first entry the current entry in the
new database."
  (interactive "P")
  (ebib--execute-when
    (database
     (let ((new-db (if (eq ebib--cur-db (car ebib--databases))
                       (car (last ebib--databases))
                     (car (last ebib--databases (1+ (length (member ebib--cur-db ebib--databases))))))))
       (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
       (setq ebib--cur-db new-db)
       (when arg
         (ebib-db-set-current-entry-key nil ebib--cur-db))
       (ebib--update-buffers 'no-refresh)))))

(defun ebib-browse-url (&optional arg)
  "Browse the URL in the \"url\" field.
If the \"url\" field contains more than one URL, ask the user
which one to open.  Alternatively, the user can provide a numeric
prefix argument ARG."
  (interactive "P")
  (ebib--execute-when
    (entries
     (let ((urls (ebib-get-field-value "url" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)))
       (if urls
           (ebib--call-browser (ebib--select-url urls (if (numberp arg) arg nil)))
         (error "[Ebib] No URL found in url field"))))
    (default
      (beep))))

(defun ebib-browse-doi ()
  "Open the DOI in the \"doi\" field in a browser.
The \"doi\" field may contain only one DOI.  If necessary, the
DOI is prepended with the URL \"https://dx.doi.org/\" before
being sent to the browser."
  (interactive)
  (ebib--execute-when
    (entries
     (let ((doi (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)))
       (unless doi
         (error "[Ebib] No DOI found in doi field"))
       (ebib--call-browser (if (string-match-p "^[[:space:]]*https?://dx.doi.org/" doi)
                               doi
                             (concat "https://dx.doi.org/" doi)))))
    (default
      (beep))))

(defun ebib--call-browser (url)
  "Send URL to a browser."
  (if ebib-browser-command
      (progn
        (message "Executing `%s %s'" ebib-browser-command url)
        (call-process ebib-browser-command nil 0 nil url))
    (message "Opening `%s'" url)
    (browse-url url)))

(defun ebib-view-file (arg)
  "View a file in the \"file\" field.
The \"file\" field may contain more than one filename.  In that
case, a numeric prefix argument ARG can be used to specify which
file to choose."
  (interactive "P")
  (ebib--execute-when
    (entries
     (let ((file (ebib-get-field-value "file" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref))
           (num (if (numberp arg) arg nil)))
       (ebib--call-file-viewer (ebib--select-file file num (ebib--get-key-at-point)))))
    (default
      (beep))))

(defun ebib--call-file-viewer (file)
  "Open FILE with an external viewer."
  (let ((file-full-path (ebib--expand-file-name file)))
    (if (file-exists-p file-full-path)
        (let* ((ext (file-name-extension file-full-path))
               (viewer (cdr (assoc ext ebib-file-associations))))
	  (cond
	   ((stringp viewer)
            (if (string-match (regexp-quote "%s") viewer)
                (let* ((viewer-arg-list (split-string-and-unquote (format viewer (shell-quote-argument file-full-path)))))
                  (message "Executing `%s'" (string-join viewer-arg-list " "))
                  (apply 'call-process (car viewer-arg-list) nil 0 nil (cdr viewer-arg-list)))
              (message "Executing `%s %s'" viewer file-full-path)
              (call-process viewer nil 0 nil file-full-path)))
	   ((functionp viewer)
	    (funcall viewer file-full-path))
	   ((equal viewer nil)
            (message "Opening `%s'" file-full-path)
            (ebib-lower)
            (find-file file-full-path))
	   (t (error "[Ebib] Invalid viewer for extension `%s'" ext))))
      (error "[Ebib] File not found: `%s'" (funcall ebib-file-name-mod-function file nil)))))

(defun ebib-set-dialect (dialect)
  "Set the BibTeX dialect of the current database.
Possible values for DIALECT are those in `bibtex-dialect-list' or
nil, in which case the dialect is unset (and the default dialect
is used)."
  (interactive (list (intern (completing-read "Dialect: " (append (mapcar #'symbol-name bibtex-dialect-list) (list "nil")) nil t))))
  (unless (or (not dialect)
              (memq dialect bibtex-dialect-list))
    (error "[Ebib] Not a valid BibTeX dialect: %s" dialect))
  (ebib--execute-when
    (database
     (ebib-db-set-dialect dialect ebib--cur-db)
     (let ((lvars (ebib-db-get-local-vars ebib--cur-db)))
       (setq lvars (if dialect
                       (ebib--local-vars-add-dialect lvars dialect 'overwrite)
                     (ebib--local-vars-delete-dialect lvars)))
       (ebib-db-set-local-vars lvars ebib--cur-db))
     (ebib--set-modified t ebib--cur-db t (ebib--list-dependents ebib--cur-db))
     (ebib--update-entry-buffer))
    (default
      (beep))))

(defun ebib-show-log ()
  "Display the contents of the log buffer."
  (interactive)
  (ebib--pop-to-buffer (ebib--buffer 'log)))

(defun ebib--colorize-substring (str beg end)
  "Return a copy of STR with `ebib-warning-face' added from BEG to END."
  (let ((new-str (copy-sequence str)))
    (add-text-properties beg end '(face ebib-warning-face) new-str)
    new-str))

(defun ebib--process-citation-template (format-string &optional key db prompt)
  "Create a citation command using FORMAT-STRING.
If FORMAT-STRING contains a %K directive, it is replaced with
KEY.  DB is the database that contains KEY.  Return value is the
citation command as a string.

FORMAT-STRING may contain any number of %A directives for
additional arguments to the citation.  The user is asked to
supply a string for each of them, which may be empty.

Each %A directive may be wrapped in a %<...%> pair, containing
optional material both before and after %A.  If the user supplies
an empty string for such an argument, the optional material
surrounding it is not included in the citation command.

When prompting for an argument, FORMAT-STRING is included in the
prompt.  PROMPT, if non-nil, is a plist with a `:before' and
`:after' string (possibly nil), which are included in the prompt
before and after FORMAT-STRING.

FORMAT-STRING may also contain a %D directive.  This is replaced
with a description, for which the user is prompted, although a
default value is provided, which the user can accept by hitting
RET.  The default value is created using the function in the user
option `ebib-citation-description-function' on the basis of the
data for entry KEY in DB."
  (when (and (string-match "%K" format-string)
             key)
    (setq format-string (replace-match key t t format-string)))
  (cl-loop for n = 1 then (1+ n)
           until (null (string-match "%<\\(.*?\\)%A\\(.*?\\)%>\\|%A\\|%D" format-string))
           do
           (let* ((data (match-data))
                  (arg-type (match-string 0 format-string))
                  (beg (match-beginning 0))
                  (end (match-end 0))
                  (arg-prompt (if (string= arg-type "%D") "Description" "Argument"))
                  (default (when (and key db (string= arg-type "%D"))
                             (funcall ebib-citation-description-function key db))) ; This may destroy the match data.
                  (prompt (format "%s%s%s%s: "
                                  arg-prompt
                                  (if (string= arg-prompt "Argument") (format " %s" n) "")
                                  (concat " in «"
                                          (plist-get prompt :before)
                                          (ebib--colorize-substring format-string beg end)
                                          (plist-get prompt :after)
                                          "»")
                                  (if default (concat " (default: «" default "»)") "")))
                  (replacement (ebib--ifstring (argument (read-string prompt nil nil default))
                                   (concat "\\1" argument "\\2")
                                 "")))
             (set-match-data data)
             (setq format-string (replace-match replacement t nil format-string)))
           finally return format-string))

(defun ebib--split-citation-string (format-string)
  "Split up FORMAT-STRING.
The return value is a list of (BEFORE REPEATER SEPARATOR AFTER),
where BEFORE is the part before the repeating part of
FORMAT-STRING, REPEATER the repeating part, SEPARATOR the string
to be placed between each instance of REPEATER and AFTER the part
after the last instance of REPEATER.  Each element can be nil, if
it is not present in FORMAT-STRING.  If there is no repeating
part, REPEATER contains just the %K directive and SEPARATOR is
nil."
  (let (before repeater separator after)
    ;; First check if the format string has a repeater and if so, separate each component.
    (cond
     ((string-match "\\(.*?\\)%(\\(.*\\)%\\(.*?\\))\\(.*\\)" format-string)
      (setq before (match-string 1 format-string)
            repeater (match-string 2 format-string)
            separator (match-string 3 format-string)
            after (match-string 4 format-string)))
     ;; Otherwise extract the %K directive and everything before and after.
     ((string-match "\\(.*?\\)\\(%K\\)\\(.*\\)" format-string)
      (setq before (match-string 1 format-string)
            repeater (match-string 2 format-string)
            after (match-string 3 format-string))))
    (cl-values before repeater separator after)))

(defun ebib--create-citation (mode keys &optional db)
  "Create a citation appropriate for a buffer with MODE as major mode.
Return value is the citation as a string.

KEYS is a list of keys for which to create the citation, DB the
database that contains the KEYS.  If DB is nil, citation commands
that prompt the user for a description cannot suggest a default
value.

The citation is based on a template that the user selects from a
set of templates defined for major mode MODE.  See the user
option `ebib-citation-commands' for details.

If the user does not provide a template, this function returns
the entry key or keys as a string, concatenated with a separator
for which the user is prompted."
  (let ((templates (or (cadr (assq mode ebib-citation-commands))
                       (cadr (assq 'any ebib-citation-commands)))))
    (ebib--ifstring (template (cadr (assoc (completing-read "Command to use: " templates nil nil nil 'ebib--citation-history)
                                           templates)))
        (cl-multiple-value-bind (before repeater separator after) (ebib--split-citation-string template)
          (when (and (not separator) (> (length keys) 1))
            (setq separator (read-string "Separator: ")))
          (let* ((formatted-keys (mapconcat (lambda (key)
                                              (ebib--process-citation-template repeater key db))
                                            keys
                                            separator))
                 (formatted-before (ebib--process-citation-template before (car keys) db `(:after ,(concat formatted-keys after))))
                 (formatted-after (ebib--process-citation-template after (car keys) db `(:before ,(concat formatted-before formatted-keys)))))
            (concat formatted-before formatted-keys formatted-after)))
      ;; If the user doesn't provide a command, we just insert the entry key or keys:
      (string-join keys (if (> (length keys) 1) (read-string "Separator: "))))))

(defun ebib-push-citation ()
  "Push a citation based on the current entry to an external buffer.
The user is prompted for the buffer to push the entry into.  The
citation is created using the format strings in
`ebib-citation-commands', which depend on the major mode of the
buffer to which the citation is pushed.  If there are marked
entries, ask whether to push them all.  If not, push only the
current entry."
  (interactive)
  (ebib--execute-when
    (entries
     (let ((keys (if (and (ebib-db-marked-entries-p ebib--cur-db)
                          (y-or-n-p "Push marked entries? "))
                     (ebib-db-list-marked-entries ebib--cur-db)
                   (list (ebib--get-key-at-point))))
           (buffer (read-buffer "Push to buffer: " ebib--push-buffer t)))
       (when buffer
         (setq ebib--push-buffer buffer)
         (let ((citation-command (ebib--create-citation (buffer-local-value 'major-mode (get-buffer buffer))
                                                        keys ebib--cur-db)))
           (when citation-command
             (with-current-buffer buffer
               (insert citation-command))
             (message "Pushed %s to buffer %s" (if (= (length keys) 1) "entry" "entries") buffer))))))
    (default
      (beep))))

(defun ebib--get-or-open-db (file)
  "Return the database for FILE.
If no database associated with FILE is currently open, try to
open FILE.  If no database can be found, return nil."
  (or (ebib--get-db-from-filename file)
      (let ((full-path (expand-file-name file)))
        (when (file-readable-p full-path)
          (ebib--load-bibtex-file-internal full-path)))))

;;;###autoload
(defun ebib-insert-citation ()
  "Insert a citation at POINT.
The user is prompted for a BibTeX key and has to choose one from
the database(s) associated with the current buffer (see
`ebib--get-local-bibfiles' for details), or from the current
database if the current buffer has no associated databases.

If the current buffer is associated with a dependent database,
the entries of its main database are offered as completion
candidates and the entry that is selected is added to the
dependent database if not already there.

This function uses a dedicated completion framework (selectrum,
ivy, helm or ido), if available and active.  It also honors the
option `ebib-citation-insert-multiple'."
  (interactive)
  (unless ebib--initialized
    (ebib-init))
  (ebib--execute-when
    (database
     (let* ((database-files (ebib--get-local-bibfiles))
            (databases (or (delq nil (mapcar #'ebib--get-or-open-db database-files))
                           ebib--databases))
            dependent-db)
       (when (and (= (length databases) 1)
                  (ebib-db-dependent-p (car databases)))
         (setq dependent-db (car databases))
         (setq databases (list (ebib-db-get-main (car databases)))))
       (let* ((entries (ebib-read-entry "Select entries: " databases 'multiple))
              (keys (mapcar #'car entries))
              (db (cdar entries)) ; We only take the database of the first entry.
              (citation (ebib--create-citation major-mode keys db)))
         (if citation
             (insert (format "%s" citation)))
         (when dependent-db
           (dolist (key keys)
             (unless (ebib-db-has-key key dependent-db)
               (ebib-db-add-entries-to-dependent key dependent-db)
               (ebib-db-set-modified t dependent-db)))
           (when (ebib-db-modified-p dependent-db)
             (ebib--mark-index-dirty dependent-db)
             (if ebib-save-dependent-after-citation
                 (condition-case err
                     (ebib--save-database dependent-db)
                   ;; If an error occurs, make sure to mark the database as modified.
                   (error (ebib-db-set-modified t dependent-db)
                          (signal (car err) (cdr err))))
               (ebib-db-set-modified t dependent-db)))))))
    (default
      (error "[Ebib] No database opened"))))

(defun ebib-index-help ()
  "Show the info node of Ebib's index buffer."
  (interactive)
  (ebib-lower)
  (info "(ebib) The Index Buffer"))

(defun ebib-info ()
  "Show Ebib's info node."
  (interactive)
  (ebib-lower)
  (info "(ebib)"))

;; Copying stuff to the kill ring.

(eval-and-compile
  (define-prefix-command 'ebib-copy-map)
  (suppress-keymap 'ebib-copy-map 'no-digits)
  (define-key ebib-copy-map "c" #'ebib-copy-citation-as-kill)
  (define-key ebib-copy-map "e" #'ebib-copy-entry-as-kill)
  (define-key ebib-copy-map "k" #'ebib-copy-key-as-kill)
  (define-key ebib-copy-map "r" #'ebib-copy-reference-as-kill))

(defun ebib-copy-citation-as-kill ()
  "Create a citation from the current entry and copy it to the kill ring."
  (interactive)
  (kill-new (ebib--process-reference-template ebib-citation-template (ebib--get-key-at-point) ebib--cur-db))
  (message "Citation copied to kill ring."))

(defun ebib-copy-entry-as-kill ()
  "Copy the current entry.
The entry is copied to the kill ring."
  (interactive)
  (ebib--execute-when
    (entries
     (let ((key (ebib--get-key-at-point)))
       (with-temp-buffer
         (ebib--format-entry key ebib--cur-db)
         (kill-new (buffer-substring-no-properties (point-min) (point-max))))
       (message (format "Entry `%s' copied to kill ring.  Use `y' to yank (or `C-y' outside Ebib)." key))))
    (default
      (beep))))

(defalias 'ebib-copy-entry 'ebib-copy-entry-as-kill)

(defun ebib-copy-key-as-kill ()
  "Copy the key of the current entry to the kill ring."
  (interactive)
  (kill-new (ebib--get-key-at-point))
  (message "Entry key copied to kill ring."))

(defun ebib-copy-reference-as-kill ()
  "Create a reference for the current entry and copy it to the kill ring."
  (interactive)
  (ebib--execute-when
    (entries
     (let* ((key (ebib--get-key-at-point))
            (type (ebib-db-get-field-value "=type=" key ebib--cur-db 'noerror))
            (template (or (alist-get type ebib-reference-templates nil nil #'cl-equalp)
                          "{Author|Editor}, ({Date|Year}). {\"Title\".} {Doi|Url.}")))
       (kill-new (ebib--process-reference-template template key ebib--cur-db))
       (message "Reference copied to kill ring.")))))

;;; Main & dependent databases

(eval-and-compile
  (define-prefix-command 'ebib-dependent-map)
  (suppress-keymap 'ebib-dependent-map 'no-digits)
  (define-key ebib-dependent-map "c" #'ebib-dependent-create-dependent)
  (define-key ebib-dependent-map "a" #'ebib-dependent-add-entry)
  (define-key ebib-dependent-map "d" #'ebib-dependent-delete-entry)
  (define-key ebib-dependent-map "m" #'ebib-dependent-switch-to-main))

(defun ebib-dependent-create-dependent ()
  "Create a dependent database based on the current database."
  (interactive)
  (ebib--execute-when
    ((or dependent-db filtered-db)
     (error "Cannot create dependent database from another dependent or from a filtered database"))
    (real-db
     (let ((file (read-file-name "Create dependent database: "))
           (dependent (ebib--create-new-database ebib--cur-db)))
       (ebib-db-set-filename (expand-file-name file) dependent)
       (setq ebib--cur-db dependent)
       (ebib--update-buffers)))
    (default (beep))))

(defun ebib-dependent-add-entry ()
  "Add an entry from the main database to a dependent database.
When called from within a dependent database, the keys of the
entries of the main database are offered for completion.  When
called in a database that is not a dependent, this function first
checks if the database has any dependent, asking the user which
one to use if there are more than one, and then adds the current
entry or the marked entries to the dependent database."
  (interactive)
  (ebib--execute-when
    (dependent-db
     (let ((key (caar (ebib-read-entry "Entry to add to the current dependent database: " (list (ebib-db-get-main ebib--cur-db))))))
       (ebib-db-add-entries-to-dependent key ebib--cur-db)
       (ebib-db-set-current-entry-key key ebib--cur-db)
       (ebib--insert-entry-in-index-sorted key t)
       (ebib--set-modified t ebib--cur-db)
       (ebib--update-entry-buffer)))
    ((or real-db filtered-db)
     (let* ((entries (or (and (ebib-db-marked-entries-p ebib--cur-db)
                              (y-or-n-p "Add marked entries to dependent database? ")
                              (ebib-db-list-marked-entries ebib--cur-db))
                         (ebib--get-key-at-point)))
            (dependent (seq-filter (lambda (db)
                                     (eq ebib--cur-db (ebib-db-get-main db)))
                                   ebib--databases))
            (target (cond
                     ((null dependent) (error "No dependent databases associated with current database"))
                     ((= (length dependent) 1) (car dependent))
                     (t (ebib-read-database (format "Add %s to dependent database: " (if (stringp entries) "entry" "entries")) dependent)))))
       (when target
         (ebib-db-add-entries-to-dependent entries target)
         (ebib-db-set-modified t target)
         (ebib--mark-index-dirty target)
         (message "[Ebib] %s added to database `%s'." (if (stringp entries) "entry" "entries") (ebib-db-get-filename target 'short)))))))

(defun ebib-dependent-delete-entry ()
  "Delete the current entry or marked entries from a dependent database."
  (interactive)
  (ebib--execute-when
    ((and dependent-db entries)
     (let ((mark (point-marker))
           (marked-entries (ebib-db-list-marked-entries ebib--cur-db)))
       (if (and marked-entries
                (y-or-n-p "Remove all marked entries from dependent database? "))
           (progn (dolist (key marked-entries)
                    (ebib--goto-entry-in-index key)
                    (let ((inhibit-read-only t))
                      (delete-region (pos-bol) (1+ (pos-eol))))
                    (ebib-db-remove-entries-from-dependent key ebib--cur-db))
                  (ebib-db-unmark-entry marked-entries ebib--cur-db) ; This works even though we already removed the entries from the database.
                  (message "Marked entries removed."))
         (let ((key (ebib--get-key-at-point)))
           (when (y-or-n-p (format "Remove %s from depedent database? " key))
             (let ((inhibit-read-only t))
               (delete-region (pos-bol) (1+ (pos-eol))))
             (ebib-db-remove-entries-from-dependent key ebib--cur-db)
             (message "Entry `%s' removed." key))))
       (ebib--set-modified t ebib--cur-db)
       (goto-char mark)
       (if (eobp)
           (forward-line -1))
       (ebib--update-entry-buffer)))
    (default (beep))))

(defun ebib-dependent-switch-to-main ()
  "Switch to the main database of the current dependent database."
  (interactive)
  (ebib--execute-when
    (dependent-db (let ((main (ebib-db-get-main ebib--cur-db)))
                    (ebib-switch-to-database main)))
    (default (beep))))

;;; Interactive keyword functions

;; The keywords keymap

(eval-and-compile
  (define-prefix-command 'ebib-keywords-map)
  (suppress-keymap 'ebib-keywords-map 'no-digits)
  (define-key ebib-keywords-map "a" #'ebib-add-keywords-to-entry)
  (define-key ebib-keywords-map "c" #'ebib-add-all-keywords-to-canonical-list)
  (define-key ebib-keywords-map "p" #'ebib-purge-keywords-field)
  (define-key ebib-keywords-map "s" #'ebib-add-canonical-keyword)
  (define-key ebib-keywords-map "S" #'ebib-save-canonical-keywords-list))

(defun ebib--completing-read-keywords (collection)
  "Read keywords with completion from COLLECTION.
Return the keywords entered as a list.  If no keywords are
entered, the return value is nil."
  (let* ((prompt (format "Add keyword (%s to finish) [%%s]" (ebib--completion-finish-key 'completing-read))))
    (cl-loop for keyword = (completing-read (format prompt (mapconcat #'identity keywords " "))
					    collection nil nil nil 'ebib--keywords-history)
             until (string= keyword "")
             collecting keyword into keywords
             finally return keywords)))

(defun ebib-add-keywords-to-entry ()
  "Add keywords to the current entry.
If there are marked entries, the user is asked if they wish to
add keywords to all of them.  If not, the keywords are added to
the current entry."
  (interactive)
  (cl-flet ((add-keywords (entry-key keywords)
                          ;; KEYWORDS is a list of keywords to be added to entry ENTRY-KEY.
                          (let* ((conts (ebib-get-field-value "keywords" entry-key ebib--cur-db 'noerror 'unbraced))
                                 (keywords-string (mapconcat #'identity keywords ebib-keywords-separator))
                                 (new-conts (if conts
                                                (concat conts ebib-keywords-separator keywords-string)
                                              keywords-string)))
                            (ebib-set-field-value "keywords"
                                                  (if ebib-keywords-field-keep-sorted
                                                      (ebib--keywords-sort new-conts)
                                                    new-conts)
                                                  entry-key ebib--cur-db 'overwrite)
                            (ebib--maybe-add-keywords-to-canonical-list keywords))))
    (let* ((minibuffer-local-completion-map (make-composed-keymap '(keymap (32)) minibuffer-local-completion-map))
           (keywords (ebib--completing-read-keywords ebib--keywords-completion-list)))
      (when keywords
        (ebib--execute-when
          (entries
           (let ((to-be-modified (or (ebib-db-list-marked-entries ebib--cur-db)
                                     (ebib--get-key-at-point))))
             (if (and (listp to-be-modified) (y-or-n-p "Add keywords to all marked entries? "))
                 (progn
                   (dolist (entry to-be-modified)
                     (add-keywords entry keywords))
                   (message "Keywords added to marked entries."))
               (add-keywords to-be-modified keywords)
               (setq to-be-modified (list to-be-modified))) ; So we can use it in `seq-difference' below.
             (ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
                                                              (= (ebib-db-count-entries dependent)
                                                                 (length (seq-difference (ebib-db-list-keys dependent) to-be-modified))))
                                                            (ebib--list-dependents ebib--cur-db)))))
          (default
            (beep)))
        (ebib--update-entry-buffer)))))

(defun ebib-add-canonical-keyword (pfx)
  "Add a keyword in the current entry to the list of canonical keywords.
With prefix argument PFX, add all keywords to the list of
canonical keywords."
  (interactive "P")
  ;; What this really does is add the keyword to the keyword completion list
  ;; (`ebib--keywords-completion-list') and mark this list as modified.  If the
  ;; user uses a canonical keywords list, this completion list will be saved as
  ;; the canonical list at the end of the session.  Hence the name of this
  ;; function, which is technically misleading.
  (let ((keywords (ebib--keywords-to-list (ebib-get-field-value "keywords" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced))))
    (if pfx
        (mapc #'ebib--keywords-add-to-completion-list
              keywords)
      (let ((collection (seq-difference keywords ebib--keywords-completion-list)))
        (ebib--ifstring (keyword (completing-read "Add keyword to canonical list: " collection))
            (ebib--keywords-add-to-completion-list keyword)))))
  (ebib--update-entry-buffer))

(defun ebib-add-all-keywords-to-canonical-list ()
  "Add all keywords of the current entry to the list of canonical keywords."
  (interactive)
  (ebib-add-canonical-keyword t))

(defun ebib--maybe-add-keywords-to-canonical-list (keywords)
  "Conditionally add KEYWORDS to the canonical list.
If there is a list of canonical keywords and if new keywords
should be added, do so.  KEYWORDS is a list of keywords."
  ;; See comment in `ebib-add-canonical-keyword'.
  (if (and ebib-keywords ebib-keywords-add-new-to-canonical)
      (mapc #'ebib--keywords-add-to-completion-list keywords)))

(defun ebib-purge-keywords-field ()
  "Remove non-canonical keywords in the current entry.
Specifically, remove all keywords that are not on the list of
canonical keywords."
  (interactive)
  (let* ((keywords (ebib--keywords-to-list (ebib-get-field-value "keywords" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced)))
         (canonical (seq-intersection keywords ebib--keywords-completion-list)))
    (unless (= (length keywords) (length canonical))
      (ebib-db-set-field-value "keywords"
                               ;; `mapconcat' on an empty list returns an empty
                               ;; string, but if `canonical' is empty, we want to
                               ;; store nil, not "":
                               (if canonical
                                   (mapconcat #'identity canonical ebib-keywords-separator))
                               (ebib--get-key-at-point)
                               ebib--cur-db
                               'overwrite)
      (ebib--set-modified t ebib--cur-db t (ebib--list-dependents ebib--cur-db))
      (ebib--update-entry-buffer))))

(defun ebib-save-canonical-keywords-list ()
  "Save the list of canonical keywords."
  (interactive)
  (when (or (get 'ebib--keywords-completion-list :modified)
            (and (not ebib-keywords)
                 (y-or-n-p "[Ebib] Save the current list of keywords as the canonical list? ")))
    (ebib--keywords-save-canonical-list)
    (put 'ebib--keywords-completion-list :modified nil)
    (message "[Ebib] List of canonical keywords saved.")))

;;; Interactive filter functions

(defun ebib-filters-logical-and (not)
  "Filter the current database.
If the current database is filtered already, perform a logical
AND on the entries.  A negative prefix argument adds a logical
NOT to the filter."
  (interactive "p")
  (ebib--execute-when
    (entries
     (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
     (ebib--filters-create-filter 'and not)
     (ebib--update-buffers))
    (default
      (beep))))

(defun ebib-filters-logical-or (not)
  "Filter the current database.
If the current database is filtered already, perform a logical OR
on the entries.  A negative prefix argument adds a logical NOT to
the filter."
  (interactive "p")
  (ebib--execute-when
    (entries
     (ebib--filters-create-filter 'or not)
     (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
     (ebib--update-buffers))
    (default
      (beep))))

(defun ebib-filters-logical-not ()
  "Negate the current filter."
  (interactive)
  (ebib--execute-when
    (filtered-db
     (ebib-db-set-filter (if (eq (car (ebib-db-get-filter ebib--cur-db)) 'not)
                             (cadr (ebib-db-get-filter ebib--cur-db))
                           `(not ,(ebib-db-get-filter ebib--cur-db)))
                         ebib--cur-db)
     (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
     (ebib--update-buffers))
    (default
      (beep))))

(defun ebib-filters-reapply-filter ()
  "Reapply the current filter."
  (interactive)
  (ebib--execute-when
    (filtered-db
     (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
     (ebib--update-buffers))
    (default
      (error "[Ebib] No filter is active"))))

(defun ebib-filters-reapply-last-filter ()
  "Reapply the last used filter."
  (interactive)
  (ebib-db-set-filter ebib--filters-last-filter ebib--cur-db)
  (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
  (ebib--update-buffers)
  (message "Reapplied last filter"))

(defun ebib-filters-cancel-filter ()
  "Cancel the current filter."
  (interactive)
  (ebib--execute-when
    (filtered-db
     (setq ebib--filters-last-filter (ebib-db-get-filter ebib--cur-db))
     (ebib-db-set-filter nil ebib--cur-db)
     (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
     (ebib--update-buffers)
     (message "Filter cancelled"))
    (default
      (beep))))

(defun ebib-filters-toggle-crossref ()
  "Toggle inclusion of cross-referenced entries."
  (interactive)
  (setq ebib-filters-include-crossref (not ebib-filters-include-crossref))
  (ebib--update-buffers))

(defun ebib-filters-apply-filter ()
  "Select a filter and apply it to the current database."
  (interactive)
  (ebib--execute-when
    (real-db
     (let ((filter (ebib--filters-select-filter "Apply filter: ")))
       (when filter
         (ebib-db-set-filter (cadr filter) ebib--cur-db)
         (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
         (ebib--update-buffers))))
    (filtered-db
     (error "[Ebib] A stored filter can only be applied to a real database"))))

(defun ebib-list-recent (days)
  "List entries created in the last DAYS days."
  (interactive "nNumber of days: ")
  (let ((filter (ebib-db-get-filter ebib--cur-db)))
    (when filter (setq ebib--filters-last-filter filter)))
  (let* ((date (time-subtract (current-time) (days-to-time days)))
         (filter `(ebib--newer-than (quote ,date))))
    (ebib-db-set-filter filter ebib--cur-db)
    (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
    (ebib--update-buffers)))

;;; Interactive reading list functions

(eval-and-compile
  (define-prefix-command 'ebib-reading-list-map)
  (suppress-keymap 'ebib-reading-list-map 'no-digits)
  (define-key ebib-reading-list-map "a" #'ebib-add-reading-list-item)
  (define-key ebib-reading-list-map "c" #'ebib-create-reading-list)
  (define-key ebib-reading-list-map "d" #'ebib-mark-reading-list-item-as-done)
  (define-key ebib-reading-list-map "v" #'ebib-view-reading-list))

(defun ebib-create-reading-list ()
  "Create a reading list.
This function simply creates a customization buffer for the
option `ebib-reading-list-file'."
  (interactive)
  (ebib-lower)
  (customize-option 'ebib-reading-list-file))

(defun ebib-add-reading-list-item ()
  "Add the current entry to the reading list.
This function adds an entry to `ebib-reading-list-file' if it
exists and runs `ebib-reading-list-new-item-hook'."
  (interactive)
  (ebib--execute-when
    (entries
     (or ebib-reading-list-file
         ebib-reading-list-new-item-hook
         (error "[Ebib] No reading list defined"))
     (let ((key (ebib--get-key-at-point)))
       (if (ebib--reading-list-item-p key)
           (error "Entry `%s' is already on the reading list" key))
       (if (file-writable-p ebib-reading-list-file)
           (unless (ebib--reading-list-new-item key ebib--cur-db)
             (error "[Ebib] Could not create reading list item for `%s'" key))
         (error "[Ebib] Reading list file is not writable"))))
    (default
      (beep))))

(defun ebib-mark-reading-list-item-as-done ()
  "Mark the current entry as done on the reading list.
The item is removed by calling the function in
`ebib-reading-list-remove-item-function'.  After removal, the
hook `ebib-reading-list-remove-item-hook' is run."
  (interactive)
  (ebib--execute-when
    (entries
     (or ebib-reading-list-file
         ebib-reading-list-new-item-hook
         (error "[Ebib] No reading list defined"))
     (unless (file-writable-p ebib-reading-list-file)
       (error "[Ebib] Reading list file is not writable"))
     (let ((key (ebib--get-key-at-point)))
       (if (ebib--reading-list-remove-item key)
           (message "Reading list item for `%s' marked as done." key)
         (error "[Ebib] Could not locate reading list item for `%s'" key))))
    (default
      (beep))))

(defun ebib-view-reading-list ()
  "Show the reading list."
  (interactive)
  (let ((buf (ebib--reading-list-buffer)))
    (ebib-lower)
    (switch-to-buffer buf)))

;;; entry-mode

(defvar ebib-entry-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map 'no-digits)
    (define-key map [up] 'ebib-prev-field)
    (define-key map [down] 'ebib-next-field)
    (define-key map [prior] 'ebib-goto-prev-set)
    (define-key map [next] 'ebib-goto-next-set)
    (define-key map [home] 'ebib-goto-first-field)
    (define-key map [end] 'ebib-goto-last-field)
    (define-key map [return] 'ebib-edit-current-field)
    (define-key map " " 'ebib-goto-next-set)
    (define-key map "a" 'ebib-add-field)
    (define-key map "b" 'ebib-goto-prev-set)
    (define-key map "c" 'ebib-copy-current-field-contents)
    (define-key map "d" 'ebib-delete-current-field-contents)
    (define-key map "e" 'ebib-edit-current-field)
    (define-key map "f" 'ebib-view-file-in-field)
    (define-key map "g" 'ebib-goto-first-field)
    (define-key map "G" 'ebib-goto-last-field)
    (define-key map "h" 'ebib-entry-help)
    (define-key map "j" 'ebib-jump-to-field)
    (define-key map "k" 'ebib-kill-current-field-contents)
    (define-key map "K" 'ebib-keywords-map)
    (define-key map "m" 'ebib-edit-current-field-as-multiline)
    (define-key map "n" 'ebib-next-field)
    (define-key map [(control n)] 'ebib-next-field)
    (define-key map [(meta n)] 'ebib-goto-prev-set)
    (define-key map "p" 'ebib-prev-field)
    (define-key map [(control p)] 'ebib-prev-field)
    (define-key map [(meta p)] 'ebib-goto-next-set)
    (define-key map "q" 'ebib-quit-entry-buffer)
    (define-key map "r" 'ebib-toggle-raw-current-field)
    (define-key map "s" 'ebib-insert-abbreviation-current-field)
    (define-key map "u" 'ebib-browse-url)
    (define-key map "v" 'ebib-view-current-field-as-help)
    (define-key map "y" 'ebib-yank-current-field-contents)
    (define-key map "\C-xb" 'ebib-quit-entry-buffer)
    (define-key map "\C-xk" 'ebib-quit-entry-buffer)
    (define-key map "\C-x\C-s" #'ebib-save-current-database)
    map)
  "Keymap for the Ebib entry buffer.")

(define-derived-mode ebib-entry-mode
  fundamental-mode "Ebib"
  "Major mode for the Ebib entry buffer."
  (setq buffer-read-only t)
  (if ebib-hide-cursor
      (setq cursor-type nil))
  (if ebib-entry-mode-line
      (setq mode-line-format ebib-entry-mode-line))
  (setq truncate-lines t)
  (ebib-set-default-dir)
  (set (make-local-variable 'hl-line-face) 'ebib-highlight-extend-face)
  (hl-line-mode 1))

(easy-menu-define ebib-entry-menu ebib-entry-mode-map "Ebib entry menu."
  '("Ebib"
    ["Edit Field" ebib-edit-current-field t]
    ["Edit Field As Multiline" ebib-edit-current-field-as-multiline t]
    ["Insert @String Abbreviation" ebib-insert-abbreviation-current-field]
    ["Toggle Raw" ebib-toggle-raw-current-field (ebib-db-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror)]
    "--"
    ["Kill Field Contents" ebib-kill-current-field-contents (ebib-db-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror)]
    ["Copy Field Contents" ebib-copy-current-field-contents (ebib-db-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror)]
    ["Yank" ebib-yank-current-field-contents t]
    ["Delete Field Contents" ebib-delete-current-field-contents (ebib-db-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror)]
    "--"
    ["Add Field" ebib-add-field t]
    "--"
    ["View File" ebib-view-file-in-field (ebib-db-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror)]
    ["Browse URL" ebib-browse-url (ebib-db-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror)]
    ["View Multiline Field" ebib-view-current-field-as-help (ebib-db-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror)]
    "--"
    ["Quit Entry Buffer" ebib-quit-entry-buffer t]
    ["Help On Entry Buffer" ebib-entry-help t]))

(defun ebib--format-entry-info-for-modeline ()
  "Format information about the current entry for display in the mode line.
Return a string that contains the entry key, `ebib-notes-symbol'
if the current entry has a note and `ebib-reading-list-symbol' if
the current entry is on the reading list.  The latter two symbols
are enclosed in braces."
  (let* ((key (ebib--get-key-at-point))
         (info (concat (if (ebib--notes-has-note key) ebib-notes-symbol "")
                       (if (ebib--reading-list-item-p key) ebib-reading-list-symbol ""))))
    (if (not (string= info ""))
        (setq info (concat " [" info "] ")))
    (format " «%s»%s" key info)))

(defun ebib-quit-entry-buffer ()
  "Quit editing the entry.
If the key of the current entry matches the pattern
<new-entry%d>, a new key is automatically generated using
`bibtex-generate-autokey'."
  (interactive)
  (hl-line-unhighlight)
  (cond
   ((and ebib-popup-entry-window
         (eq ebib-layout 'index-only))
    (quit-window))
   ((eq ebib-layout 'index-only)
    (switch-to-buffer nil t)))
  (unless (ebib--pop-to-buffer (ebib--buffer 'index))
    (call-interactively (global-key-binding "\C-xb"))) ; If the index buffer isn't visible, call `switch-to-buffer'.
  (if (string-match-p "<new-entry[0-9]+>" (ebib--get-key-at-point))
      (ebib-generate-autokey)))

(defun ebib--current-field ()
  "Return the current field name.
The current field is simply the field that point is on.  If point
is on an empty line, return nil.  This function leaves point at
the beginning of the current line."
  (with-current-ebib-buffer 'entry
    (beginning-of-line)
    (if (bobp)                   ; If we're at the beginning of the buffer,
        "=type="                 ; the current field is `=type='.
      (unless (eolp)             ; We're not on an empty line
        (save-excursion
          (let ((beg (point))
                (end (progn
                       (skip-chars-forward "^[:space:]" (line-end-position))
                       (point))))
            (buffer-substring-no-properties beg end)))))))

(defun ebib--line-at-point ()
  "Return the type of line in the entry buffer that point is on.
If point is on an empty line between fields, return the symbol
`empty-line'.  If point is on the line below the last
field (i.e., at the end of the buffer), return `final-line'.  If
point is on a line that is part of a multi-line value (except its
first line), return `multi-line'.

If point is on a line with a field name, return nil.  This return
value makes it easier to move point in a `while' loop until it
has reached a field name.

Point should be at the beginning of the line, but this is not
checked.  This function also does not check if the entry buffer
is indeed active."
  ;; We don't call `beginning-of-line' here, because we use this function in
  ;; `while' loops, which means calling `beginning-of-line' would be wasteful.
  (cond
   ((eobp) 'final-line)
   ((eolp) 'empty-line)
   ((looking-at-p "[[:space:]]") 'multi-line)
   (t nil)))

(defun ebib-prev-field ()
  "Move to the previous field."
  (interactive)
  (if (bobp)                           ; Don't move if we're on the first field already.
      (beep)
    (forward-line -1)
    (while (ebib--line-at-point)       ; Move up to the line with the field name.
      (forward-line -1))))

(defun ebib-next-field (&optional pfx)
  "Move to the next field.
The prefix argument PFX is used to determine whether the command
was called interactively."
  (interactive "p")
  (let ((field-end (next-single-property-change (point) 'ebib-field-end)))
    (if (= (1+ field-end) (point-max))  ; If we would end up at the empty line
        (if pfx                         ; below the last field, beep.
            (beep))
      (goto-char (1+ field-end))        ; Otherwise move point.
      (while (ebib--line-at-point)      ; And see if we need to adjust.
        (forward-line 1)))))

(defun ebib-goto-first-field ()
  "Move to the first field."
  (interactive)
  (goto-char (point-min)))

(defun ebib-goto-last-field ()
  "Move to the last field."
  (interactive)
  (goto-char (point-max))
  (while (ebib--line-at-point)           ; Move up as long as we're not at a field.
    (forward-line -1)))

(defun ebib-goto-next-set ()
  "Move to the next set of fields."
  (interactive)
  (beginning-of-line)
  (let ((p (point)))
    (while (not (eolp))              ; Search for the first empty line.
      (forward-line))
    (if (not (= (forward-line) 0))   ; If we cannot move to the next line,
        (goto-char p))))             ; go back to where we started.

(defun ebib-goto-prev-set ()
  "Move to the previous set of fields."
  (interactive)
  (beginning-of-line)
  (if (bobp)                  ; If we're at the =type= field, we don't move.
      (beep)
    ;; First move up to the next-higher empty line.
    (while (not (eq (ebib--line-at-point) 'empty-line))
      (forward-line -1))
    ;; Then move to the field name.
    (while (ebib--line-at-point)
      (forward-line -1))))

(defun ebib-jump-to-field ()
  "Read a field from the user and jump to it."
  (interactive)
  (when-let ((entry-type (ebib-get-field-value "=type=" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced))
             (fields (ebib--list-fields entry-type 'all))
             (field (completing-read "Jump to field: " fields nil t))
             (location (save-excursion
                         (goto-char (point-min))
                         (re-search-forward (concat "^" field) nil t))))
    (if location
	(goto-char location)
      (beginning-of-line))))

(defun ebib-add-field (&optional default)
  "Prompt with completion for a field name and add it to current entry.
Completion candidates are all fields ebib knows about for the
current dialect, less all fields in the current entry.

If DEFAULT is specified, it is the initial input for the prompt."
  (interactive)
  (let* ((key (ebib--get-key-at-point))
	 (db-fields (seq-mapcat (lambda (type) (ebib--list-fields type 'all (ebib--get-dialect ebib--cur-db)))
				(ebib--list-entry-types)))
	 (current-entry-fields (mapcar 'car (ebib-db-get-entry key ebib--cur-db)))
	 (candidate-fields (delete-dups
			    (seq-difference db-fields current-entry-fields)))
	 (field (completing-read "Field: " candidate-fields nil nil default)))
    (if (ebib-get-field-value field key ebib--cur-db 'noerror)
        (error "[Ebib] Field `%s' already has a value in entry `%s'" field key)
      ;; We store the field with an empty string as value and then let the user
      ;; edit it. (We cannot pass nil as a value because
      ;; `ebib--update-entry-buffer' ignores hidden or undefined fields with a
      ;; nil value.
      (ebib-set-field-value field "" key ebib--cur-db 'noerror)
      (ebib--update-entry-buffer)
      (re-search-forward (concat "^" field))
      (condition-case err
          (ebib-edit-current-field)
        (quit (ebib-set-field-value field nil key ebib--cur-db 'overwrite)  ; If the user quits the edit, we must remove the field again.
              (ebib--update-entry-buffer))
        (error (signal (car err) (cdr err)))))))

(defun ebib--edit-file-field (_ _ init-contents)
  "Edit the \"file\" field'.
Completing-read-multiple over filenames. Filenames are added to
the standard file field separated by `ebib-filename-separator'.
The first directory in `ebib-file-search-dirs' is used as the
start directory. If `ebib-truncate-file-names' is t, file names
are truncated relative to the directories listed in
`ebib-file-search-dirs', otherwise they are stored as absolute
paths.

Ignored first two arguments for compatibility with the format of
`ebib-field-edit-functions'. INIT-CONTENTS is used as the initial
input to `completing-read-multiple'. Consequently, entries are
separated in the completion by `ebib-filename-separator'."
  (let* ((crm-local-must-match-map (make-composed-keymap '(keymap (32)) crm-local-must-match-map))
         (crm-separator ebib-filename-separator)
         (key (ebib--get-key-at-point))
	 (default-directory (file-name-as-directory (car ebib-file-search-dirs)))
	 (list (completing-read-multiple
		(format "File(s) for %s: " key)
		#'completion-file-name-table
		nil 'require-match
		(when init-contents
		  (concat init-contents ebib-filename-separator))))
	 (string (string-join (mapcar #'ebib--transform-file-name-for-storing list)
			      ebib-filename-separator))
	 ;; `ebib-filename-separator', but without trailing whitespace
	 (to-trim (string-trim ebib-filename-separator nil "[[:space:]]+")))
    ;; Return the string, but with `to-trim', and any trailing
    ;; whitespace, removed. Removing whitespace above and declaring it
    ;; here ensures that it is matched with "*". This accounts for
    ;; cases where `ebib-filename-separator' is set to "; ", but there
    ;; is a trailing ";" on the final value, without whitespace
    (string-trim string nil (concat to-trim "[[:space:]]*"))))

(defun ebib--transform-file-name-for-storing (file)
  "Return a name for FILE that can be stored in the file field.
If `ebib-truncate-file-names' is non-nil, the name is truncated
relative to `ebib-file-search-dirs'.  Subsequently,
`ebib-file-name-mod-function' is applied to the name."
  (setq file (if ebib-truncate-file-names
                 (ebib--file-relative-name file)
               file))
  (funcall ebib-file-name-mod-function file t))

(defun ebib--file-relative-name (file)
  "Return a name for FILE relative to `ebib-file-search-dirs'.
If FILE is not in (a subdirectory of) one of the directories in
`ebib-file-search-dirs', return FILE."
  ;; We first create a list of names relative to each dir in
  ;; `ebib-file-search-dirs', discarding those that start with `..'
  (let* ((names (delq nil (mapcar (lambda (dir)
                                    (let ((rel-name (file-relative-name file dir)))
                                      (unless (string-prefix-p ".." rel-name)
                                        rel-name)))
                                  ebib-file-search-dirs)))
         ;; Then we take the shortest one...
         (name (car (sort names (lambda (x y)
                                  (< (length x) (length y)))))))
    ;; ...and return it, or the filename itself if it couldn't be
    ;; relativized.
    (or name file)))

(defun ebib--create-author/editor-collection ()
  "Create a collection from authors and editors."
  (seq-uniq (delq nil (apply #'seq-concatenate 'list (mapcar (lambda (entry)
                                                               (split-string (ebib-unbrace (or (cdr (assoc-string "author" entry 'case-fold))
                                                                                               (cdr (assoc-string "editor" entry 'case-fold))
                                                                                               ""))
                                                                             "[[:space:]]+\\(and\\|AND\\)[[:space:]]+" t))
                                                             (apply #'seq-concatenate 'list (mapcar (lambda (db)
                                                                                                      (hash-table-values (ebib-db-val 'entries db)))
                                                                                                    (seq-filter (lambda (db)
                                                                                                                  (not (ebib-db-dependent-p db)))
                                                                                                                ebib--databases))))))))

(defun ebib--edit-author/editor-field (field)
  "Edit the author or editor field.
FIELD should be \"Author\" or \"Editor\".  Offer completion on
all other authors and editors in all databases."
  ;; We shadow the binding of `minibuffer-local-completion-map' so that we
  ;; can unbind <SPC>, since authors and editors contain spaces.
  (let ((minibuffer-local-completion-map (make-composed-keymap '(keymap (32)) minibuffer-local-completion-map))
        (collection (ebib--create-author/editor-collection))
        (prompt (format "Add a new %s (%s to finish): " field (ebib--completion-finish-key 'completing-read)))
        (key (ebib--get-key-at-point)))
    (cl-loop for author = (completing-read prompt collection)
             until (string= author "")
             do (let* ((conts (ebib-get-field-value field key ebib--cur-db 'noerror 'unbraced))
                       (new-conts (if conts
                                      (concat conts " and " author)
                                    author)))
                  (ebib-set-field-value field new-conts key ebib--cur-db 'overwrite)
                  (ebib--redisplay-current-field)
                  (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                                     (ebib-db-has-key key dependent))
                                                                   (ebib--list-dependents ebib--cur-db))))
             finally return (ebib-db-modified-p ebib--cur-db))))

(defun ebib--create-collection-from-field (field)
  "Create a collection from the contents of FIELD."
  (seq-uniq (delq nil (mapcar (lambda (entry)
                                (ebib-unbrace (cdr (assoc-string field entry 'case-fold))))
                              (apply #'seq-concatenate 'list (mapcar (lambda (db)
                                                                       (hash-table-values (ebib-db-val 'entries db)))
                                                                     (seq-filter (lambda (db)
                                                                                   (not (ebib-db-dependent-p db)))
                                                                                 ebib--databases)))))))

(defun ebib--create-collection-from-fields (fields)
  "Create a collection from contents of a list of FIELDS."
  (seq-uniq (apply 'append (mapcar #'ebib--create-collection-from-field
				   fields))))

(defun ebib--edit-field-literally (field init-contents &optional complete)
  "Edit a field as a string.
FIELD is the field being edited, INIT-CONTENTS is its initial
contents.  If COMPLETE is non-nil, offer completion, the list of
completion candidates being composed of the contents of FIELD in
all entries of the database.  If COMPLETE is nil, just ask for a
string in the minibuffer.  Return the resulting string."
  (setq init-contents (ebib-unbrace init-contents))
  (if complete
      (let ((minibuffer-local-completion-map (make-composed-keymap '(keymap (32)) minibuffer-local-completion-map)))
        (completing-read (format "%s: " field)
                         (ebib--create-collection-from-field field)
                         nil nil
                         (if init-contents
                             (cons init-contents 0))))
    (read-string (format "%s: " field)
                 (if init-contents
                     (cons init-contents 0)))))

(defun ebib-edit-field (field &optional interactive)
  "Edit FIELD of a BibTeX entry.
To obtain the value from the user, this function calls out to one
of a set of helper functions that provide completion relevant to
the field being edited.  See `ebib-field-edit-functions' for
details.

INTERACTIVE indicates whether this function should be considered
to be called interactively.  If non-nil, point is moved to the
next field when the edit is done.

This function also checks the value of `current-prefix-arg': if
it is non-nil, FIELD is edited directly, without
completion (with the exception of the `=type=' field, which
always uses completion)."
  (let* ((key (ebib--get-key-at-point))
	 (pfx current-prefix-arg)
         (init-contents (ebib-get-field-value field key ebib--cur-db 'noerror))
         (result (cond
                  ((string= field "=type=")
		   (let ((rtn (ebib--edit-type-field)))
		     ;; `ebib--edit-type-field' can change the current
		     ;; key, so update it
		     (setq key (ebib--get-key-at-point))
		     rtn))
                  ((ebib--multiline-p init-contents)
                   (ebib--edit-field-as-multiline field)
                   ;; A multiline edit differs from the other fields, because
                   ;; the edit isn't done when `ebib--edit-field-as-multiline'
                   ;; returns. This means we cannot move to the next field.  For
                   ;; this reason, we return `nil', so we know below not to take
                   ;; any further action.
                   nil)
                  ;; A prefix argument means the user wants to edit the field as
                  ;; a string, without any form of completion.
                  (pfx
                   (ebib--edit-field-literally field init-contents))
		  ((when-let ((data (assoc field ebib-field-edit-functions
					   (lambda (a b) (member-ignore-case b a)))))
		     (funcall (cdr data) field (car data) (ebib-unbrace init-contents))))
                  ;; An external note is shown in the field "external note", but
                  ;; `ebib--current-field' only reads up to the first space, so
                  ;; it just returns "external".
                  ((string= field "external")
                   (ebib-popup-note (ebib--get-key-at-point))
                   nil) ; See above.
                  ;; The catch-all edits a field without completion.
                  (t (ebib--edit-field-literally field init-contents)))))
    ;; When the edit returns, see if we need to move to the next field and
    ;; check whether we need to update the index display.
    (when result
      (if (and (stringp result)
               (not (string-empty-p result)))
	  (ebib-set-field-value field result key ebib--cur-db 'overwrite (ebib-unbraced-p init-contents))
	(ebib-db-remove-field-value field key ebib--cur-db))
      (ebib--redisplay-field field)
      (ebib--redisplay-index-item field)
      (when interactive (ebib-next-field))
      (ebib--set-modified t ebib--cur-db t
			  (seq-filter (lambda (dependent)
                                        (ebib-db-has-key key dependent))
                                      (ebib--list-dependents ebib--cur-db))))))

(defun ebib--edit-literal-field (field-name fields init-contents &optional &rest extra-tables)
  "Edit a \"literal\" type field.
FIELD-NAME is the name of the field being edited.  FIELDS is a
list of fields from which to pull completion candidates.
INIT-CONTENTS is the original value of the field.  See also
`ebib-field-edit-functions'.

EXTRA-TABLES are extra completion tables to be included in the
candidate list, alongside the candidates generated from FIELDS.
This argument is useful for fields which have a canonical
list of candidates, like \"pubstate\".

See also `ebib-field-edit-functions'."
  (completing-read
   (format "%s: " field-name)
   (apply #'completion-table-merge
	  `(,(ebib--create-collection-from-fields fields)
	    ,@extra-tables))
   (lambda (str) (not (string-empty-p str))) ;; predicate
   nil					;; require-match
   init-contents))                      ;; initial-input

(defun ebib--edit-editor-type-field (field-name fields init-contents)
  "Edit \"editor-type\" field.
Arguments FIELD-NAME, FIELDS AND INIT-CONTENTS are as in
`ebib--edit-literal-field'."
  (ebib--edit-literal-field
   field-name fields init-contents
   '("editor" "compiler" "founder" "continuator"
     "redactor" "reviser" "collaborator" "organizer")))

(defconst ebib--biblatex-language-keys
  '("bulgarian" "catalan" "croatian" "czech" "danish" "dutch"
    "american" "USenglish" "english" "british" "UKenglish"
    "canadian" "australian" "newzealand" "estonian" "finnish"
    "french" "german" "austrian" "swissgerman" "ngerman"
    "naustrian" "nswissgerman" "greek" "magyar" "hungarian"
    "icelandic" "italian" "latvian" "norsk" "nynorsk" "polish"
    "brazil" "portuguese" "portuges" "russian" "slovak"
    "slovene" "slovenian" "spanish" "swedish" "ukrainian")
  "List of language IDs used in BibLaTeX.")

(defun ebib--edit-lang-id-field (field-name fields init-contents)
  "Edit \"lang-id\" field.
Arguments FIELD-NAME, FIELDS AND INIT-CONTENTS are as in
`ebib--edit-literal-field'."
  (ebib--edit-literal-field
   field-name fields init-contents ebib--biblatex-language-keys))

(defun ebib--edit-pagination-field (field-name fields init-contents)
  "Edit \"pagination\" field.
Arguments FIELD-NAME, FIELDS AND INIT-CONTENTS are as in
`ebib--edit-literal-field'."
  (ebib--edit-literal-field
   field-name fields init-contents
   '("page" "column" "line" "verse" "section" "paragraph")))

(defun ebib--edit-pubstate-field (field-name fields init-contents)
  "Edit \"pubstate\" field.
Arguments FIELD-NAME, FIELDS AND INIT-CONTENTS are as in
`ebib--edit-literal-field'."
  (ebib--edit-literal-field
   field-name fields init-contents
   '("inpreparation" "submitted"
     "forthcoming" "inpress" "prepublished")))

(defun ebib--read-ref-as-entry (prompt databases &optional multiple filter)
  "Read entry keys from user.
Arguments PROMPT and DATABASES are as in `ebib-read-entry',
except that `ebib-citation-insert-multiple' is ignored when
reading multiple candidates.

If MULTIPLE is nil, the key is returned as a string, If non-nil,
all selected keys are returned as a list of strings.

FILTER is passed unmodified to `ebib-read-entry'."
  (let* ((ebib-citation-insert-multiple t)
	 (entry (ebib-read-entry
		 (format "%s: " prompt) databases
		 multiple filter)))
    (if multiple
	(mapcar #'car entry)
      (caar entry))))

(defcustom ebib-always-prompt-for-special-keys t
  "Prompt for new key in already-named Xdata, Set entries."
  :group 'ebib
  :type 'boolean)

(defun ebib--edit-type-field ()
  "Prompt for an entry type.
If the selected type is \"xdata\" or \"set\" (or a
case-insensitive variation) and
`ebib-always-prompt-for-special-keys' is non-nil or the original
key is temporary or then also invoke `ebib-edit-keyname', to
force the user to select a key for the entry manually."
  (let ((type (completing-read
	       "Type: "
	       (ebib--list-entry-types (ebib--get-dialect ebib--cur-db) t)
	       nil t)))
    (when (and (member-ignore-case type '("xdata" "set"))
	       (or ebib-always-prompt-for-special-keys
		   (save-match-data
		     (string-match "<new-entry[[:digit:]]+>"
				   (ebib--get-key-at-point)))))
      (ebib-edit-keyname))
    type))

(defun ebib--edit-ref-field (field-name _ _)
  "Edit an \"entry key\" type field.
FIELD-NAME is the name of the field being edited.  Key is read
with `ebib--read-ref-as-entry'.

The other two argument places are for compatibility with
`ebib-field-edit-functions', and are ignored."
  (ebib--read-ref-as-entry (format "%s: " field-name) `(,ebib--cur-db)))

(defun ebib--edit-ref-list-field (field-name _ _)
  "Read multiple entry keys, returning a comma-separated list.
FIELD-NAME is the name of the field being edited.  Key is read
with `ebib--read-ref-as-entry'.

The other two argument places are for compatibility with
`ebib-field-edit-functions', and are ignored."
  (let ((list (ebib--read-ref-as-entry
	       field-name `(,ebib--cur-db) 'multiple
	       ;; If editing an xdata field, limit to only @XData
	       ;; entries, by using a filtered db
	       (when (cl-equalp field-name "xdata")
		 (lambda (key _ db)
		   (cl-equalp
		    (ebib-db-get-field-value "=type=" key db)
		    "xdata"))))))
    ;; NOTE When used with BibLaTeX, this assumes that xsvsep is set to
    ;; "\s*,\s*", equivalent to "[[:space:]]*,[[:space:]]" in Emacs.
    (string-join list ", ")))

(defun ebib--edit-list-field (field-name fields init-contents &optional &rest extra-tables)
  "Edit a \"name list\" or \"literal list\" type field.
FIELD-NAME is the name of the field being edited.  FIELDS is a
list of fields from which to pull completion candidates.
INIT-CONTENTS is the original value of the field.  In name- and
literal-list fields, this is an \"and\"-delimited list of values,
as a single string.

EXTRA-TABLES are extra completion tables to be included in the
candidate list, alongside the candidates generated from FIELDS.
This argument is useful for list fields which have a canonical
list of candidates, like \"language\".

See also `ebib-field-edit-functions'."
  (let* ((crm-local-completion-map (make-composed-keymap '(keymap (32)) crm-local-completion-map))
         (crm-char ";")
	 (crm-separator (format "[[:space:]]+*%s[[:space:]]+*" crm-char))
	 (and-regexp "[[:space:]]+and[[:space:]]+")
	 (collection
	  ;; Account for fields containing more than one entry, e.g.
	  ;; author = {Bar, Foo and Qux, Baz}
	  (delete-dups
	   (apply
	    #'append
	    (mapcar (lambda (str) (split-string str and-regexp t "[[:space:]]"))
		    (ebib--create-collection-from-fields fields)))))
	 (table (apply #'completion-table-merge `(,collection ,@extra-tables)))
	 (result (completing-read-multiple
		  (format "%s: " field-name)           ;; prompt
		  table                                ;; collection
		  (lambda (str) (not (string-empty-p str))) ;; predicate
		  nil                                  ;; require-match
		  ;; Replace all instances of " and " (Bib(La)TeX's
		  ;; list separator) in initial contents with " ; "
		  ;; (our list separator). Then append a crm-separator
		  ;; to the result, so that `completing-read-multiple'
		  ;; treats it as a list of selected candidates
		  (when init-contents
		    (concat
		     (replace-regexp-in-string
		      and-regexp (format " %s " crm-char)
		      init-contents)
		     crm-char)))))
    (string-join result " and ")))

(defun ebib--edit-language-field (field-name fields init-contents)
  "Edit \"language\" field.
Arguments FIELD-NAME, FIELDS AND INIT-CONTENTS are as in
`ebib--edit-list-field'."
  (ebib--edit-list-field
   field-name fields init-contents
   ebib--biblatex-language-keys))

(defun ebib--edit-separated-values-field (field-name fields init-contents)
  "Edit a \"separated values\" type field.
FIELD-NAME is the name of the field being edited.  FIELDS is a
list of fields from which to pull completion candidates.
INIT-CONTENTS is the original value of the field and should be a
Biblatex list (i.e., a string consisting of comma-separated
values).

See also `ebib-field-edit-functions'."
  ;; NOTE When used with BibLaTeX, this assumes that xsvsep is set to
  ;; "\s*,\s*", equivalent to "[[:space:]]*,[[:space:]]" in Emacs.
  (let* ((crm-local-completion-map (make-composed-keymap '(keymap (32)) crm-local-completion-map))
         (crm-separator "[[:space:]]*,[[:space:]]*")
         ;; If we're editing the "keywords" field, the completion candidates are
         ;; taken from `ebib--keywords-completion-list'. Otherwise, we collect
         ;; all the values for the current field in the currently open
         ;; databases. Since the field values are actually (Biblatex) list, we
         ;; need to split them on commas.))
	 (collection (if (string= field-name "keywords")
                         ebib--keywords-completion-list
	               (delete-dups
	                (apply
	                 #'append
	                 (mapcar (lambda (str)
		                   (split-string str crm-separator t "[[:space:]]"))
		                 (ebib--create-collection-from-fields fields))))))
	 (result (completing-read-multiple
		  (format "%s: " field-name) collection nil nil
		  ;; Append a crm-separator to the result, so that
		  ;; `completing-read-multiple' treats it as a list of
		  ;; selected candidates
		  (when init-contents (concat init-contents ", ")))))
    (string-join result ", ")))

(defun ebib--edit-TeX-code-field (field-name _fields _init-contents)
  "Edit FIELD-NAME in a multline `tex-mode' buffer.
_FIELDS and _INIT-CONTENTS are ignored, because
`ebib--edit-field-as-multiline' ignores them."
  (let ((ebib-multiline-major-mode 'tex-mode))
    (ebib--edit-field-as-multiline field-name)))

(defun ebib-edit-current-field ()
  "Edit current field of a BibTeX entry.
Most fields are edited directly using the minibuffer, but a few
are handled specially: the `type' `crossref', `xref' and
`related' fields offer completion, the `annote', `annotation' and
`abstract' fields are edited as multiline fields, the `keywords'
field adds keywords one by one, also allowing completion, and the
\"file\" field uses filename completion and shortens filenames if
they are in (a subdirectory of) one of the directories in
`ebib-file-search-dirs'.

With a prefix argument, edit the current field directly,
without any special treatment.  Exceptions are the \"type\" field
and any field with a multiline value, which are always edited in
a special manner."
  (interactive)
  (ebib-edit-field (ebib--current-field) t))

(defun ebib--redisplay-index-item (field)
  "Redisplay current index item if FIELD is being displayed."
  (if (or (assoc-string field ebib-index-columns t)
          (and (member-ignore-case field '("Author" "Editor"))
               (assoc-string "Author/Editor" ebib-index-columns t))
          (and (cl-equalp field "Date")
               (assoc-string "Year" ebib-index-columns t)))
      (with-current-ebib-buffer 'index
        (let ((key (ebib--get-key-at-point))
              (inhibit-read-only t))
          (delete-region (pos-bol) (1+ (pos-eol)))
          (ebib--insert-entry-in-index-sorted key t)))))

(defun ebib-view-file-in-field (arg)
  "View a file in the current field.
The field may contain multiple filenames, in which case the
prefix argument ARG can be used to specify which file is to be
viewed."
  (interactive "P")
  (let ((file (ebib-get-field-value (ebib--current-field) (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref))
        (num (if (numberp arg) arg nil)))
    (ebib--call-file-viewer (ebib--select-file file num (ebib--get-key-at-point)))))

(defun ebib-copy-field-contents (field)
  "Copy the contents of FIELD to the kill ring.
If the field contains a value from a cross-referenced entry, that
value is copied to the kill ring."
    (unless (or (not field)
                (string= field "=type="))
      (let ((contents (ebib-get-field-value field (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)))
        (if (stringp contents)
            (progn (kill-new contents)
                   (message "Field contents copied."))
          (error "Cannot copy an empty field")))))

(defun ebib-copy-current-field-contents ()
  "Copy the contents of the current field to the kill ring.
If the field contains a value from a cross-referenced entry, that
value is copied to the kill ring."
  (interactive)
  (ebib-copy-field-contents (ebib--current-field)))

(defun ebib-delete-field-contents (field &optional kill)
  "Delete the contents of FIELD in the current entry.
If KILL is non-nil, put the deleted text in the kill ring.

Note that if FIELD contains a value from a cross-referenced
entry, it is never killed."
  (if (or (not field)
          (string= field "=type="))
      (beep)
    (let* ((external (string= field "external"))
	   (key (ebib--get-key-at-point))
	   (contents (if external
			 (string-join (ebib--extract-note-text key nil) "\n")
		       (ebib-get-field-value field key ebib--cur-db 'noerror 'unbraced 'xref)))
	   (action-string (if kill "kill" "delete"))
	   (thing-string (if external "note" "field contents")))
      (cond
       ((and (stringp contents)
             (stringp (get-text-property 0 'ebib--xref contents)))
	(error "[Ebib] Cannot %s a cross-referenced field value" action-string))
       ((and (stringp contents) (or kill (y-or-n-p (format "Delete %s? " thing-string))))
	(when kill (kill-new contents))
	(if external
	    (if (eq ebib-notes-storage 'one-file-per-note)
		(progn
		  (delete-file (expand-file-name (ebib--create-notes-file-name key)))
		  (delete key ebib--notes-list))
	      (error "[Ebib] `ebib-notes-storage' set to `%s', deleting not supported.
Try opening the note file and deleting it manually" ebib-notes-storage))
	  (ebib-db-remove-field-value field key ebib--cur-db))
	;; Applicable for external or otherwise
	(ebib--redisplay-field field)
	(ebib--redisplay-index-item field)
	(ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
							 (ebib-db-has-key key dependent))
						       (ebib--list-dependents ebib--cur-db)))
	(message "%s %s" (capitalize thing-string) (if kill "killed" "deleted")))
       (t (error "[Ebib] Cannot %s an empty field" action-string))))))

(defun ebib-kill-current-field-contents ()
  "Kill the contents of the current field.
The killed text is put in the kill ring.  If the field contains a
value from a cross-referenced entry, it is not killed."
  (interactive)
  (ebib-delete-field-contents (ebib--current-field) 'kill))

(defun ebib-delete-current-field-contents ()
  "Delete the contents of the current field.
The deleted text is not put in the kill ring.  If the field
contains a value from a cross-referenced entry, it is not
deleted."
  (interactive)
  (ebib-delete-field-contents (ebib--current-field) nil))

(defun ebib-yank-field-contents (field)
  "Insert the last killed text into FIELD.
Repeated calls to this function cycle through the kill ring,
similar to \\[yank] followed by \\[yank-pop].  The prefix
argument ARG functions as with \\[yank] / \\[yank-pop]."
  (let ((arg current-prefix-arg)
	(key (ebib--get-key-at-point)))
    (if (or (member-ignore-case field '("=type=" "crossref")) ; We cannot yank into the `=type=' or `crossref' fields.
            (unless (eq last-command 'ebib-yank-current-field-contents) ; Nor into a field already filled.
              (ebib-get-field-value field key ebib--cur-db 'noerror)))
        (progn
          (setq this-command t)
          (beep))
      (let ((new-contents (current-kill (cond
                                         ((listp arg)
                                          (if (eq last-command 'ebib-yank-current-field-contents) 1 0))
                                         ((eq arg '-) -2)
                                         (t (1- arg))))))
        (when new-contents
          (ebib-set-field-value field new-contents key ebib--cur-db 'overwrite)
	  (ebib--redisplay-field field)
          (ebib--redisplay-index-item field)
          (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                             (ebib-db-has-key key dependent))
                                                           (ebib--list-dependents ebib--cur-db))))))))

(defun ebib-yank-current-field-contents ()
  "Insert the last killed text into the current field."
  (interactive)
  (ebib-yank-field-contents (ebib--current-field)))

(defun ebib-toggle-raw (field)
  "Toggle the \"special\" status of FIELD's contents."
  (let ((key (ebib--get-key-at-point)))
    (unless (member-ignore-case field '("=type=" "crossref" "xref" "related" "keywords"))
      (let ((contents (ebib-get-field-value field key ebib--cur-db 'noerror)))
        (if (ebib--multiline-p contents) ; Multiline fields cannot be raw.
            (beep)
          (unless contents            ; If there is no value, the user can enter one,
            (ebib-edit-field field)   ; which we must then store unbraced.
            (setq contents (ebib-get-field-value field key ebib--cur-db 'noerror)))
          (when contents ; We must check to make sure the user entered some value.
            (ebib-set-field-value field contents key ebib--cur-db 'overwrite (not (ebib-unbraced-p contents)))
            (ebib--redisplay-field field)
            (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                               (ebib-db-has-key key dependent))
                                                             (ebib--list-dependents ebib--cur-db)))))))))

(defun ebib-toggle-raw-current-field ()
  "Toggle the \"special\" status of the current field contents."
  (interactive)
  (ebib-toggle-raw (ebib--current-field)))

(defun ebib--edit-field-as-multiline (field &optional _fields _init-contents)
  "Edit a field in multiline-mode.
FIELD is the field being edited.

_FIELDS and _INIT-CONTENTS are ignored.  They are included as
arguments for compatibility with `ebib-field-edit-functions'."
  (let ((init-contents
	 (ebib-get-field-value field (ebib--get-key-at-point) ebib--cur-db 'noerror)))
    (cond
     ((member-ignore-case field '("=type=" "crossref" "xref" "related"))
      (error "[Ebib] Cannot edit `%s' field as multiline" field))
     ((ebib-unbraced-p init-contents)
      (error "[Ebib] Cannot edit a raw field as multiline"))
     (t (ebib--multiline-edit
	 (list 'field (ebib-db-get-filename ebib--cur-db) (ebib--get-key-at-point) field)
	 (ebib-unbrace init-contents))))))

(defun ebib-edit-current-field-as-multiline ()
  "Edit current field in multiline-mode."
  (interactive)
  (ebib--edit-field-as-multiline (ebib--current-field)))

(defun ebib-insert-abbreviation (field)
  "Insert an abbreviation into FIELD from the ones defined in the database."
  (let ((key (ebib--get-key-at-point)))
    (if (ebib-get-field-value field key ebib--cur-db 'noerror)
        (beep)
      (let ((strings (ebib-db-list-strings ebib--cur-db)))
        (when strings
          (with-selected-window (get-buffer-window (ebib--buffer 'index))
            (let ((string (completing-read "Abbreviation to insert: " strings nil t)))
              (when string
                (ebib-set-field-value field string key ebib--cur-db 'overwrite 'unbraced)
                (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                                   (ebib-db-has-key key dependent))
                                                                 (ebib--list-dependents ebib--cur-db))))))
          (ebib--redisplay-field field)
          (ebib-next-field))))))

(defun ebib-insert-abbreviation-current-field ()
  "Insert an abbreviation into current field from the ones defined the database ."
  (interactive)
  (ebib-insert-abbreviation (ebib--current-field)))

(defun ebib-view-field-as-help (field)
  "Show the contents of the FIELD in a *Help* window."
  (let ((help-window-select t))                          ; Make sure the help window is selected.
    (with-help-window (help-buffer)
      (princ (propertize (format "%s" field) 'face '(:weight bold)))
      (princ "\n\n")
      (let ((contents (ebib-get-field-value field (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced)))
        (if contents
            (princ contents)
          (princ "[Empty field]"))))))

(defun ebib-view-current-field-as-help ()
  "Show the contents of the current field in a *Help* window."
  (interactive)
  (ebib-view-field-as-help (ebib--current-field)))

(defun ebib-entry-help ()
  "Show the info node for Ebib's entry buffer."
  (interactive)
  (ebib-lower)
  (info "(ebib) The Entry Buffer"))

;;; strings-mode

(defvar ebib-strings-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map 'no-digits)
    (define-key map [up] 'ebib-prev-string)
    (define-key map [down] 'ebib-next-string)
    (define-key map [prior] 'ebib-strings-page-up)
    (define-key map [next] 'ebib-strings-page-down)
    (define-key map [home] 'ebib-goto-first-string)
    (define-key map [end] 'ebib-goto-last-string)
    (define-key map " " 'ebib-strings-page-down)
    (define-key map "a" 'ebib-add-string)
    (define-key map "b" 'ebib-strings-page-up)
    (define-key map "c" 'ebib-copy-string-contents)
    (define-key map "d" 'ebib-delete-string)
    (define-key map "e" 'ebib-edit-string)
    (define-key map "g" 'ebib-goto-first-string)
    (define-key map "G" 'ebib-goto-last-string)
    (define-key map "h" 'ebib-strings-help)
    (define-key map "n" 'ebib-next-string)
    (define-key map [(control n)] 'ebib-next-string)
    (define-key map [(meta n)] 'ebib-strings-page-down)
    (define-key map "p" 'ebib-prev-string)
    (define-key map [(control p)] 'ebib-prev-string)
    (define-key map [(meta p)] 'ebib-strings-page-up)
    (define-key map "q" 'ebib-quit-strings-buffer)
    (define-key map "r" 'ebib-toggle-raw-string)
    (define-key map "x" 'ebib-export-string)
    (define-key map "X" 'ebib-export-all-strings)
    (define-key map "\C-xb" 'ebib-quit-strings-buffer)
    (define-key map "\C-xk" 'ebib-quit-strings-buffer)
    (define-key map "\C-x\C-s" #'ebib-save-current-database)
    map)
  "Keymap for the ebib strings buffer.")

(define-derived-mode ebib-strings-mode
  fundamental-mode "Ebib-strings"
  "Major mode for the Ebib strings buffer."
  (setq buffer-read-only t)
  (if ebib-hide-cursor
      (setq cursor-type nil))
  (setq truncate-lines t)
  (ebib-set-default-dir)
  (set (make-local-variable 'hl-line-face) 'ebib-highlight-extend-face)
  (hl-line-mode 1))

(easy-menu-define ebib-strings-menu ebib-strings-mode-map "Ebib strings menu."
  '("Ebib"
    ["Add @String" ebib-add-string t]
    ["Edit @String" ebib-edit-string t]
    ["Toggle @String raw" ebib-toggle-raw-string t]
    ["Copy @String" ebib-copy-string-contents (ebib--current-string)]
    ["Delete @String" ebib-delete-string (ebib--current-string)]
    "--"
    ["Export @String" ebib-export-string (ebib--current-string)]
    ["Export All @Strings" ebib-export-all-strings (ebib--current-string)]
    "--"
    ["Quit @Strings Buffer" ebib-quit-strings-buffer t]
    ["Help On @Strings Buffer" ebib-strings-help t]))

(defun ebib-quit-strings-buffer ()
  "Quit editing the @String definitions."
  (interactive)
  (if (eq ebib-layout 'index-only)
      (if ebib-popup-entry-window
          (quit-window)
        (with-ebib-window-nondedicated (selected-window)
          (switch-to-buffer nil t)))
    (with-ebib-window-nondedicated (selected-window)
      (switch-to-buffer (ebib--buffer 'entry) t)
      (ebib--update-entry-buffer)))  ; Make sure changes made by the user show up.
  (unless (ebib--pop-to-buffer (ebib--buffer 'index))
    (call-interactively (global-key-binding "\C-xb"))))  ; If the index buffer isn't visible, call `switch-to-buffer'.

(defun ebib--current-string ()
  "Return the currently selected string.
The current string is simply the string that point is on.  If
point is on an empty line (e.g., when there are no @String
definitions), return nil.  This function leaves point at the
beginning of the current line."
  (with-current-ebib-buffer 'strings
    (beginning-of-line)
    (unless (eolp)
      (save-excursion
        (let ((beg (point))
              (end (progn
                     (skip-chars-forward "a-zA-Z" (line-end-position))
                     (point))))
          (buffer-substring-no-properties beg end))))))

(defun ebib-prev-string ()
  "Move to the previous string."
  (interactive)
  (if (= (forward-line -1) -1)
      (beep))) ; We're at the first line already.

(defun ebib-next-string ()
  "Move to the next string."
  (interactive)
  (forward-line)
  (when (eobp)        ; If we've ended up on the empty line after the last string
    (forward-line -1) ; go back and beep.
    (beep)))

(defun ebib-goto-first-string ()
  "Move to the first string."
  (interactive)
  (goto-char (point-min)))

(defun ebib-goto-last-string ()
  "Move to the last string."
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

(defun ebib-strings-page-up ()
  "Move 10 strings up."
  (interactive)
  (forward-line -10))

(defun ebib-strings-page-down ()
  "Move 10 strings down."
  (interactive)
  (forward-line 10)
  (if (eobp)
      (forward-line -1)))

(defun ebib--fill-strings-buffer ()
  "Fill the strings buffer with the @String definitions."
  (with-current-ebib-buffer 'strings
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cl-dolist (elem (sort (ebib-db-list-strings ebib--cur-db) #'string<))
        (insert (ebib--generate-string-display elem) "\n")))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun ebib-edit-string ()
  "Edit the value of an @String definition.
When the user enters an empty string, the value is not changed."
  (interactive)
  (let* ((string (ebib--current-string))
         (init-contents-raw (ebib-get-string string ebib--cur-db 'noerror))
	 (init-contents (ebib-unbrace init-contents-raw)))
    (ebib--ifstring (new-contents (read-string (format "%s: " string)
                                               (if init-contents
                                                   (cons init-contents 0)
                                                 nil)))
        (progn
	  (ebib-set-string string new-contents ebib--cur-db 'overwrite
			   (ebib-unbraced-p init-contents-raw))
          (ebib--redisplay-strings-buffer)
          (ebib-next-string)
          (ebib--set-modified t ebib--cur-db t (ebib--list-dependents ebib--cur-db)))
      (error "[Ebib] @String definition cannot be empty"))))

(defun ebib-copy-string-contents ()
  "Copy the contents of the current string to the kill ring."
  (interactive)
  (let ((contents (ebib-get-string (ebib--current-string) ebib--cur-db nil 'unbraced)))
    (kill-new contents)
    (message "String value copied.")))

(defun ebib-delete-string ()
  "Delete the current @String definition from the database."
  (interactive)
  (let ((string (ebib--current-string)))
    (when (y-or-n-p (format "Delete @String definition %s? " string))
      (ebib-db-remove-string string ebib--cur-db)
      (let ((inhibit-read-only t))
        (delete-region (pos-bol) (1+ (pos-eol))))
      (when (eobp)                      ; Deleted the last string.
        (forward-line -1))
      (ebib--redisplay-strings-buffer)
      (ebib--set-modified t ebib--cur-db t (ebib--list-dependents ebib--cur-db))
      (message "@String definition deleted."))))

(defun ebib-toggle-raw-string ()
  "Toggle the \"special\" status of the current string's contents."
  (interactive)
  (let* ((string (ebib--current-string))
	 (def (ebib-get-string string ebib--cur-db 'noerror)))
    (ebib-set-string
     string			  ;; label
     def			  ;; content
     ebib--cur-db		  ;; db
     'overwrite			  ;; overwrite
     (not (ebib-unbraced-p def))) ;; whether to include braces
    (ebib--redisplay-strings-buffer)
    (ebib--set-modified t ebib--cur-db t (ebib--list-dependents ebib--cur-db))))

(defun ebib-add-string (&optional arg)
  "Create a new @String definition.
With prefix ARG, the string is created unbraced."
  (interactive "P")
  (ebib--ifstring (new-abbr (read-string "New @String abbreviation: " nil 'ebib--key-history))
      (if (member new-abbr (ebib-db-list-strings ebib--cur-db))
          (error "[Ebib] %s already exists" new-abbr)
        (ebib--ifstring (new-string (read-string (format "Value for %s: " new-abbr)))
            (progn
              (ebib-set-string new-abbr new-string ebib--cur-db 'error arg)
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (insert (ebib--generate-string-display new-abbr) "\n")
                (sort-lines nil (point-min) (point-max)))
              (goto-char (point-min))
              (re-search-forward new-string nil 'noerror)
              (beginning-of-line)
	      (ebib--redisplay-strings-buffer)
              (ebib--set-modified t ebib--cur-db t (ebib--list-dependents ebib--cur-db)))))))

(defun ebib-export-string (prefix)
  "Export the current @String to another database.
With PREFIX argument, export the @String to a file.  Otherwise export
the @String to another open database.  The user is asked for the file
or database to export the @String to."
  (interactive "P")
  (let ((string (ebib--current-string)))
    (unless string
      (error "No current string found"))
    (if prefix
        ;; Export to file.
        (let ((filename (expand-file-name (read-file-name "File to export @String to:" nil nil nil ebib--export-filename))))
          (if (file-writable-p filename)
              (with-temp-buffer
                (insert (format "\n@String{%s = %s}\n"
                                string
                                (ebib-db-get-string string ebib--cur-db))))
            (error "[Ebib] Cannot write to file `%s'" filename)))
      ;; Export to another database.
      (let ((target-db (ebib-read-database "Export @String to database:")))
        (unless target-db
          (error "[Ebib] Could not export @Strings"))
        (unless (or (ebib-db-filtered-p target-db)
                    (ebib-db-dependent-p target-db))
          (error "[Ebib] Cannot export to filtered or dependent database"))
        (if (member string (ebib-db-list-strings target-db))
            (ebib--log 'message "@String `%s' already exists in database %d" string (ebib-db-get-filename target-db 'short))
          (ebib-set-string string (ebib-db-get-string string ebib--cur-db) target-db 'error)
          (ebib-db-set-modified t target-db)
          (mapc (lambda (dependent)
                  (ebib-db-set-modified t dependent))
                (ebib--list-dependents target-db)))))))

(defun ebib-export-all-strings (prefix)
  "Export all @String definitions to another database.
With PREFIX argument, export the @String definitions to a file.
Otherwise export the @String definitions to another open
database.  The user is asked for the file or database to export
the @String definitions to."
  (interactive "P")
  (let ((strings (ebib-db-list-strings ebib--cur-db)))
    (unless strings
      (error "No @String definitions found"))
    (if prefix
        ;; Export to file.
        (let ((filename (expand-file-name (read-file-name "File to export @String to:" nil nil nil ebib--export-filename))))
          (if (file-writable-p filename)
              (with-temp-buffer
                (mapc (lambda (string)
                        (insert (format "\n@String{%s = %s}\n"
                                        string
                                        (ebib-db-get-string string ebib--cur-db))))
                      strings))
            (error "[Ebib] Cannot write to file `%s'" filename)))
      ;; Export to another database.
      (let ((target-db (ebib-read-database "Export @String to database:")))
        (unless target-db
          (error "[Ebib] Could not export @Strings"))
        (unless (or (ebib-db-filtered-p target-db)
                    (ebib-db-dependent-p target-db))
          (error "[Ebib] Cannot export to filtered or dependent database"))
        (let ((modified nil)
              (target-strings (ebib-db-list-strings target-db)))
          (mapc (lambda (string)
                  (if (member string target-strings)
                      (ebib--log 'message "@String `%s' already exists in database %d" string (ebib-db-get-filename target-db 'short))
                    (ebib-set-string string (ebib-db-get-string string ebib--cur-db) target-db 'error)
                    (setq modified t)))
                strings)
          (when modified
            (ebib-db-set-modified t target-db)
            (mapc (lambda (dependent)
                    (ebib-db-set-modified t dependent))
                  (ebib--list-dependents target-db))))))))

(defun ebib-strings-help ()
  "Show the info node on Ebib's strings buffer."
  (interactive)
  (ebib-lower)
  (info "(ebib) The Strings Buffer"))

;;; multiline-mode

(define-minor-mode ebib-multiline-mode
  "Minor mode for Ebib's multiline edit buffer."
  :init-value nil :lighter " Ebib/M" :global nil
  :keymap '(("\C-c\C-c" . ebib-quit-multiline-buffer-and-save)
            ("\C-c\C-k" . ebib-cancel-multiline-buffer)
            ("\C-c\C-s" . ebib-save-from-multiline-buffer)
            ("\C-c\C-h" . ebib-multiline-help)))

(easy-menu-define ebib-multiline-menu ebib-multiline-mode-map "Ebib multiline menu."
  '("Ebib"
    ["Store Text and Exit" ebib-quit-multiline-buffer-and-save t]
    ["Cancel Edit" ebib-cancel-multiline-buffer t]
    ["Save Text" ebib-save-from-multiline-buffer t]
    ["Help" ebib-multiline-help t]))

(defun ebib--multiline-edit (info &optional starttext)
  "Edit a multiline text.
INFO contains information about the text being edited.  It is a
list, the first element of which indicates the type of text,
either `preamble' or `field', and the second element the
database.  If the text being edited is a field value, the third
element is the entry key and the fourth the field name of the
field being edited.

If the preamble or field value pointed to by INFO already has a
multiline edit buffer associated with it, switch to that buffer.
Otherwise, create a new buffer and add it to
`ebib--multiline-buffer-list'.

STARTTEXT is a string that contains the initial text of the
buffer."
  (ebib--pop-to-buffer (or (ebib--get-multiline-buffer info)
                           (ebib--create-multiline-buffer info starttext))))

(defun ebib--get-multiline-buffer (info)
  "Return the multiline edit buffer associated with INFO."
  (car (cl-member info ebib--multiline-buffer-list
                  :test (lambda (elem buffer)
                          (cl-equalp (buffer-local-value 'ebib--multiline-info buffer) elem)))))

(defun ebib--create-multiline-buffer (info starttext)
  "Create a new multiline edit buffer.
INFO indicates what value will be edited and is stored in the
buffer-local value of `ebib--multiline-info'.  STARTTEXT is
inserted as initial text."
  (let* ((name (if (eq (car info) 'preamble)
                   "Preamble"
                 (format "%s-->%s" (cl-third info) (cl-fourth info))))
         (buffer (generate-new-buffer name)))
    (if buffer
        (with-current-buffer buffer
          (funcall ebib-multiline-major-mode)
          (ebib-multiline-mode t)
          (setq ebib--multiline-info info)
          (when starttext
            (insert starttext))
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (push buffer ebib--multiline-buffer-list)
          buffer)
      (error "[Ebib] Unable to create a new multiline edit buffer"))))

(defun ebib-quit-multiline-buffer-and-save ()
  "Quit the multiline edit buffer, saving the text."
  (interactive)
  (ebib--store-multiline-text (current-buffer))
  (ebib--kill-multiline-edit-buffer (current-buffer))
  (message "Text stored."))

(defun ebib-cancel-multiline-buffer ()
  "Quit the multiline edit buffer without saving.
If the buffer has been modified, ask for confirmation."
  (interactive)
  (catch 'no-cancel
    (when (buffer-modified-p)
      (unless (y-or-n-p "Text has been modified.  Abandon changes? ")
        (throw 'no-cancel nil)))
    (set-buffer-modified-p nil)
    (ebib--kill-multiline-edit-buffer (current-buffer))
    (message "Text not stored.")))

(defun ebib--kill-multiline-edit-buffer (buffer)
  "Kill multiline edit buffer BUFFER.
Also return focus to the index or entry buffer."
  (setq ebib--multiline-buffer-list (delq buffer ebib--multiline-buffer-list))
  (let ((info (buffer-local-value 'ebib--multiline-info buffer)))
    ;; Put the buffer out of sight.
    (if (and (eq ebib-layout 'index-only)
             ebib-popup-entry-window)
        (quit-window)
      (switch-to-buffer nil t))
    ;; Return to the index or entry window.
    (cond
     ((eq (car info) 'preamble)
      (ebib--pop-to-buffer (ebib--buffer 'index)))
     ((eq (car info) 'field)
      ;; Make sure we display the correct entry & field.
      (setq ebib--cur-db (ebib--get-db-from-filename (cl-second info)))
      (ebib-db-set-current-entry-key (cl-third info) ebib--cur-db)
      (ebib--update-buffers 'no-refresh)
      (ebib--pop-to-buffer (ebib--buffer 'entry))
      (re-search-forward (concat "^" (regexp-quote (cl-fourth info))) nil t)
      (beginning-of-line))))
  (kill-buffer buffer))

(defun ebib-save-from-multiline-buffer ()
  "Save the database from within the multiline edit buffer.
The text being edited is stored before saving the database."
  (interactive)
  (ebib--store-multiline-text (current-buffer))
  (ebib--save-database ebib--cur-db)
  (set-buffer-modified-p nil))

(defun ebib--store-multiline-text (buffer)
  "Store the text being edited in multiline edit buffer BUFFER."
  (with-current-buffer buffer
    (let ((text (buffer-substring-no-properties (point-min) (point-max)))
          (type (cl-first ebib--multiline-info))
          (db (ebib--get-db-from-filename (cl-second ebib--multiline-info)))
          (key (cl-third ebib--multiline-info)))
      (cond
       ((eq type 'preamble)
        (if (string= text "")
            (ebib-db-remove-preamble db)
          (ebib-db-set-preamble text db 'overwrite)))
       ((eq type 'field)
        (let ((field (cl-fourth ebib--multiline-info)))
          (if (string= text "")
              (ebib-db-remove-field-value field key db)
            (ebib-set-field-value field text key db 'overwrite)))))
      (set-buffer-modified-p nil)
      (ebib--set-modified t db t (seq-filter (lambda (dependent)
                                               (ebib-db-has-key key dependent))
                                             (ebib--list-dependents db))))))

(defun ebib-multiline-help ()
  "Show the info node on Ebib's multiline edit buffer."
  (interactive)
  (ebib-lower)
  (info "(ebib) The Multiline Edit Buffer"))

;;; log-mode

(defvar ebib-log-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map 'no-digits)
    (define-key map " " 'scroll-up)
    (define-key map "b" 'scroll-down)
    (define-key map "q" 'ebib-quit-log-buffer)
    map)
  "Keymap for the ebib log buffer.")

(define-derived-mode ebib-log-mode
  fundamental-mode "Ebib-log"
  "Major mode for the Ebib log buffer."
  (local-set-key "\C-xb" 'ebib-quit-log-buffer)
  (local-set-key "\C-xk" 'ebib-quit-log-buffer)
  (ebib-set-default-dir))

(defun ebib-quit-log-buffer ()
  "Exit the log buffer."
  (interactive)
  (if (and (eq ebib-layout 'index-only)
           ebib-popup-entry-window)
      (quit-window)
    (with-ebib-window-nondedicated (selected-window)
      (switch-to-buffer nil t)))
  (unless (ebib--pop-to-buffer (ebib--buffer 'index))
    (call-interactively (global-key-binding "\C-xb"))))  ; If the index buffer isn't visible, call `switch-to-buffer'.

;;; Functions for downloading additional data

(defun ebib-download-url (arg)
  "Download the file in the standard URL field from the Internet.
If the URL field contains more than one URL, ask the user which one
to download.  Alternatively, the user can provide a numeric prefix
argument ARG.

If an entry is found in `ebib-url-download-transformations' that
matches the URL, it is applied to the URL before attempting to
download the file.

The file is downloaded to the first entry for
`ebib-file-search-dirs'.  It is assumed that the file linked is a
pdf.  The file name is created by applying the function in
`ebib-name-transform-function' to the entry key and by appending
\".pdf\" to it."
  (interactive "P")
  (ebib--execute-when
    (entries
     (let* ((key (ebib--get-key-at-point))
            (urls (ebib-get-field-value "url" key ebib--cur-db 'noerror 'unbraced)))
       (if urls
           (let* ((fname (ebib--create-file-name-from-key key "pdf"))
                  (fullpath (concat (file-name-as-directory (car ebib-file-search-dirs)) fname))
                  (urlvalue (ebib--select-url urls (if (numberp arg) arg nil)))
                  (pdfurl (ebib--transform-url urlvalue))
                  (overwrite nil))
             (if (not (file-writable-p fullpath))
                 (error "[Ebib] File %s cannot be written to" fullpath))
             (if (not (string-match-p "\\.pdf\\'" pdfurl))
                 (let ((choice (read-char-choice (format "URL %s does not seem to be a pdf paper; (d)ownload anyway / (m)odify URL / (c)ancel? " pdfurl) '(?d ?m ?c ?q))))
                   (cl-case choice
                     ((?c ?q) (error "[Ebib] Cancelled downloading URL"))
                     (?m (setq pdfurl (read-string "New URL: " pdfurl))))))
             (while (and (file-exists-p fullpath)
                         (not overwrite))
               (let ((choice (read-char-choice (format "File %s already exists; (o)verwrite / (r)ename / (c)ancel? " fname) '(?o ?r ?c ?q))))
                 (cl-case choice
                   ((?c ?q) (error "[Ebib] Cancelled downloading URL"))
                   (?r (setq fname (read-string (format "Change `%s' to: " fname) fname))
                       (setq fullpath (concat (file-name-as-directory (car ebib-file-search-dirs)) fname)))
                   (?o (setq overwrite t)))))
             (url-copy-file pdfurl fullpath t)
             (message "[Ebib] Downloaded URL %s to %s" pdfurl fullpath)
             (let ((files (ebib-get-field-value "file" key ebib--cur-db 'noerror 'unbraced)))
               (when (or (null files)
                         (not (string-match-p (regexp-quote fname) files)))
                 (ebib-set-field-value "file" fname key ebib--cur-db ebib-filename-separator)
                 (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                                    (ebib-db-has-key key dependent))
                                                                  (ebib--list-dependents ebib--cur-db)))
                 (ebib--update-entry-buffer))))
         (error "[Ebib] No URL found in url field"))))
    (default
      (beep))))

(defun ebib--transform-url (url)
  "Transform URL by applying `ebib-url-download-transformations' to it.
The first matching entry in `ebib-url-download-transformations'
is applied.  If no entry matches, URL is returned unchanged."
  (let* ((transformer (seq-find (lambda (elt)
                                  (string-match-p (car elt) url))
                                ebib-url-download-transformations))
         (fn (cdr transformer)))
    (if fn
        (funcall fn url)
      url)))

(defun ebib-import-file (arg)
  "Import a file into the database.
Ask the user for a file path, rename it and move it to the first
directory in `ebib-file-search-dirs'.  The new name is created by
applying the function in `ebib-name-transform-function' to the
entry key.  The file extension of the original file is retained.
If prefix ARG is non-nil, do not delete the original file."
  (interactive "P")
  (let* ((key (ebib--get-key-at-point))
         (file-path (expand-file-name (read-file-name "File to import: " ebib-import-directory nil t)))
         (ext (file-name-extension file-path))
         (new-name (ebib--create-file-name-from-key key ext))
         (dest-dir (file-name-as-directory (car ebib-file-search-dirs)))
         (dest-path (concat dest-dir new-name))
         (overwrite nil))
    (if (not (file-writable-p dest-path))
        (error "[Ebib] Cannot write file %s" dest-path))
    (while (and (file-exists-p dest-path)
                (not overwrite))
      (let ((choice (read-char-choice (format "File %s already exists; (o)verwrite / (r)ename / (c)ancel? " new-name) '(?o ?r ?c ?q))))
        (cl-case choice
          ((?c ?q) (error "[Ebib] Cancelled importing file"))
          (?r (setq new-name (read-string (format "Change `%s' to: " new-name) new-name))
              (setq dest-path (concat (file-name-as-directory (car ebib-file-search-dirs)) new-name)))
          (?o (setq overwrite t)))))
    (copy-file file-path dest-path t)
    (unless arg
      (delete-file file-path t))
    (let ((files (ebib-get-field-value "file" key ebib--cur-db 'noerror 'unbraced)))
      (when (or (null files)
                (not (string-match-p (regexp-quote new-name) files)))
        (ebib-set-field-value "file" (ebib--transform-file-name-for-storing (expand-file-name dest-path)) key ebib--cur-db ebib-filename-separator)
        (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                           (ebib-db-has-key key dependent))
                                                         (ebib--list-dependents ebib--cur-db)))
        (ebib--update-entry-buffer)))))

;;; Functions for non-Ebib buffers

(defun ebib-import-entries (&optional db)
  "Search for BibTeX entries in the current buffer.
The entries are added to DB, which defaults to the current
database (i.e., the database that was active when Ebib was
lowered.  Works on the whole buffer, or on the region if it is
active."
  (interactive)
  (ebib--execute-when
    (real-db
     (save-excursion
       (save-restriction
         (if (use-region-p)
             (narrow-to-region (region-beginning)
                               (region-end)))
         (let ((db (or db ebib--cur-db))
               (buffer (current-buffer)))
           (with-temp-buffer
             (insert-buffer-substring buffer)
             (let ((result (ebib--bib-find-bibtex-entries db t)))
               (ebib-db-set-modified t db)
               (if-let ((index (ebib-db-get-buffer db))
                        (window (get-buffer-window index t)))
                   (ebib--update-index-buffer)
                 (ebib--mark-index-dirty db))
               (message (format "Imported %d entries, %d @Strings and %s @Preamble."
                                (car result)
                                (cadr result)
                                (if (nth 2 result) "a" "no")))))))))
    (default (error "[Ebib] No database loaded"))))

(define-obsolete-function-alias 'ebib-import 'ebib-import-entries "Ebib 2.32")

(defun ebib--get-db-from-filename (search-filename)
  "Return the database struct associated with SEARCH-FILENAME."
  (when search-filename
    (if (file-name-absolute-p search-filename)
        (setq search-filename (expand-file-name search-filename))) ; expand ~, . and ..
    (catch 'found
      (mapc (lambda (db)
              ;; If `search-filename' is an absolute file name, we want to compare to the
              ;; absolute file name of the database, otherwise we should use only
              ;; the non-directory component.
              (let ((db-filename (ebib-db-get-filename db (not (file-name-absolute-p search-filename)))))
                (if (file-name-absolute-p db-filename)
                    (setq db-filename (expand-file-name db-filename)))
                (if (string= search-filename db-filename)
                    (throw 'found db))))
            ebib--databases)
      nil)))

(defun ebib--get-local-bibfiles ()
  "Return a list of .bib files associated with the current buffer.
Each element in the list is a string holding the name of the .bib
file.

If the file-local variable `ebib-local-bibfiles' is set to a
list, return its value, otherwise try to find the .bib files for
the current buffer.  The method by which the .bib files are
searched depends on the buffer's major mode.  In LaTeX buffers,
this function searches the current buffer or the buffer file's
master file for a `\\bibliography' or `\\addbibresource' command
and returns the file(s) given in its argument.

In buffers in which `pandoc-mode' is active, check if the current
settings include a `bibliography' setting and use the files
listed there.

If no .bib files are found, return nil."
  (if (listp ebib-local-bibfiles)
      ebib-local-bibfiles
    (setq-local ebib-local-bibfiles
		(cond
		 ((eq major-mode 'latex-mode)
		  (ebib--get-local-bibfiles-latex))
		 ((and (boundp 'pandoc-mode) pandoc-mode)
		  (pandoc--get 'bibliography))
		 (t nil)))))

(defun ebib--get-local-bibfiles-latex ()
  "Return a list of .bib files associated with the current buffer.
The current buffer should be a LaTeX buffer.  See
`ebib--get-local-bibfiles' for details."
  (let ((texfile-buffer (current-buffer))
	texfile
	files)
    ;; If AucTeX's TeX-master is used and set to a string, we must
    ;; search that file for a \bibliography command, as it's more
    ;; likely to be in there than in the file we're in.
    (and (boundp 'TeX-master)
	 (stringp TeX-master)
	 (setq texfile (ebib--ensure-extension TeX-master ".tex")))
    (with-temp-buffer
      (if (and texfile (file-readable-p texfile))
	  (insert-file-contents texfile)
	(insert-buffer-substring texfile-buffer))
      (save-match-data
	(goto-char (point-min))
	;; First search for a \bibliography command:
	(if (re-search-forward "\\\\\\(?:no\\)*bibliography{\\(.*?\\)}" nil t)
	    (setq files (mapcar (lambda (file)
				  (ebib--ensure-extension file ".bib"))
				(split-string (buffer-substring-no-properties (match-beginning 1) (match-end 1)) ",[ ]*")))
	  ;; If we didn't find a \bibliography command, search for \addbibresource commands:
	  (while (re-search-forward "\\\\addbibresource\\(\\[.*?\\]\\)?{\\(.*?\\)}" nil t)
	    (let ((option (match-string 1))
		  (file (match-string-no-properties 2)))
	      ;; If this isn't a remote resource, add it to the list.
	      (unless (and option (string-match-p "location=remote" option))
		(push file files)))))))
    (when files
      (mapcar (lambda (file)
		(if (string= file "\\jobname.bib")
		    (setq file (file-name-nondirectory (concat (file-name-sans-extension (buffer-file-name))
							       (car ebib-bibtex-extensions)))))
		;; If a file has a directory part, we expand it, so
		;; `ebib--get-db-from-filename' can match it up with a
		;; database's file path.
		(if (file-name-directory file)
		    (expand-file-name file)
		  file))
	      files))))

(defun ebib-create-bib-from-bbl ()
  "Create a .bib file for the current LaTeX document.
The LaTeX document must have a .bbl file associated with it.  All
bibitems are extracted from this file and a new .bib file is
created containing only these entries."
  (interactive)
  (ebib--execute-when
   (database
    (or ebib-local-bibfiles
	(setq ebib-local-bibfiles (ebib--get-local-bibfiles)))
    (let* ((filename-sans-extension (file-name-sans-extension (buffer-file-name)))
	   (bbl-file (concat filename-sans-extension ".bbl"))
	   (bib-file (concat filename-sans-extension (car ebib-bibtex-extensions))))
      (unless (file-exists-p bbl-file)
	(error "[Ebib] No .bbl file exists.  Run BibTeX first"))
      (when (or (not (file-exists-p bib-file))
		(y-or-n-p (format "%s already exists.  Overwrite? " (file-name-nondirectory bib-file))))
	(when (file-exists-p bib-file)
	  (delete-file bib-file t))
	(let ((databases (seq-filter #'ebib--get-db-from-filename
				     ebib-local-bibfiles)))
	  (with-temp-buffer
	    (insert-file-contents bbl-file)
	    (ebib--export-entries-to-file (ebib-read-entries-from-bbl) bib-file databases))))))
   (default
     (beep))))

(defun ebib-read-entries-from-bbl ()
  "Read BibTeX entries from the .bbl file of the current buffer."
  (goto-char (point-min))
  (let (entries)
    (while (re-search-forward "\\\\\\(?:bibitem\\[\\(?:.\\|\n[^\n]\\)*]\\|entry\\){\\(.*?\\)}" nil t)
      (push (match-string 1) entries))
    entries))

(provide 'ebib)

;;; ebib.el ends here
