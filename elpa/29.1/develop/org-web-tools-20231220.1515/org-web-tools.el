;;; org-web-tools.el --- Display and capture web content with Org-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2023  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-web-tools
;; Version: 1.3
;; Package-Requires: ((emacs "27.1") (org "9.0") (compat "29.1.4.2") (dash "2.12") (esxml "0.3.4") (s "1.10.0") (plz "0.7.1") (request "0.3.0"))
;; Keywords: hypermedia, outlines, Org, Web

;;; Commentary:

;; This file contains library functions and commands useful for
;; retrieving web page content and processing it into Org-mode
;; content.

;; For example, you can copy a URL to the clipboard or kill-ring, then
;; run a command that downloads the page, isolates the "readable"
;; content with `eww-readable', converts it to Org-mode content with
;; Pandoc, and displays it in an Org-mode buffer.  Another command
;; does all of that but inserts it as an Org entry instead of
;; displaying it in a new buffer.

;;;; Commands:

;; `org-web-tools-insert-link-for-url': Insert an Org-mode link to the
;; URL in the clipboard or kill-ring.  Downloads the page to get the
;; HTML title.

;; `org-web-tools-insert-web-page-as-entry': Insert the web page for
;; the URL in the clipboard or kill-ring as an Org-mode entry, as a
;; sibling heading of the current entry.

;; `org-web-tools-read-url-as-org': Display the web page for the URL
;; in the clipboard or kill-ring as Org-mode text in a new buffer,
;; processed with `eww-readable'.

;; `org-web-tools-convert-links-to-page-entries': With point on a
;; list of URLs in an Org-mode buffer, replace the list of URLs with a
;; list of Org headings, each containing the web page content of that
;; URL, converted to Org-mode text and processed with `eww-readable'.

;;;; Functions:

;; These are used in the commands above and may be useful in building
;; your own commands.

;; `org-web-tools--dom-to-html': Return parsed HTML DOM as an HTML
;; string.  Note: This is an approximation and is not necessarily
;; correct HTML (e.g. IMG tags may be rendered with a closing "</img>"
;; tag).

;; `org-web-tools--eww-readable': Return "readable" part of HTML with
;; title.

;; `org-web-tools--get-url': Return content for URL as string.

;; `org-web-tools--html-to-org-with-pandoc': Return string of HTML
;; converted to Org with Pandoc.

;; `org-web-tools--url-as-readable-org': Return string containing Org
;; entry of URL's web page content.  Content is processed with
;; `eww-readable' and Pandoc.  Entry will be a top-level heading, with
;; article contents below a second-level "Article" heading, and a
;; timestamp in the first-level entry for writing comments.

;; `org-web-tools--demote-headings-below': Demote all headings in
;; buffer so the highest level is below LEVEL.

;; `org-web-tools--get-first-url': Return URL in clipboard, or first
;; URL in the kill-ring, or nil if none.

;; `org-web-tools--read-org-bracket-link': Return (TARGET . DESCRIPTION)
;; for Org bracket LINK or next link on current line.

;; `org-web-tools--remove-dos-crlf': Remove all DOS CRLF (^M) in buffer.

;;; License:

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

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'dom)
(require 'esxml-query)
(require 'eww)
(require 'org)
(require 's)
(require 'shr)
(require 'subr-x)
(require 'thingatpt)

(require 'plz)

;;;; Variables

;;;; Customization

(defgroup org-web-tools nil
  "Options for `org-web-tools'."
  :group 'org
  :link '(url-link "http://github.com/alphapapa/org-web-tools"))

;;;; Pandoc support

(defconst org-web-tools--pandoc-no-wrap-option nil
  "Option to pass to Pandoc to disable wrapping.
Pandoc >= 1.16 deprecates `--no-wrap' in favor of
`--wrap=none'.")

(defcustom org-web-tools-pandoc-replacements
  (list (cons (rx "") ""))
  "Used to clean output from Pandoc."
  :type '(alist :key-type string
                :value-type string))

(defcustom org-web-tools-pandoc-sleep-time 0.2
  "Wait this long for Pandoc to start the first time in a session..
Normally this should not need to be changed, but if Pandoc takes
unusually long to start on your system (which it seems to on
FreeBSD, for some reason), you may need to increase this."
  :type 'float)

(defun org-web-tools--html-to-org-with-pandoc (html &optional selector)
  "Return string of HTML converted to Org with Pandoc.
When SELECTOR is non-nil, the HTML is filtered using
`esxml-query' SELECTOR and re-rendered to HTML with
`org-web-tools--dom-to-html', which see."
  (when selector
    (setq html (->> (with-temp-buffer
                      (insert html)
                      (libxml-parse-html-region 1 (point-max)))
                    (esxml-query selector)
                    ;; MAYBE: Should probably use `shr-dom-print' instead.
                    (org-web-tools--dom-to-html))))
  (with-temp-buffer
    (insert html)
    (let ((stderr-file (make-temp-file "org-web-tools-pandoc-stderr")))
      (unwind-protect
          (if (not (zerop (call-process-region (point-min) (point-max) "pandoc"
                                               t (list t stderr-file) nil
                                               "--verbose"
                                               (org-web-tools--pandoc-no-wrap-option)
                                               "-f" "html-raw_html-native_divs" "-t" "org")))
              (progn
                (delete-region (point-min) (point-max))
                (insert-file-contents stderr-file)
                (error "Pandoc failed: %s" (buffer-string)))
            (org-mode)
            (org-web-tools--clean-pandoc-output)
            (buffer-string))
        (delete-file stderr-file)))))

(defun org-web-tools--pandoc-no-wrap-option ()
  "Return option `org-web-tools--pandoc-no-wrap-option', setting if unset."
  (or org-web-tools--pandoc-no-wrap-option
      (setq org-web-tools--pandoc-no-wrap-option (org-web-tools--check-pandoc-no-wrap-option))))

(defun org-web-tools--check-pandoc-no-wrap-option ()
  "Return appropriate no-wrap option string depending on Pandoc version."
  ;; Pandoc >= 1.16 deprecates the --no-wrap option, replacing it with
  ;; --wrap=none.  Sending the wrong option causes output to STDERR,
  ;; which `call-process-region' doesn't like.  So we test Pandoc to see
  ;; which option to use.
  (with-temp-buffer
    (let* ((limit 3)
           (checked 0)
           (process (start-process "test-pandoc" (current-buffer)
                                   "pandoc" "--dump-args" "--no-wrap")))
      (while (process-live-p process)
        (if (= checked limit)
            (progn
              ;; Pandoc didn't exit in time.  Kill it and raise an
              ;; error.  This function will return `nil' and
              ;; `org-web-tools--pandoc-no-wrap-option' will remain
              ;; `nil', which will cause this function to run again and
              ;; set the const when a capture is run.
              (set-process-query-on-exit-flag process nil)
              (error "Unable to test Pandoc.  Try increasing `org-web-tools-pandoc-sleep-time'.  If it still doesn't work, please report this bug! (Include the output of \"pandoc --dump-args --no-wrap\")"))
          (sleep-for org-web-tools-pandoc-sleep-time)
          (cl-incf checked)))
      (if (and (zerop (process-exit-status process))
               (not (string-match "--no-wrap is deprecated" (buffer-string))))
          "--no-wrap"
        "--wrap=none"))))

(defun org-web-tools--clean-pandoc-output ()
  "Remove unwanted things in current buffer of Pandoc output."
  (org-web-tools--remove-bad-characters)
  (org-web-tools--remove-html-blocks)
  (org-web-tools--remove-custom_id_properties))

(defun org-web-tools--remove-bad-characters ()
  "Remove unwanted characters from current buffer.
Bad characters are matched by `org-web-tools-pandoc-replacements'."
  (save-excursion
    (cl-loop for (re . replacement) in org-web-tools-pandoc-replacements
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match replacement))))))

(defun org-web-tools--remove-html-blocks ()
  "Remove \"#+BEGIN_HTML...#+END_HTML\" blocks from current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx (optional "\n")
                                  "#+BEGIN_HTML"
                                  (minimal-match (1+ anything))
                                  "#+END_HTML"
                                  (optional "\n"))
                              nil t)
      (replace-match ""))))

(defun org-web-tools--remove-custom_id_properties ()
  "Remove property drawers containing CUSTOM_ID properties.
This is a blunt instrument: any drawer containing the CUSTOM_ID
property is removed, regardless of other properties it may
contain.  This seems to be the best course of action in current
Pandoc output."
  (let ((regexp (org-re-property "CUSTOM_ID" nil nil)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (when (org-at-property-p)
          (org-back-to-heading)
          ;; As a minor optimization, we don't bound the search to the current entry.  Unless the
          ;; current property drawer is malformed, which shouldn't happen in Pandoc output, it
          ;; should work.
          (re-search-forward org-property-drawer-re)
          (setf (buffer-substring (match-beginning 0) (match-end 0)) ""))))))

;;;; Commands

;;;###autoload
(defun org-web-tools-insert-link-for-url (url)
  "Insert Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'."
  (interactive (list (org-web-tools--get-first-url)))
  (insert (org-web-tools--org-link-for-url url)))

;;;###autoload
(cl-defun org-web-tools-insert-web-page-as-entry (url &key (capture-function #'org-web-tools--url-as-readable-org))
  "Insert web page contents of URL as Org sibling entry.
CAPTURE-FUNCTION is called with URL to get the contents.  Page is
processed with `eww-readable'."
  (interactive (list (org-web-tools--get-first-url)))
  (let ((content (s-trim (funcall capture-function url))))
    (unless (string-empty-p content)
      (unless (eq major-mode 'org-mode)
        (display-warning 'org-web-tools "Pasting Org subtree into non-org-mode buffer; this may cause problems"))
      (beginning-of-line) ; Necessary for org-paste-subtree to choose the right heading level
      (org-paste-subtree nil content)
      ;; Return t because org-paste-subtree doesn't
      t)))

;;;###autoload
(cl-defun org-web-tools-read-url-as-org (url &key (show-buffer-function #'switch-to-buffer))
  "Read URL's readable content in an Org buffer.
Buffer is displayed using SHOW-BUFFER-FUNCTION."
  (interactive (list (org-web-tools--get-first-url)))
  (let ((entry (org-web-tools--url-as-readable-org url)))
    (when entry
      (funcall show-buffer-function url)
      (org-mode)
      (insert entry)
      ;; Set buffer title
      (goto-char (point-min))
      (rename-buffer (cdr (org-web-tools--read-org-bracket-link))))))

;;;###autoload
(defun org-web-tools-convert-links-to-page-entries ()
  "Convert links in current entry into entries containing linked pages' content.
Both plain links and Org bracket links are processed.  Page
content is processed with `eww-readable'.  All links in the
current entry (i.e. this does not look deeper in the subtree, nor
outside of it) will be converted."
  (interactive)
  (cl-flet ((prev-url (entry-beg)
              ;; Work from the bottom of the list to the top, makes it simpler
              (when (re-search-backward (rx "http" (optional "s") "://" (1+ (not (any space)))) entry-beg 'no-error)
                ;; Found link; see if it's an Org link
                (beginning-of-line)
                (if (re-search-forward org-link-bracket-re (line-end-position) 'noerror)
                    ;; Org link
                    (list ;; Reconstruct link from regexp groups
                     (concat (match-string 1) (match-string 3))
                     (match-beginning 0))
                  ;; Plain link
                  (list (match-string 0) (match-beginning 0))))))
    (let ((level (1+ (org-outline-level)))
          (entry-beg (org-entry-beginning-position)))
      (goto-char (org-entry-end-position))
      (while (-when-let* (((url link-beg) (save-excursion
                                            (prev-url entry-beg)))
                          (new-entry (org-web-tools--url-as-readable-org url)))
               ;; TODO: Needs error handling
               ;; FIXME: If a URL fails to fetch, this should skip it, but
               ;; that means the failed URL will become part of the next
               ;; entry's contents.  Might need to read the whole list at
               ;; once, use markers to track the list's position, then
               ;; replace the whole list with any errored URLs after it's
               ;; done.
               (goto-char link-beg) ; This should NOT be necessary!  But it is, because the point moves back down a line!  Why?!
               (delete-region (line-beginning-position) (line-end-position))
               (org-paste-subtree level new-entry)
               ;; org-paste-subtree returns nil, so we have to return t
               t)
        t))))

;;;; Functions

(cl-defun org-web-tools--org-link-for-url (&optional (url (org-web-tools--get-first-url)))
  "Return Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'.  If page
at URL has no title, return URL."
  (if-let ((dom (plz 'get url :as (lambda ()
                                    (libxml-parse-html-region (point-min) (point-max)))))
           (title (cl-caddr (car (dom-by-tag dom 'title)))))
      (org-link-make-string url (org-web-tools--cleanup-title title))
    (message "HTML page at URL has no title")
    url))

(defun org-web-tools--eww-readable (dom)
  "Return \"readable\" part of DOM with title.
Returns list (TITLE . HTML).  Based on `eww-readable'."
  (let ((title (cl-caddr (car (dom-by-tag dom 'title)))))
    (eww-score-readability dom)
    (cons title
          (with-temp-buffer
            (shr-dom-print (eww-highest-readability dom))
            (buffer-string)))))

(defun org-web-tools--url-as-readable-org (&optional url)
  "Return string containing Org entry of URL's web page content.
Content is processed with `eww-readable' and Pandoc.  Entry will
be a top-level heading, with article contents below a
second-level \"Article\" heading, and a timestamp in the
first-level entry for writing comments."
  ;; By taking an optional URL, and getting it from the clipboard if
  ;; none is given, this becomes suitable for use in an org-capture
  ;; template, like:

  ;; ("wr" "Capture Web site with eww-readable" entry
  ;;  (file "~/org/articles.org")
  ;;  "%(org-web-tools--url-as-readable-org)")
  (-let* ((url (or url (org-web-tools--get-first-url)))
          (dom (plz 'get url :as #'org-web-tools--sanitized-dom))
          ((title . readable) (org-web-tools--eww-readable dom))
          (title (org-web-tools--cleanup-title (or title "")))
          (converted (org-web-tools--html-to-org-with-pandoc readable))
          (link (org-link-make-string url title))
          (timestamp (format-time-string (org-time-stamp-format 'with-time 'inactive))))
    (with-temp-buffer
      (org-mode)
      ;; Insert article text
      (insert converted)
      ;; Demote in-article headings
      ;; MAYBE: Use `org-paste-subtree' instead of demoting headings ourselves.
      (org-web-tools--demote-headings-below 2)
      ;; Insert headings at top
      (goto-char (point-min))
      (insert "* " link " :website:" "\n\n"
              timestamp "\n\n"
              "** Article" "\n\n")
      (buffer-string))))

(defun org-web-tools--sanitized-dom ()
  "Return sanitized DOM for HTML in current buffer."
  ;; libxml-parse-html-region converts "&nbsp;" to " ", so we have to
  ;; clean the HTML first.
  (cl-loop for (match . replace) in (list (cons "&nbsp;" " "))
           do (progn
                (goto-char (point-min))
                (while (re-search-forward match nil t)
                  (replace-match replace))))
  (libxml-parse-html-region (point-min) (point-max)))

;;;;; Misc

(defun org-web-tools--cleanup-title (title)
  "Return TITLE with spurious whitespace removed."
  (->> title
       (s-replace "\n" " ")
       (s-trim)
       (s-collapse-whitespace)))

(defun org-web-tools--demote-headings-below (level &optional skip)
  "Demote all headings in buffer so the highest level is below LEVEL.
If all headings are already below that level, none are adjusted.
If SKIP is non-nil, it is passed to `org-map-entries', which see.
Note that \"highest level\" means the fewest number of
stars (i.e. the highest level possible has 1 star)."
  (let* ((buffer-highest-level (progn
                                 (goto-char (point-min))
                                 (when (org-before-first-heading-p)
                                   (outline-next-heading))
                                 (cl-loop while (org-at-heading-p)
                                          collect (org-outline-level) into result
                                          do (outline-next-heading)
                                          finally return (if result
                                                             (seq-min result)
                                                           0))))
         (difference (- buffer-highest-level level))
         (adjust-by (when (<= difference 0)
                      (1+ (* -1 difference)))))
    (when adjust-by
      ;; Demote headings in buffer
      (org-map-entries
       (lambda ()
         (dotimes (_ adjust-by)
           (org-demote)))
       t nil skip))))

(defun org-web-tools--dom-to-html (dom)
  "Return parsed HTML object DOM as an HTML string.
Note: This is an approximation and is not necessarily correct
HTML."
  ;; MAYBE: Use `shr-dom-print' instead?  (I think I wasn't aware of that function when I wrote
  ;; this.)
  ;; NOTE: As the docstring says, certain HTML tags may not be
  ;; rendered correctly, like IMG tags which aren't supposed to have
  ;; closing </img> tags.  As far as I can tell, there is no canonical
  ;; way to transform a parsed DOM back to correct HTML in Emacs.
  ;; This is probably close enough to still be useful in many cases.
  (cl-labels ((render (node)
                (cl-typecase node
                  (string node)
                  (list (concat "<"
                                (symbol-name (dom-tag node))
                                (when (dom-attributes node)
                                  (concat " " (mapconcat #'attr (dom-attributes node) " ")))
                                ">"
                                (mapconcat #'render (dom-children node) "\n")
                                "</" (symbol-name (dom-tag node)) ">"))))
              (attr (pair)
                (format "%s=\"%s\"" (car pair) (cdr pair))))
    (render dom)))

(defun org-web-tools--get-first-url ()
  "Return URL in clipboard, or first URL in the `kill-ring', or nil if none."
  (cl-loop for item in (cons (current-kill 0) kill-ring)
           when (and item (string-match-p (rx bol "http" (optional "s") "://") item))
           return item))

(declare-function org-element-property "org-element")
(defun org-web-tools--read-url ()
  "Return URL at point, from clipboard, from `kill-ring', or prompt."
  (or (thing-at-point-url-at-point)
      (org-element-property :raw-link (org-element-context))
      (org-web-tools--get-first-url)
      (read-string "URL: ")))

(defconst org-web-tools--link-desc-submatch
  (if (version<= "9.3" org-version) 2 3)
  "Match group index of link description in `org-link-bracket-re'.")

(defun org-web-tools--read-org-bracket-link (&optional link)
  "Return (TARGET . DESCRIPTION) for Org bracket LINK or next link on current line."
  ;; Searching to the end of the line seems the simplest way
  (save-excursion
    (let (target desc)
      (if link
          ;; Link passed as arg
          (when (string-match org-link-bracket-re link)
            (setq target (match-string-no-properties 1 link)
                  desc (match-string-no-properties org-web-tools--link-desc-submatch link)))
        ;; No arg; get link from buffer
        (when (re-search-forward org-link-bracket-re (pos-eol) t)
          (setq target (match-string-no-properties 1)
                desc (match-string-no-properties org-web-tools--link-desc-submatch))))
      (when (and target desc)
        ;; Link found; return parts
        (cons target desc)))))

(provide 'org-web-tools)

;;; org-web-tools.el ends here
