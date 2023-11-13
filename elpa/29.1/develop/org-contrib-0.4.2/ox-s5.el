;;; ox-s5.el --- S5 Presentation Back-End for Org Export Engine

;; Copyright (C) 2011-2014, 2021  Rick Frankel

;; Author: Rick Frankel <emacs at rickster dot com>
;; Keywords: outlines, hypermedia, S5, wp

;; This file is not part of GNU Emacs.

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

;; This library implements an S5 Presentation back-end for the Org
;; generic exporter.

;; Installation
;; ------------
;; Get the s5 scripts from
;;    https://meyerweb.com/eric/tools/s5/
;; (Note that the default s5 version is set for using the alpha, 1.2a2.
;; Copy the ui dir to somewhere reachable from your published presentation
;; The default (`org-s5-ui-url') is set to "ui" (e.g., in the
;; same directory as the html file).

;; Usage
;; -----
;; Follow the general instructions at the above website. To generate
;; incremental builds, you can set the HTML_CONTAINER_CLASS on an
;; object to "incremental" to make it build. If you want an outline to
;; build, set the :INCREMENTAL property on the parent headline.

;; To test it, run:
;;
;;   M-x org-s5-export-as-html
;;
;; in an Org mode buffer.  See ox.el and ox-html.el for more details
;; on how this exporter works.

;; TODOs
;; ------
;; The title page is formatted using format-spec.  This is error prone
;; when details are missing and may insert empty tags, like <h2></h2>,
;; for missing values.

(require 'ox-html)
(eval-when-compile (require 'cl))

(org-export-define-derived-backend 's5 'html
  :menu-entry
  '(?s "Export to S5 HTML Presentation"
       ((?H "To temporary buffer" org-s5-export-as-html)
	(?h "To file" org-s5-export-to-html)
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-s5-export-to-html t s v b)
		(org-open-file (org-s5-export-to-html nil s v b)))))))
  :options-alist
  '((:html-link-home "HTML_LINK_HOME" nil nil)
    (:html-link-up "HTML_LINK_UP" nil nil)
    (:s5-postamble "S5_POSTAMBLE" nil org-s5-postamble newline)
    (:s5-preamble "S5_PREAMBLE" nil org-s5-preamble newline)
    (:html-head-include-default-style "HTML_INCLUDE_DEFAULT_STYLE" nil nil)
    (:html-head-include-scripts "HTML_INCLUDE_SCRIPTS" nil nil)
    (:s5-version "S5_VERSION" nil org-s5-version)
    (:s5-theme-file "S5_THEME_FILE" nil org-s5-theme-file)
    (:s5-ui-url "S5_UI_URL" nil org-s5-ui-url)
    (:s5-default-view "S5_DEFAULT_VIEW" nil org-s5-default-view)
    (:s5-control-visibility "S5_CONTROL_VISIBILITY" nil
			    org-s5-control-visibility))
  :translate-alist
  '((headline . org-s5-headline)
    (plain-list . org-s5-plain-list)
    (inner-template . org-s5-inner-template)
    (template . org-s5-template)))

(defgroup org-export-s5 nil
  "Options for exporting Org mode files to S5 HTML Presentations."
  :tag "Org Export S5"
  :group 'org-export-html)

(defcustom org-s5-version "1.2a2"
  "Version of s5 being used (for version metadata.) Defaults to
s5 v2 alpha 2.
Can be overridden with S5_VERSION."
  :group 'org-export-s5
  :type 'string)

(defcustom org-s5-theme-file nil
"Url to S5 theme (slides.css) file. Can be overridden with the
S5_THEME_FILE property. If nil, defaults to
`org-s5-ui-url'/default/slides.css. If it starts with anything but
\"http\" or \"/\", it is used as-is. Otherwise the link in generated
relative to `org-s5-ui-url'.
The links for all other required stylesheets and scripts will be
generated relative to `org-s5-ui-url'/default."
  :group 'org-export-s5
  :type 'string)

(defcustom org-s5-ui-url "ui"
  "Base url to directory containing S5 \"default\" subdirectory
and the \"s5-notes.html\" file.
Can be overridden with the S5_UI_URL property."
  :group 'org-export-s5
  :type 'string)

(defcustom org-s5-default-view 'slideshow
  "Setting for \"defaultView\" meta info."
  :group 'org-export-s5
  :type '(choice (const slideshow) (const outline)))

(defcustom org-s5-control-visibility 'hidden
  "Setting for \"controlVis\" meta info."
  :group 'org-export-s5
  :type '(choice (const hidden) (const visibile)))

(defvar org-s5--divs
  '((preamble  "div" "header")
    (content   "div" "content")
    (postamble "div" "footer"))
  "Alist of the three section elements for HTML export.
The car of each entry is one of 'preamble, 'content or 'postamble.
The cdrs of each entry are the ELEMENT_TYPE and ID for each
section of the exported document.

If you set `org-html-container-element' to \"li\", \"ol\" will be
uses as the content ELEMENT_TYPE, generating an XOXO format
slideshow.

Note that changing the preamble or postamble will break the
core S5 stylesheets.")

(defcustom org-s5-postamble "<h1>%a - %t</h1>"
  "Preamble inserted into the S5 layout section.
When set to a string, use this string as the postamble.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting the S5_POSTAMBLE option -- or the :s5-postamble in publishing
projects -- will take precedence over this variable.

Note that the default css styling will break if this is set to nil
or an empty string."
  :group 'org-export-s5
  :type '(choice (const :tag "No postamble" "&#x20;")
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-s5-preamble "&#x20;"
  "Peamble inserted into the S5 layout section.

When set to a string, use this string as the preamble.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting S5_PREAMBLE option -- or the :s5-preamble in publishing
projects -- will take precedence over this variable.

Note that the default css styling will break if this is set to nil
or an empty string."
  :group 'org-export-s5
  :type '(choice (const :tag "No preamble" "&#x20;")
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-s5-title-slide-template
  "<h1>%t</h1>
<h2>%s</h2>
<h2>%a</h2>
<h3>%e</h3>
<h4>%d</h4>"
  "Format template to specify title page section.
See `org-html-postamble-format' for the valid elements which
can be included.

It will be wrapped in the element defined in the :html-container
property, and defaults to the value of `org-html-container-element',
and have the id \"title-slide\"."
  :group 'org-export-s5
  :type 'string)

(defun org-s5--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
Note that (currently) the S5 exporter does not support deep links,
so the table of contents is not \"active\".
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
         (section-number
          (and (not (org-export-low-level-p headline info))
               (org-export-numbered-headline-p headline info)
               (concat (mapconcat 'number-to-string headline-number ".") ". ")))
         (tags (and (eq (plist-get info :with-tags) t)
                    (org-export-get-tags headline info))))
    (concat section-number
            (org-export-data
             (org-export-get-alt-title headline info) info)
            (and tags "&nbsp;&nbsp;&nbsp;") (org-html--tags tags info))))

(defun org-s5-toc (depth info)
  (let* ((headlines (org-export-collect-headlines info depth))
         (toc-entries
          (mapcar (lambda (headline)
                    (cons (org-s5--format-toc-headline headline info)
                          (org-export-get-relative-level headline info)))
                  (org-export-collect-headlines info depth))))
    (when toc-entries
      (concat
       (format "<%s id='table-of-contents' class='slide'>\n"
               (plist-get info :html-container))
       (format "<h1>%s</h1>\n"
               (org-html--translate "Table of Contents" info))
       "<div id=\"text-table-of-contents\">"
       (org-html--toc-text toc-entries)
       "</div>\n"
       (format "</%s>\n" (plist-get info :html-container))))))

(defun org-s5--build-head (info)
  (let* ((dir (plist-get info :s5-ui-url))
         (theme (or (plist-get info :s5-theme-file) "default/slides.css")))
    (mapconcat
     'identity
     (list
      "<!-- style sheet links -->"
      (mapconcat
       (lambda (list)
         (format
          (concat
           "<link rel='stylesheet' href='%s/default/%s' type='text/css'"
           " media='%s' id='%s' />")
          dir (nth 0 list) (nth 1 list) (nth 2 list)))
       (list
        '("outline.css" "screen" "outlineStyle")
        '("print.css" "print" "slidePrint")
        '("opera.css" "projection" "operaFix")) "\n")
      (format (concat
               "<link rel='stylesheet' href='%s' type='text/css'"
               " media='screen' id='slideProj' />")
              (if (string-match-p "^\\(http\\|/\\)" theme) theme
                (concat dir "/" theme)))
      "<!-- S5 JS -->"
      (concat
       "<script src='" dir
       "/default/slides.js'></script>")) "\n")))

(defun org-s5--build-meta-info (info)
  (concat
   (org-html--build-meta-info info)
   (format "<meta name=\"version\" content=\"S5 %s\" />\n"
           (plist-get info :s5-version))
   (format "<meta name='defaultView' content='%s' />\n"
           (plist-get info :s5-default-view))
   (format "<meta name='controlVis' content='%s' />"
           (plist-get info :s5-control-visibility))))

(defun org-s5-headline (headline contents info)
  (let ((org-html-toplevel-hlevel 1)
        (class (or (org-element-property :HTML_CONTAINER_CLASS headline) ""))
        (level (org-export-get-relative-level headline info)))
    (when (and (= 1 level) (not (string-match-p "\\<slide\\>" class)))
      (org-element-put-property headline :HTML_CONTAINER_CLASS (concat class " slide")))
    (org-html-headline headline contents info)))

(defun org-s5-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information.
If a containing headline has the property :INCREMENTAL,
then the \"incremental\" class will be added to the to the list,
which will make the list into a \"build\"."
  (let* ((type (org-element-property :type plain-list))
        (tag (case type
               (ordered "ol")
               (unordered "ul")
               (descriptive "dl"))))
    (format "%s\n%s%s"
            (format
             "<%s class='org-%s%s'>" tag tag
             (if (org-export-get-node-property :INCREMENTAL plain-list t)
                 " incremental" ""))
            contents
	    (format "</%s>" tag))))

(defun org-s5-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat contents "\n"))

(defun org-s5-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((info (plist-put
	       (plist-put
		(plist-put info :html-preamble (plist-get info :s5-preamble))
		:html-postamble
		(plist-get info :s5-postamble))
	       :html-divs
	       (if (equal "li" (plist-get info :html-container))
		   (cons '(content "ol" "content") org-s5--divs)
		 org-s5--divs))))
    (mapconcat
     'identity
     (list
      (org-html-doctype info)
      (format "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\">"
	      (plist-get info :language) (plist-get info :language))
      "<head>"
      (org-s5--build-meta-info info)
      (org-s5--build-head info)
      (org-html--build-head info)
      (org-html--build-mathjax-config info)
      "</head>"
      "<body>"
      "<div class=\"layout\">"
      "<div id=\"controls\"><!-- no edit --></div>"
      "<div id=\"currentSlide\"><!-- no edit --></div>"
      (org-html--build-pre/postamble 'preamble info)
      (org-html--build-pre/postamble 'postamble info)
      "</div>"
      (format "<%s id=\"%s\" class=\"presentation\">"
	      (nth 1 (assq 'content org-html-divs))
	      (nth 2 (assq 'content org-html-divs)))
      ;; title page
      (format "<%s id='title-slide' class='slide'>"
	      (plist-get info :html-container))
      (format-spec org-s5-title-slide-template (org-html-format-spec info))
      (format "</%s>" (plist-get info :html-container))
      ;; table of contents.
      (let ((depth (plist-get info :with-toc)))
	(when depth (org-s5-toc depth info)))
      contents
      (format "</%s>" (nth 1 (assq 'content org-html-divs)))
      "</body>"
      "</html>\n") "\n")))

(defun org-s5-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org S5 Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 's5 "*Org S5 Export*"
    async subtreep visible-only body-only ext-plist (lambda () (nxml-mode))))

(defun org-s5-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a S5 HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 's5 file
      async subtreep visible-only body-only ext-plist)))

(defun org-s5-publish-to-html (plist filename pub-dir)
  "Publish an org file to S5 HTML Presentation.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 's5 filename ".html" plist pub-dir))

(provide 'ox-s5)

;;; ox-s5.el ends here
