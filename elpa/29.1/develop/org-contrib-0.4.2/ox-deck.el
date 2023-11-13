;;; ox-deck.el --- deck.js Presentation Back-End for Org Export Engine

;; Copyright (C) 2013, 2014, 2021  Rick Frankel

;; Author: Rick Frankel <emacs at rickster dot com>
;; Keywords: outlines, hypermedia, slideshow

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

;; This library implements a deck.js presentation back-end for the Org
;; generic exporter.

;; Installation
;; -------------
;; Get a copy of deck.js from http://imakewebthings.com/deck.js/ or
;; the gitub repository at https://github.com/imakewebthings/deck.js.
;;
;; Add the path to the extracted code to the variable
;; `org-deck-directories' There are a number of customization in the
;; org-export-deck group, most of which can be overridden with buffer
;; local customization (starting with DECK_.)

;; See ox.el and ox-html.el for more details on how this exporter
;; works (it is derived from ox-html.)

;; TODOs
;; ------
;; The title page is formatted using format-spec.  This is error prone
;; when details are missing and may insert empty tags, like <h2></h2>,
;; for missing values.

(require 'ox-html)
(eval-when-compile (require 'cl))

(org-export-define-derived-backend 'deck 'html
  :menu-entry
  '(?d "Export to deck.js HTML Presentation"
       ((?H "To temporary buffer" org-deck-export-as-html)
	(?h "To file" org-deck-export-to-html)
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-deck-export-to-html t s v b)
		(org-open-file (org-deck-export-to-html nil s v b)))))))
  :options-alist
  '((:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    (:html-link-home "HTML_LINK_HOME" nil nil)
    (:html-link-up "HTML_LINK_UP" nil nil)
    (:deck-postamble "DECK_POSTAMBLE" nil org-deck-postamble newline)
    (:deck-preamble "DECK_PREAMBLE" nil org-deck-preamble newline)
    (:html-head-include-default-style "HTML_INCLUDE_DEFAULT_STYLE" "html-style" nil)
    (:html-head-include-scripts "HTML_INCLUDE_SCRIPTS" nil nil)
    (:deck-base-url "DECK_BASE_URL" nil org-deck-base-url)
    (:deck-theme "DECK_THEME" nil org-deck-theme)
    (:deck-transition "DECK_TRANSITION" nil org-deck-transition)
    (:deck-include-extensions "DECK_INCLUDE_EXTENSIONS" nil
			      org-deck-include-extensions split)
    (:deck-exclude-extensions "DECK_EXCLUDE_EXTENSIONS" nil
			      org-deck-exclude-extensions split))
  :translate-alist
  '((headline . org-deck-headline)
    (inner-template . org-deck-inner-template)
    (item . org-deck-item)
    (link . org-deck-link)
    (template . org-deck-template)))

(defgroup org-export-deck nil
  "Options for exporting Org mode files to deck.js HTML Presentations."
  :tag "Org Export DECK"
  :group 'org-export-html)

(defcustom org-deck-directories '("./deck.js")
  "Directories to search for deck.js components (jquery,
modernizr; core, extensions and themes directories.)"
  :group 'org-export-deck
  :type '(repeat (string :tag "Directory")))

(defun org-deck--cleanup-components (components)
  (remove-duplicates
   (car (remove 'nil components))
   :test (lambda (x y)
           (string= (file-name-nondirectory x)
                    (file-name-nondirectory y)))))

(defun org-deck--find-extensions ()
  "Returns a unique list of all extensions found in
in the extensions directories under `org-deck-directories'"
  (org-deck--cleanup-components
   (mapcar                              ; extensions under existing dirs
    (lambda (dir)
      (when (file-directory-p dir) (directory-files dir t "^[^.]")))
    (mapcar                           ; possible extension directories
     (lambda (x) (expand-file-name "extensions" x))
     org-deck-directories))))

(defun org-deck--find-css (type)
  "Return a unique list of all the css stylesheets in the themes/TYPE
directories under `org-deck-directories'."
  (org-deck--cleanup-components
   (mapcar
    (lambda (dir)
      (let ((css-dir (expand-file-name
                      (concat (file-name-as-directory "themes") type) dir)))
        (when (file-directory-p css-dir)
          (directory-files css-dir t  "\\.css$"))))
    org-deck-directories)))

(defun org-deck-list-components ()
  "List all available deck extensions, styles and
transitions (with full paths) to a temporary buffer."
  (interactive)
  (let ((outbuf (get-buffer-create "*deck.js Extensions*")))
    (with-current-buffer outbuf
      (erase-buffer)
      (insert "Extensions\n----------\n")
      (insert (mapconcat 'identity (org-deck--find-extensions) "\n"))
      (insert "\n\nStyles\n------\n")
      (insert (mapconcat 'identity (org-deck--find-css "style") "\n"))
      (insert "\n\nTransitions\n----------\n")
      (insert (mapconcat 'identity (org-deck--find-css "transition") "\n")))
    (switch-to-buffer-other-window outbuf)))

(defcustom org-deck-include-extensions nil
  "If non-nil, list of extensions to include instead of all available.
Can be overridden or set with the DECK_INCLUDE_EXTENSIONS property.
During output generation, the extensions found by
`org-deck--find-extensions' are searched for the appropriate
files (scripts and/or stylesheets) to include in the generated
html. The href/src attributes are created relative to `org-deck-base-url'."
  :group 'org-export-deck
  :type '(repeat (string :tag "Extension")))

(defcustom org-deck-exclude-extensions nil
  "If non-nil, list of extensions to exclude.
Can be overridden or set with the DECK_EXCLUDE_EXTENSIONS property."
  :group 'org-export-deck
  :type '(repeat (string :tag "Extension")))

(defcustom org-deck-theme "swiss.css"
  "deck.js theme. Can be overridden with the DECK_THEME property.
If this value contains a path component (\"/\"), it is used as a
literal path (url). Otherwise it is prepended with
`org-deck-base-url'/themes/style/."
  :group 'org-export-deck
  :type 'string)

(defcustom org-deck-transition "fade.css"
  "deck.js transition theme. Can be overridden with the
DECK_TRANSITION property.
If this value contains a path component (\"/\"), it is used as a
literal path (url). Otherwise it is prepended with
`org-deck-base-url'/themes/transition/."
  :group 'org-export-deck
  :type 'string)

(defcustom org-deck-base-url "deck.js"
  "Url prefix to deck.js base directory containing the core, extensions
and themes directories.
Can be overridden with the DECK_BASE_URL property."
  :group 'org-export-deck
  :type 'string)

(defvar org-deck-pre/postamble-styles
  `((both "left: 5px; width: 100%;")
    (preamble "position: absolute; top: 10px;")
    (postamble ""))
  "Alist of css styles for the preamble, postamble and both respectively.
Can be overridden in `org-deck-styles'. See also `org-html-divs'.")

(defcustom org-deck-postamble "<h1>%a - %t</h1>"
  "Non-nil means insert a postamble in HTML export.

When set to a string, use this string
as the postamble.  When t, insert a string as defined by the
formatting string in `org-html-postamble-format'.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

This is included in the document at the bottom of the content
section, and uses the postamble element and id from
`org-html-divs'. The default places the author and presentation
title at the bottom of each slide.

The css styling is controlled by `org-deck-pre/postamble-styles'.

Setting :deck-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-deck
  :type '(choice (const :tag "No postamble" nil)
                 (const :tag "Default formatting string" t)
                 (string :tag "Custom formatting string")
                 (function :tag "Function (must return a string)")))

(defcustom org-deck-preamble nil
  "Non-nil means insert a preamble in HTML export.

When set to a string, use this string
as the preamble.  When t, insert a string as defined by the
formatting string in `org-html-preamble-format'.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

This is included in the document at the top of  content section, and
uses the preamble element and id from `org-html-divs'. The css
styling is controlled by `org-deck-pre/postamble-styles'.

Setting :deck-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-deck
  :type '(choice (const :tag "No preamble" nil)
                 (const :tag "Default formatting string" t)
                 (string :tag "Custom formatting string")
                 (function :tag "Function (must return a string)")))

(defvar org-deck-toc-styles
  (mapconcat
   'identity
   (list
    "#table-of-contents a {color: inherit;}"
    "#table-of-contents ul {margin-bottom: 0;}"
    "#table-of-contents li {padding: 0;}") "\n")
  "Default css styles used for formatting a table of contents slide.
Can be overridden in `org-deck-styles'.
Note that when the headline numbering option is true, a \"list-style: none\"
is automatically added to avoid both numbers and bullets on the toc entries.")

(defcustom org-deck-styles
  "
#title-slide h1 {
    position: static; padding: 0;
    margin-top: 10%;
    -webkit-transform: none;
    -moz-transform: none;
    -ms-transform: none;
    -o-transform: none;
    transform: none;
}
#title-slide h2 {
    text-align: center;
    border:none;
    padding: 0;
    margin: 0;
}"
  "Deck specific CSS styles to include in exported html.
Defaults to styles for the title page."
  :group 'org-export-deck
  :type 'string)

(defcustom org-deck-title-slide-template
  "<h1>%t</h1>
<h2>%s</h2>
<h2>%a</h2>
<h2>%e</h2>
<h2>%d</h2>"
  "Format template to specify title page section.
See `org-html-postamble-format' for the valid elements which
can be included.

It will be wrapped in the element defined in the :html-container
property, and defaults to the value of `org-html-container-element',
and have the id \"title-slide\"."
  :group 'org-export-deck
  :type 'string)

(defun org-deck-toc (depth info)
  (concat
   (format "<%s id='table-of-contents' class='slide'>\n"
           (plist-get info :html-container))
   (format "<h2>%s</h2>\n" (org-html--translate "Table of Contents" info))
   (org-html--toc-text
    (mapcar
     (lambda (headline)
       (let* ((class (org-element-property :HTML_CONTAINER_CLASS headline))
              (section-number
               (when
                   (and (not (org-export-low-level-p headline info))
                        (org-export-numbered-headline-p headline info))
                 (concat
                  (mapconcat
                   'number-to-string
                   (org-export-get-headline-number headline info) ".") ". ")))
              (title
               (concat
                section-number
                (replace-regexp-in-string ; remove any links in headline...
                 "</?a[^>]*>" ""
                 (org-export-data
                  (org-element-property :title headline) info)))))
         (cons
          (if (and class (string-match-p "\\<slide\\>" class))
              (format
               "<a href='#outline-container-%s'>%s</a>"
               (or (org-element-property :CUSTOM_ID headline)
		   (concat
		    "sec-"
		    (mapconcat
		     'number-to-string
		     (org-export-get-headline-number headline info) "-")))
               title)
            title)
          (org-export-get-relative-level headline info))))
     (org-export-collect-headlines info depth)))
   (format "</%s>\n" (plist-get info :html-container))))

(defun org-deck--get-packages (info)
  (let ((prefix (concat (plist-get info :deck-base-url) "/"))
        (theme (plist-get info :deck-theme))
        (transition (plist-get info :deck-transition))
        (include (plist-get info :deck-include-extensions))
        (exclude (plist-get info :deck-exclude-extensions))
        (scripts '()) (sheets '()) (snippets '()))
    (add-to-list 'scripts (concat prefix "jquery.min.js"))
    (add-to-list 'scripts (concat prefix "core/deck.core.js"))
    (add-to-list 'scripts (concat prefix "modernizr.custom.js"))
    (add-to-list 'sheets  (concat prefix "core/deck.core.css"))
    (mapc
     (lambda (extdir)
       (let* ((name (file-name-nondirectory extdir))
              (dir (file-name-as-directory extdir))
              (path (concat prefix "extensions/" name "/"))
              (base (format "deck.%s." name)))
         (when (and (or (eq nil include) (member name include))
                    (not (member name exclude)))
           (when (file-exists-p (concat dir base "js"))
             (add-to-list 'scripts (concat path base "js")))
           (when (file-exists-p (concat dir base "css"))
             (add-to-list 'sheets (concat path base "css")))
           (when (file-exists-p (concat dir base "html"))
             (add-to-list 'snippets (concat dir base "html"))))))
     (org-deck--find-extensions))
    (if (not (string-match-p "^[[:space:]]*$" theme))
        (add-to-list 'sheets
                     (if (file-name-directory theme) theme
                       (format "%sthemes/style/%s" prefix theme))))
    (if (not (string-match-p "^[[:space:]]*$" transition))
        (add-to-list
         'sheets
         (if (file-name-directory transition) transition
           (format "%sthemes/transition/%s" prefix transition))))
    (list :scripts (nreverse scripts) :sheets (nreverse sheets)
          :snippets snippets)))

(defun org-deck-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat contents "\n"))

(defun org-deck-headline (headline contents info)
  (let ((org-html-toplevel-hlevel 2)
        (class (or (org-element-property :HTML_CONTAINER_CLASS headline) ""))
        (level (org-export-get-relative-level headline info)))
    (when (and (= 1 level) (not (string-match-p "\\<slide\\>" class)))
      (org-element-put-property headline :HTML_CONTAINER_CLASS (concat class " slide")))
    (org-html-headline headline contents info)))

(defun org-deck-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.
If the containing headline has the property :STEP, then
the \"slide\" class will be added to the to the list element,
 which will make the list into a \"build\"."
  (let ((text (org-html-item item contents info)))
    (if (org-export-get-node-property :STEP item t)
	(progn
	  (replace-regexp-in-string "^<li>" "<li class='slide'>" text)
	  (replace-regexp-in-string "^<li class='checkbox'>" "<li class='checkbox slide'>" text))
      text)))

(defun org-deck-link (link desc info)
  (replace-regexp-in-string "href=\"#" "href=\"#outline-container-"
			    (org-export-with-backend 'html link desc info)))

(defun org-deck-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((pkg-info (org-deck--get-packages info))
	(org-html--pre/postamble-class "deck-status")
	(info (plist-put
	       (plist-put info :html-preamble (plist-get info :deck-preamble))
	       :html-postamble (plist-get info :deck-postamble))))
    (mapconcat
     'identity
     (list
      (org-html-doctype info)
      (let ((lang (plist-get info :language)))
        (mapconcat
         (lambda (x)
           (apply
            'format
            "<!--%s <html %s lang='%s' xmlns='http://www.w3.org/1999/xhtml'> %s<![endif]-->"
            x))
         (list `("[if lt IE 7]>" "class='no-js ie6'" ,lang "")
               `("[if IE 7]>" "class='no-js ie7'" ,lang "")
               `("[if IE 8]>" "class='no-js ie8'" ,lang "")
               `("[if gt IE 8]><!-->" "" ,lang "<!--")) "\n"))
      "<head>"
      (org-deck--build-meta-info info)
      (mapconcat
       (lambda (sheet)
         (format
          "<link rel='stylesheet' href='%s' type='text/css' />" sheet))
       (plist-get pkg-info :sheets) "\n")
      (mapconcat
       (lambda (script)
         (format
          "<script src='%s'></script>" script))
       (plist-get pkg-info :scripts) "\n")
      (org-html--build-mathjax-config info)
      "<script>"
      "  $(document).ready(function () { $.deck('.slide'); });"
      "</script>"
      (org-html--build-head info)
      "<style type='text/css'>"
      org-deck-toc-styles
      (when (plist-get info :section-numbers)
        "#table-of-contents ul li {list-style-type: none;}")
      (format "#%s, #%s {%s}"
              (nth 2 (assq 'preamble org-html-divs))
              (nth 2 (assq 'postamble org-html-divs))
              (nth 1 (assq 'both org-deck-pre/postamble-styles)))
      (format "#%s {%s}"
              (nth 2 (assq 'preamble org-html-divs))
              (nth 1 (assq 'preamble org-deck-pre/postamble-styles)))
      (format "#%s {%s}"
              (nth 2 (assq 'postamble org-html-divs))
              (nth 1 (assq 'postamble org-deck-pre/postamble-styles)))
      org-deck-styles
      "</style>"
      "</head>"
      "<body>"
      (format "<%s id='%s' class='deck-container'>"
              (nth 1 (assq 'content org-html-divs))
              (nth 2 (assq 'content org-html-divs)))
      (org-html--build-pre/postamble 'preamble info)
      ;; title page
      (format "<%s id='title-slide' class='slide'>"
              (plist-get info :html-container))
      (format-spec org-deck-title-slide-template (org-html-format-spec info))
      (format "</%s>" (plist-get info :html-container))
      ;; toc page
      (let ((depth (plist-get info :with-toc)))
        (when depth (org-deck-toc depth info)))
      contents
      (mapconcat
       (lambda (snippet)
         (with-temp-buffer (insert-file-contents snippet)
                           (buffer-string)))
       (plist-get pkg-info :snippets) "\n")
      (org-html--build-pre/postamble 'postamble info)
      (format "</%s>" (nth 1 (assq 'content org-html-divs)))
      "</body>"
      "</html>\n") "\n")))

(defun org-deck--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let* ((title (org-export-data (plist-get info :title) info))
         (author (and (plist-get info :with-author)
                      (let ((auth (plist-get info :author)))
                        (and auth (org-export-data auth info)))))
         (date (and (plist-get info :with-date)
                    (let ((date (org-export-get-date info)))
                      (and date (org-export-data date info)))))
         (description (plist-get info :description))
         (keywords (plist-get info :keywords)))
    (mapconcat
     'identity
     (list
      (format "<title>%s</title>" title)
      (format "<meta http-equiv='Content-Type' content='text/html; charset=%s'/>"
              (or (and org-html-coding-system
                       (fboundp 'coding-system-get)
                       (coding-system-get
                        org-html-coding-system 'mime-charset))
                  "iso-8859-1"))
      (mapconcat
       (lambda (attr)
         (when (< 0 (length (car attr)))
           (format "<meta name='%s' content='%s'/>\n"
                   (nth 1 attr) (car attr))))
       (list '("Org-mode" "generator")
             `(,author "author")
             `(,description "description")
             `(,keywords "keywords")) "")) "\n")))
(defun org-deck-export-as-html
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

Export is done in a buffer named \"*Org deck.js Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'deck "*Org deck.js Export*"
    async subtreep visible-only body-only ext-plist (lambda () (nxml-mode))))

(defun org-deck-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a deck.js HTML file.

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
    (org-export-to-file 'deck file
      async subtreep visible-only body-only ext-plist)))

(defun org-deck-publish-to-html (plist filename pub-dir)
  "Publish an org file to deck.js HTML Presentation.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory. Returns output file name."
  (org-publish-org-to 'deck filename ".html" plist pub-dir))

(provide 'ox-deck)

;;; ox-deck.el ends here
