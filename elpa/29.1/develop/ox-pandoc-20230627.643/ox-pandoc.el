;;; ox-pandoc.el --- An Org-mode exporter using pandoc -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 KAWABATA, Taichi
;; Copyright (C) 2021-2023 FENTON, Alex

;; Filename: ox-pandoc.el
;; Description: Another org exporter for Pandoc
;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;;         FENTON, Alex <a-fent@github>
;; Maintainer: FENTON, Alex <a-fent@github>
;; Created: 2014-07-20
;; Version: 2.0
;; Package-Requires: ((org "8.2") (emacs "24.4") (dash "2.8") (ht "2.0"))
;; Keywords: tools
;; URL: https://github.com/a-fent/ox-pandoc

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

;; This is another exporter for org-mode that translates Org-mode file
;; to various other formats via Pandoc.  You need org-mode version 8.2
;; or later, and Pandoc 2.0 or later, to use this package.  For
;; details, please refer https://github.com/a-fent/ox-pandoc.

;;; Code:
(require 'ox-org)
(require 'dash)
(require 'ht)
(require 'cl-lib)
(require 'ox-html) ; from `org', needed for `org-html-standalone-image-p'

(defgroup org-pandoc nil
  "Options specific to Pandoc export back-end."
  :tag "Org Pandoc"
  :group 'org-export)

(defconst org-pandoc-valid-options
  '(abbreviations ascii atx-headers base-header-level bash-completion
    biblatex bibliography citation-abbreviations citeproc columns csl css
    data-dir default-image-extension defaults dpi dump-args email-obfuscation eol
    epub-chapter-level epub-cover-image epub-embed-font epub-metadata
    epub-subdirectory extract-media fail-if-warnings file-scope filter
    highlight-style html-q-tags id-prefix ignore-args include-after-body
    include-before-body include-in-header incremental
    indented-code-classes katex list-extensions list-highlight-languages
    list-highlight-styles listings log lua-filter mathjax mathml
    metadata natbib no-highlight number-offset number-sections
    pdf-engine-opt pdf-engine preserve-tabs print-default-data-file
    print-default-template quiet reference-doc reference-links
    reference-location resource-path section-divs self-contained
	shift-heading-level-by slide-level standalone strip-comments
	syntax-definition tab-stop  table-of-contents template title-prefix
	toc top-level-division toc-depth trace track-changes variable
	verbose version webtex wrap))

(defconst org-pandoc-colon-separated-options
  '(abbreviations css include-in-header include-before-body
    include-after-body pdf-engine-opt epub-embed-font bibliography
    filter lua-filter))

(defconst org-pandoc-file-options
  '(abbreviations bibliography citation-abbreviations csl defaults
    epub-cover-image epub-embed-font epub-metadata include-after-body
    include-before-body include-in-header log lue-filter
	print-default-data-file reference-doc syntax-definition))

(defconst org-pandoc-extensions
  '((asciidoc . txt) (beamer . tex)
    (commonmark . md) (context . tex)
    (docbook4 . dbk) (docbook5 . dbk) (dokuwiki . doku)
    (dzslides . html) (epub2 . epub) (epub3 . epub) (gfm . md)
    (haddock . hs) (html4 . html) (html5 . html) (jira . md) (latex . tex)
    (markdown . md)
    (markdown_mmd . md) (markdown_phpextra . md)
    (markdown_strict . md) (native . hs)
    (opendocument . xml) (plain . txt) (revealjs . html) (s5 . html)
    (slideous . html) (slidy . html) (texinfo . texi)
    (zimwiki . zim)))

(defconst org-pandoc-option-type
  `(choice (const t) (const nil)
           (alist :key-type (choice ,@(--map `(const ,it) org-pandoc-valid-options))
                  :value-type (choice (const t) (const nil) string))))

;; For exporting with ox-pandoc, the "LABEL" attribute is preserved and
;; not, as in other exporters, normalised to "NAME"
(defconst org-pandoc-element-keyword-translation-alist
	  (--remove (equal "LABEL" (car it) ) org-element-keyword-translation-alist))

(defcustom org-pandoc-options '((standalone . t)) ;; (mathjax . t) (parse-raw . t)
  "Pandoc options."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-format-extensions nil
  "List of Pandoc format extensions for specific output format.
For example, if you want to specify markdown to have footnotes extension,
set as `(markdown_strict+footnotes)'."
  :group 'org-pandoc
  :type '(repeat symbol))

(defcustom org-pandoc-command "pandoc"
  "Pandoc command."
  :group 'org-pandoc
  :type 'string)

(defcustom org-pandoc-check-version t
  "Whether to check for a pandoc executable and its version on loading.
If true, will warn if no pandoc can be found, or if it is an older
version. If nil, no checks are performed and no warnings generated."
  :group 'org-pandoc
  :type 'boolean)

(defcustom org-pandoc-menu-entry
  '(
    ;;(?0 "to jats." org-pandoc-export-to-jats)
    ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
    ;;(?  "as jats." org-pandoc-export-as-jats)
    ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
    ;;(?! "to epub2." org-pandoc-export-to-epub2)
    ;;(?2 "to tei." org-pandoc-export-to-tei)
    ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
    ;;(?" "as tei." org-pandoc-export-as-tei)
    ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
    ;;(?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
    ;;(?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
    ;;(?4 "to html5." org-pandoc-export-to-html5)
    (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
    (?$ "as html5." org-pandoc-export-as-html5)
    (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
    (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
    ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
    ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
    ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
    ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
    ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
    ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
    ;;(?8 "to opendocument." org-pandoc-export-to-opendocument)
    ;;(?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
    ;;(?( "as opendocument." org-pandoc-export-as-opendocument)
    ;;(?9 "to opml." org-pandoc-export-to-opml)
    ;;(?9 "to opml and open." org-pandoc-export-to-opml-and-open)
    ;;(?) "as opml." org-pandoc-export-as-opml)
    ;;(?: "to rst." org-pandoc-export-to-rst)
    ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
    ;;(?* "as rst." org-pandoc-export-as-rst)
    ;;(?< "to slideous." org-pandoc-export-to-slideous)
    ;; (?\[ "to jira." org-pandoc-export-to-jira)
	;; (?\[ "as jira." org-pandoc-export-as-jira)
    ;; (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
    ;; (?, "as slideous." org-pandoc-export-as-slideous)
    (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
    (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
    ;;(?> "to textile." org-pandoc-export-to-textile)
    ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
    ;;(?. "as textile." org-pandoc-export-as-textile)
    ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
    ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
    ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
    (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
    (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
    ;; (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
    ;; (?C "to context-pdf." org-pandoc-export-to-context-pdf)
    ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
    (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
    (?D "as docbook5." org-pandoc-export-as-docbook5)
    ;; (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
    ;; (?E "to epub3." org-pandoc-export-to-epub3)
    ;;(?f "to fb2." org-pandoc-export-to-fb2)
    ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
    ;;(?F "as fb2." org-pandoc-export-as-fb2)
    ;;(?g "to gfm." org-pandoc-export-to-gfm)
    (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
    (?G "as gfm." org-pandoc-export-as-gfm)
    ;;(?h "to html4." org-pandoc-export-to-html4)
    (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
    (?H "as html4." org-pandoc-export-as-html4)
    ;;(?i "to icml." org-pandoc-export-to-icml)
    ;; (?i "to icml and open." org-pandoc-export-to-icml-and-open)
    ;; (?I "as icml." org-pandoc-export-as-icml)
    ;;(?j "to json." org-pandoc-export-to-json)
    (?j "to json and open." org-pandoc-export-to-json-and-open)
    (?J "as json." org-pandoc-export-as-json)
    ;;(?k "to markdown." org-pandoc-export-to-markdown)
    ;;(?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
    ;;(?K "as markdown." org-pandoc-export-as-markdown)
    (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
    (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
    ;;(?m "to man." org-pandoc-export-to-man)
    (?m "to man and open." org-pandoc-export-to-man-and-open)
    (?M "as man." org-pandoc-export-as-man)
    ;;(?n "to native." org-pandoc-export-to-native)
    (?n "to native and open." org-pandoc-export-to-native-and-open)
    (?N "as native." org-pandoc-export-as-native)
    (?o "to odt and open." org-pandoc-export-to-odt-and-open)
    (?O "to odt." org-pandoc-export-to-odt)
    (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
    (?P "to pptx." org-pandoc-export-to-pptx)
    ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
    ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
    ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
    ;;(?r "to rtf." org-pandoc-export-to-rtf)
    (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
    (?R "as rtf." org-pandoc-export-as-rtf)
    ;;(?s "to s5." org-pandoc-export-to-s5)
    ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
    ;;(?S "as s5." org-pandoc-export-as-s5)
    ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
    ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
    ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
    ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
    ;; (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
    ;; (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
    ;;(?v "to revealjs." org-pandoc-export-to-revealjs)
    ;; (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
    ;; (?V "as revealjs." org-pandoc-export-as-revealjs)
    ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
    ;; (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
    ;; (?W "as mediawiki." org-pandoc-export-as-mediawiki)
    (?x "to docx and open." org-pandoc-export-to-docx-and-open)
    (?X "to docx." org-pandoc-export-to-docx)
    ;;(?y "to slidy." org-pandoc-export-to-slidy)
    ;; (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
    ;; (?Y "as slidy." org-pandoc-export-as-slidy)
    ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
    ;; (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
    ;; (?Z "as dzslides." org-pandoc-export-as-dzslides)
    ;;(?{ "to muse." org-pandoc-export-to-muse)
    ;;(?{ "to muse and open." org-pandoc-export-to-muse-and-open)
    ;;(?[ "as muse." org-pandoc-export-as-muse)
    ;;(?} "to zimwiki." org-pandoc-export-to-zimwiki)
    ;;(?} "to zimwiki and open." org-pandoc-export-to-zimwiki-and-open)
    ;;(?] "as zimwiki." org-pandoc-export-as-zimwiki)
    ;;(?~ "to haddock." org-pandoc-export-to-haddock)
    ;;(?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
    ;;(?^ "as haddock." org-pandoc-export-as-haddock)
    )
  "Pandoc menu-entry."
  :group 'org-pandoc
  :type 'list)

(org-export-define-derived-backend 'pandoc 'org
  :translate-alist '((entity    . org-pandoc-entity)
					 (export-block . org-pandoc-export-block)
					 (export-snippet . org-pandoc-export-snippet)
					 (latex-environment . org-pandoc-latex-environ)
                     (link      . org-pandoc-link)
                     (paragraph . org-pandoc-paragraph)
                     (src-block . org-pandoc-src-block)
                     (table     . org-pandoc-table)
                     (template  . org-pandoc-template))
  ;; :export-block "PANDOC"
  :menu-entry
  `(?p "export via pandoc"
       ,org-pandoc-menu-entry)
  :options-alist
  '((:pandoc-options "PANDOC_OPTIONS" nil nil space)
    (:pandoc-extensions "PANDOC_EXTENSIONS" nil nil space)
    (:pandoc-metadata "PANDOC_METADATA" nil nil space)
    (:pandoc-variables "PANDOC_VARIABLES" nil nil space)
    (:epub-chapter-level "EPUB_CHAPTER_LEVEL" nil nil t)
    (:epub-cover-image "EPUB_COVER" nil nil t)
    (:epub-stylesheet "EPUB_STYLESHEET" nil nil t)
    (:epub-embed-font "EPUB_EMBED_FONT" nil nil newline)
    (:epub-meta "EPUB_META" nil nil newline)
    (:epub-css "EPUB_CSS" nil nil newline)
    (:epub-rights "EPUB_RIGHTS" nil nil newline)
    (:bibliography "BIBLIOGRAPHY")))

(defcustom org-pandoc-epub-rights
  (concat "Copyright " (format-time-string "%Y")
          (if user-full-name (concat " " user-full-name))
          (if user-mail-address (concat " <" user-mail-address ">")))
  "Pandoc option for EPUB copyrihgt statement."
  :group 'org-pandoc
  :type 'string)

;;; each backend processor

(defcustom org-pandoc-options-for-asciidoc nil
  "Pandoc options for asciidoc."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-asciidoc-hook nil
  "Hook called after processing asciidoc."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-asciidoc (&optional a s v b e)
  "Export to asciidoc."
  (interactive) (org-pandoc-export 'asciidoc a s v b e))

;;;###autoload
(defun org-pandoc-export-to-asciidoc-and-open (&optional a s v b e)
  "Export to asciidoc and open."
  (interactive) (org-pandoc-export 'asciidoc a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-asciidoc (&optional a s v b e)
  "Export as asciidoc."
  (interactive) (org-pandoc-export 'asciidoc a s v b e t))

(defcustom org-pandoc-options-for-beamer nil
  "Pandoc options for beamer."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-beamer-hook nil
  "Hook called after processing beamer."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-beamer (&optional a s v b e)
  "Export to beamer."
  (interactive) (org-pandoc-export 'beamer a s v b e))

;;;###autoload
(defun org-pandoc-export-to-beamer-and-open (&optional a s v b e)
  "Export to beamer and open."
  (interactive) (org-pandoc-export 'beamer a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-beamer (&optional a s v b e)
  "Export as beamer."
  (interactive) (org-pandoc-export 'beamer a s v b e t))

(defcustom org-pandoc-options-for-beamer-pdf nil
  "Pandoc options for beamer-pdf."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-beamer-pdf (&optional a s v b e)
  "Export to beamer-pdf."
  (interactive) (org-pandoc-export 'beamer-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-beamer-pdf-and-open (&optional a s v b e)
  "Export to beamer-pdf and open."
  (interactive) (org-pandoc-export 'beamer-pdf a s v b e 0))

(defcustom org-pandoc-options-for-commonmark nil
  "Pandoc options for commonmark."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-commonmark-hook nil
  "Hook called after processing commonmark."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-commonmark (&optional a s v b e)
  "Export to commonmark."
  (interactive) (org-pandoc-export 'commonmark a s v b e))

;;;###autoload
(defun org-pandoc-export-to-commonmark-and-open (&optional a s v b e)
  "Export to commonmark and open."
  (interactive) (org-pandoc-export 'commonmark a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-commonmark (&optional a s v b e)
  "Export as commonmark."
  (interactive) (org-pandoc-export 'commonmark a s v b e t))

(defcustom org-pandoc-options-for-context nil
  "Pandoc options for context."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-context-hook nil
  "Hook called after processing context."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-context (&optional a s v b e)
  "Export to context."
  (interactive) (org-pandoc-export 'context a s v b e))

;;;###autoload
(defun org-pandoc-export-to-context-and-open (&optional a s v b e)
  "Export to context and open."
  (interactive) (org-pandoc-export 'context a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-context (&optional a s v b e)
  "Export as context."
  (interactive) (org-pandoc-export 'context a s v b e t))

(defcustom org-pandoc-options-for-context-pdf nil
  "Pandoc options for context-pdf."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-context-pdf (&optional a s v b e)
  "Export to context-pdf."
  (interactive) (org-pandoc-export 'context-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-context-pdf-and-open (&optional a s v b e)
  "Export to context-pdf and open."
  (interactive) (org-pandoc-export 'context-pdf a s v b e 0))

(defcustom org-pandoc-options-for-docbook4 nil
  "Pandoc options for docbook4."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-docbook4-hook nil
  "Hook called after processing docbook4."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-docbook4 (&optional a s v b e)
  "Export to docbook4."
  (interactive) (org-pandoc-export 'docbook4 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-docbook4-and-open (&optional a s v b e)
  "Export to docbook4 and open."
  (interactive) (org-pandoc-export 'docbook4 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-docbook4 (&optional a s v b e)
  "Export as docbook4."
  (interactive) (org-pandoc-export 'docbook4 a s v b e t))

(defcustom org-pandoc-options-for-docbook5 nil
  "Pandoc options for docbook5."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-docbook5-hook nil
  "Hook called after processing docbook5."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-docbook5 (&optional a s v b e)
  "Export to docbook5."
  (interactive) (org-pandoc-export 'docbook5 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-docbook5-and-open (&optional a s v b e)
  "Export to docbook5 and open."
  (interactive) (org-pandoc-export 'docbook5 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-docbook5 (&optional a s v b e)
  "Export as docbook5."
  (interactive) (org-pandoc-export 'docbook5 a s v b e t))

(defcustom org-pandoc-options-for-docx nil
  "Pandoc options for docx."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-docx (&optional a s v b e)
  "Export to docx."
  (interactive) (org-pandoc-export 'docx a s v b e))

;;;###autoload
(defun org-pandoc-export-to-docx-and-open (&optional a s v b e)
  "Export to docx and open."
  (interactive) (org-pandoc-export 'docx a s v b e 0))

(defcustom org-pandoc-options-for-dokuwiki nil
  "Pandoc options for dokuwiki."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-dokuwiki-hook nil
  "Hook called after processing dokuwiki."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-dokuwiki (&optional a s v b e)
  "Export to dokuwiki."
  (interactive) (org-pandoc-export 'dokuwiki a s v b e))

;;;###autoload
(defun org-pandoc-export-to-dokuwiki-and-open (&optional a s v b e)
  "Export to dokuwiki and open."
  (interactive) (org-pandoc-export 'dokuwiki a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-dokuwiki (&optional a s v b e)
  "Export as dokuwiki."
  (interactive) (org-pandoc-export 'dokuwiki a s v b e t))

(defcustom org-pandoc-options-for-dzslides nil
  "Pandoc options for dzslides."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-dzslides-hook nil
  "Hook called after processing dzslides."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-dzslides (&optional a s v b e)
  "Export to dzslides."
  (interactive) (org-pandoc-export 'dzslides a s v b e))

;;;###autoload
(defun org-pandoc-export-to-dzslides-and-open (&optional a s v b e)
  "Export to dzslides and open."
  (interactive) (org-pandoc-export 'dzslides a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-dzslides (&optional a s v b e)
  "Export as dzslides."
  (interactive) (org-pandoc-export 'dzslides a s v b e t))

(defcustom org-pandoc-options-for-epub2 nil
  "Pandoc options for epub2."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-epub2 (&optional a s v b e)
  "Export to epub2."
  (interactive) (org-pandoc-export 'epub2 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-epub2-and-open (&optional a s v b e)
  "Export to epub2 and open."
  (interactive) (org-pandoc-export 'epub2 a s v b e 0))

(defcustom org-pandoc-options-for-epub3 nil
  "Pandoc options for epub3."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-epub3 (&optional a s v b e)
  "Export to epub3."
  (interactive) (org-pandoc-export 'epub3 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-epub3-and-open (&optional a s v b e)
  "Export to epub3 and open."
  (interactive) (org-pandoc-export 'epub3 a s v b e 0))

(defcustom org-pandoc-options-for-fb2 nil
  "Pandoc options for fb2."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-fb2-hook nil
  "Hook called after processing fb2."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-fb2 (&optional a s v b e)
  "Export to fb2."
  (interactive) (org-pandoc-export 'fb2 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-fb2-and-open (&optional a s v b e)
  "Export to fb2 and open."
  (interactive) (org-pandoc-export 'fb2 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-fb2 (&optional a s v b e)
  "Export as fb2."
  (interactive) (org-pandoc-export 'fb2 a s v b e t))

(defcustom org-pandoc-options-for-gfm nil
  "Pandoc options for gfm."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-gfm-hook nil
  "Hook called after processing gfm."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-gfm (&optional a s v b e)
  "Export to gfm."
  (interactive) (org-pandoc-export 'gfm a s v b e))

;;;###autoload
(defun org-pandoc-export-to-gfm-and-open (&optional a s v b e)
  "Export to gfm and open."
  (interactive) (org-pandoc-export 'gfm a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-gfm (&optional a s v b e)
  "Export as gfm."
  (interactive) (org-pandoc-export 'gfm a s v b e t))

(defcustom org-pandoc-options-for-haddock nil
  "Pandoc options for haddock."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-haddock-hook nil
  "Hook called after processing haddock."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-haddock (&optional a s v b e)
  "Export to haddock."
  (interactive) (org-pandoc-export 'haddock a s v b e))

;;;###autoload
(defun org-pandoc-export-to-haddock-and-open (&optional a s v b e)
  "Export to haddock and open."
  (interactive) (org-pandoc-export 'haddock a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-haddock (&optional a s v b e)
  "Export as haddock."
  (interactive) (org-pandoc-export 'haddock a s v b e t))

(defcustom org-pandoc-options-for-html4 nil
  "Pandoc options for html4."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-html4-hook nil
  "Hook called after processing html4."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-html4 (&optional a s v b e)
  "Export to html4."
  (interactive) (org-pandoc-export 'html4 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-html4-and-open (&optional a s v b e)
  "Export to html4 and open."
  (interactive) (org-pandoc-export 'html4 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-html4 (&optional a s v b e)
  "Export as html4."
  (interactive) (org-pandoc-export 'html4 a s v b e t))

(defcustom org-pandoc-options-for-html5 nil
  "Pandoc options for html5."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-html5-hook nil
  "Hook called after processing html5."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-html5 (&optional a s v b e)
  "Export to html5."
  (interactive) (org-pandoc-export 'html5 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-html5-and-open (&optional a s v b e)
  "Export to html5 and open."
  (interactive) (org-pandoc-export 'html5 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-html5 (&optional a s v b e)
  "Export as html5."
  (interactive) (org-pandoc-export 'html5 a s v b e t))

(defcustom org-pandoc-options-for-html5-pdf nil
  "Pandoc options for html5-pdf."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-html5-pdf (&optional a s v b e)
  "Export to html5-pdf."
  (interactive) (org-pandoc-export 'html5-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-html5-pdf-and-open (&optional a s v b e)
  "Export to html5-pdf and open."
  (interactive) (org-pandoc-export 'html5-pdf a s v b e 0))

(defcustom org-pandoc-options-for-icml nil
  "Pandoc options for icml."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-icml-hook nil
  "Hook called after processing icml."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-icml (&optional a s v b e)
  "Export to icml."
  (interactive) (org-pandoc-export 'icml a s v b e))

;;;###autoload
(defun org-pandoc-export-to-icml-and-open (&optional a s v b e)
  "Export to icml and open."
  (interactive) (org-pandoc-export 'icml a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-icml (&optional a s v b e)
  "Export as icml."
  (interactive) (org-pandoc-export 'icml a s v b e t))

(defcustom org-pandoc-options-for-jats nil
  "Pandoc options for jats."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-jats-hook nil
  "Hook called after processing jats."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-jats (&optional a s v b e)
  "Export to jats."
  (interactive) (org-pandoc-export 'jats a s v b e))

;;;###autoload
(defun org-pandoc-export-to-jats-and-open (&optional a s v b e)
  "Export to jats and open."
  (interactive) (org-pandoc-export 'jats a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-jats (&optional a s v b e)
  "Export as jats."
  (interactive) (org-pandoc-export 'jats a s v b e t))

(defcustom org-pandoc-options-for-jira nil
  "Pandoc options for jira."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-jira-hook nil
  "Hook called after processing jira."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-jira (&optional a s v b e)
  "Export to jira."
  (interactive) (org-pandoc-export 'jira a s v b e))

;;;###autoload
(defun org-pandoc-export-as-jira (&optional a s v b e)
  "Export as jira."
  (interactive) (org-pandoc-export 'jira a s v b e t))

(defcustom org-pandoc-options-for-json nil
  "Pandoc options for json."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-json-hook nil
  "Hook called after processing json."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-json (&optional a s v b e)
  "Export to json."
  (interactive) (org-pandoc-export 'json a s v b e))

;;;###autoload
(defun org-pandoc-export-to-json-and-open (&optional a s v b e)
  "Export to json and open."
  (interactive) (org-pandoc-export 'json a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-json (&optional a s v b e)
  "Export as json."
  (interactive) (org-pandoc-export 'json a s v b e t))

(defcustom org-pandoc-options-for-latex nil
  "Pandoc options for latex."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-latex-hook nil
  "Hook called after processing latex."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-latex (&optional a s v b e)
  "Export to latex."
  (interactive) (org-pandoc-export 'latex a s v b e))

;;;###autoload
(defun org-pandoc-export-to-latex-and-open (&optional a s v b e)
  "Export to latex and open."
  (interactive) (org-pandoc-export 'latex a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-latex (&optional a s v b e)
  "Export as latex."
  (interactive) (org-pandoc-export 'latex a s v b e t))

(defcustom org-pandoc-options-for-latex-pdf nil
  "Pandoc options for latex-pdf."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-latex-pdf (&optional a s v b e)
  "Export to latex-pdf."
  (interactive) (org-pandoc-export 'latex-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-latex-pdf-and-open (&optional a s v b e)
  "Export to latex-pdf and open."
  (interactive) (org-pandoc-export 'latex-pdf a s v b e 0))

(defcustom org-pandoc-options-for-man nil
  "Pandoc options for man."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-man-hook nil
  "Hook called after processing man."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-man (&optional a s v b e)
  "Export to man."
  (interactive) (org-pandoc-export 'man a s v b e))

;;;###autoload
(defun org-pandoc-export-to-man-and-open (&optional a s v b e)
  "Export to man and open."
  (interactive) (org-pandoc-export 'man a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-man (&optional a s v b e)
  "Export as man."
  (interactive) (org-pandoc-export 'man a s v b e t))

(defcustom org-pandoc-options-for-markdown nil
  "Pandoc options for markdown."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown-hook nil
  "Hook called after processing markdown."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-markdown (&optional a s v b e)
  "Export to markdown."
  (interactive) (org-pandoc-export 'markdown a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown-and-open (&optional a s v b e)
  "Export to markdown and open."
  (interactive) (org-pandoc-export 'markdown a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown (&optional a s v b e)
  "Export as markdown."
  (interactive) (org-pandoc-export 'markdown a s v b e t))

(defcustom org-pandoc-options-for-markdown_mmd nil
  "Pandoc options for markdown_mmd."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown_mmd-hook nil
  "Hook called after processing markdown_mmd."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-markdown_mmd (&optional a s v b e)
  "Export to markdown_mmd."
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown_mmd-and-open (&optional a s v b e)
  "Export to markdown_mmd and open."
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown_mmd (&optional a s v b e)
  "Export as markdown_mmd."
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e t))

(defcustom org-pandoc-options-for-markdown_phpextra nil
  "Pandoc options for markdown_phpextra."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown_phpextra-hook nil
  "Hook called after processing markdown_phpextra."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-markdown_phpextra (&optional a s v b e)
  "Export to markdown_phpextra."
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown_phpextra-and-open (&optional a s v b e)
  "Export to markdown_phpextra and open."
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown_phpextra (&optional a s v b e)
  "Export as markdown_phpextra."
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e t))

(defcustom org-pandoc-options-for-markdown_strict nil
  "Pandoc options for markdown_strict."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown_strict-hook nil
  "Hook called after processing markdown_strict."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-markdown_strict (&optional a s v b e)
  "Export to markdown_strict."
  (interactive) (org-pandoc-export 'markdown_strict a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown_strict-and-open (&optional a s v b e)
  "Export to markdown_strict and open."
  (interactive) (org-pandoc-export 'markdown_strict a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown_strict (&optional a s v b e)
  "Export as markdown_strict."
  (interactive) (org-pandoc-export 'markdown_strict a s v b e t))

(defcustom org-pandoc-options-for-mediawiki nil
  "Pandoc options for mediawiki."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-mediawiki-hook nil
  "Hook called after processing mediawiki."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-mediawiki (&optional a s v b e)
  "Export to mediawiki."
  (interactive) (org-pandoc-export 'mediawiki a s v b e))

;;;###autoload
(defun org-pandoc-export-to-mediawiki-and-open (&optional a s v b e)
  "Export to mediawiki and open."
  (interactive) (org-pandoc-export 'mediawiki a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-mediawiki (&optional a s v b e)
  "Export as mediawiki."
  (interactive) (org-pandoc-export 'mediawiki a s v b e t))

(defcustom org-pandoc-options-for-ms nil
  "Pandoc options for ms."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-ms-hook nil
  "Hook called after processing ms."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-ms (&optional a s v b e)
  "Export to ms."
  (interactive) (org-pandoc-export 'ms a s v b e))

;;;###autoload
(defun org-pandoc-export-to-ms-and-open (&optional a s v b e)
  "Export to ms and open."
  (interactive) (org-pandoc-export 'ms a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-ms (&optional a s v b e)
  "Export as ms."
  (interactive) (org-pandoc-export 'ms a s v b e t))

(defcustom org-pandoc-options-for-ms-pdf nil
  "Pandoc options for ms-pdf."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-ms-pdf (&optional a s v b e)
  "Export to ms-pdf."
  (interactive) (org-pandoc-export 'ms-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-ms-pdf-and-open (&optional a s v b e)
  "Export to ms-pdf and open."
  (interactive) (org-pandoc-export 'ms-pdf a s v b e 0))

(defcustom org-pandoc-options-for-muse nil
  "Pandoc options for muse."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-muse-hook nil
  "Hook called after processing muse."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-muse (&optional a s v b e)
  "Export to muse."
  (interactive) (org-pandoc-export 'muse a s v b e))

;;;###autoload
(defun org-pandoc-export-to-muse-and-open (&optional a s v b e)
  "Export to muse and open."
  (interactive) (org-pandoc-export 'muse a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-muse (&optional a s v b e)
  "Export as muse."
  (interactive) (org-pandoc-export 'muse a s v b e t))

(defcustom org-pandoc-options-for-native nil
  "Pandoc options for native."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-native-hook nil
  "Hook called after processing native."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-native (&optional a s v b e)
  "Export to native."
  (interactive) (org-pandoc-export 'native a s v b e))

;;;###autoload
(defun org-pandoc-export-to-native-and-open (&optional a s v b e)
  "Export to native and open."
  (interactive) (org-pandoc-export 'native a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-native (&optional a s v b e)
  "Export as native."
  (interactive) (org-pandoc-export 'native a s v b e t))

(defcustom org-pandoc-options-for-odt nil
  "Pandoc options for odt."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-odt (&optional a s v b e)
  "Export to odt."
  (interactive) (org-pandoc-export 'odt a s v b e))

;;;###autoload
(defun org-pandoc-export-to-odt-and-open (&optional a s v b e)
  "Export to odt and open."
  (interactive) (org-pandoc-export 'odt a s v b e 0))

(defcustom org-pandoc-options-for-opendocument nil
  "Pandoc options for opendocument."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-opendocument-hook nil
  "Hook called after processing opendocument."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-opendocument (&optional a s v b e)
  "Export to opendocument."
  (interactive) (org-pandoc-export 'opendocument a s v b e))

;;;###autoload
(defun org-pandoc-export-to-opendocument-and-open (&optional a s v b e)
  "Export to opendocument and open."
  (interactive) (org-pandoc-export 'opendocument a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-opendocument (&optional a s v b e)
  "Export as opendocument."
  (interactive) (org-pandoc-export 'opendocument a s v b e t))

(defcustom org-pandoc-options-for-opml nil
  "Pandoc options for opml."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-opml-hook nil
  "Hook called after processing opml."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-opml (&optional a s v b e)
  "Export to opml."
  (interactive) (org-pandoc-export 'opml a s v b e))

;;;###autoload
(defun org-pandoc-export-to-opml-and-open (&optional a s v b e)
  "Export to opml and open."
  (interactive) (org-pandoc-export 'opml a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-opml (&optional a s v b e)
  "Export as opml."
  (interactive) (org-pandoc-export 'opml a s v b e t))

(defcustom org-pandoc-options-for-org nil
  "Pandoc options for org."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-org-hook nil
  "Hook called after processing org."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-org (&optional a s v b e)
  "Export to org."
  (interactive) (org-pandoc-export 'org a s v b e))

;;;###autoload
(defun org-pandoc-export-to-org-and-open (&optional a s v b e)
  "Export to org and open."
  (interactive) (org-pandoc-export 'org a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-org (&optional a s v b e)
  "Export as org."
  (interactive) (org-pandoc-export 'org a s v b e t))

(defcustom org-pandoc-options-for-plain nil
  "Pandoc options for plain."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-plain-hook nil
  "Hook called after processing plain."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-plain (&optional a s v b e)
  "Export to plain."
  (interactive) (org-pandoc-export 'plain a s v b e))

;;;###autoload
(defun org-pandoc-export-to-plain-and-open (&optional a s v b e)
  "Export to plain and open."
  (interactive) (org-pandoc-export 'plain a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-plain (&optional a s v b e)
  "Export as plain."
  (interactive) (org-pandoc-export 'plain a s v b e t))

(defcustom org-pandoc-options-for-pptx nil
  "Pandoc options for pptx."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-pptx (&optional a s v b e)
  "Export to pptx."
  (interactive) (org-pandoc-export 'pptx a s v b e))

;;;###autoload
(defun org-pandoc-export-to-pptx-and-open (&optional a s v b e)
  "Export to pptx and open."
  (interactive) (org-pandoc-export 'pptx a s v b e 0))

(defcustom org-pandoc-options-for-revealjs nil
  "Pandoc options for revealjs."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-revealjs-hook nil
  "Hook called after processing revealjs."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-revealjs (&optional a s v b e)
  "Export to revealjs."
  (interactive) (org-pandoc-export 'revealjs a s v b e))

;;;###autoload
(defun org-pandoc-export-to-revealjs-and-open (&optional a s v b e)
  "Export to revealjs and open."
  (interactive) (org-pandoc-export 'revealjs a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-revealjs (&optional a s v b e)
  "Export as revealjs."
  (interactive) (org-pandoc-export 'revealjs a s v b e t))

(defcustom org-pandoc-options-for-rst nil
  "Pandoc options for rst."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-rst-hook nil
  "Hook called after processing rst."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-rst (&optional a s v b e)
  "Export to rst."
  (interactive) (org-pandoc-export 'rst a s v b e))

;;;###autoload
(defun org-pandoc-export-to-rst-and-open (&optional a s v b e)
  "Export to rst and open."
  (interactive) (org-pandoc-export 'rst a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-rst (&optional a s v b e)
  "Export as rst."
  (interactive) (org-pandoc-export 'rst a s v b e t))

(defcustom org-pandoc-options-for-rtf nil
  "Pandoc options for rtf."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-rtf-hook nil
  "Hook called after processing rtf."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-rtf (&optional a s v b e)
  "Export to rtf."
  (interactive) (org-pandoc-export 'rtf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-rtf-and-open (&optional a s v b e)
  "Export to rtf and open."
  (interactive) (org-pandoc-export 'rtf a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-rtf (&optional a s v b e)
  "Export as rtf."
  (interactive) (org-pandoc-export 'rtf a s v b e t))

(defcustom org-pandoc-options-for-s5 nil
  "Pandoc options for s5."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-s5-hook nil
  "Hook called after processing s5."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-s5 (&optional a s v b e)
  "Export to s5."
  (interactive) (org-pandoc-export 's5 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-s5-and-open (&optional a s v b e)
  "Export to s5 and open."
  (interactive) (org-pandoc-export 's5 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-s5 (&optional a s v b e)
  "Export as s5."
  (interactive) (org-pandoc-export 's5 a s v b e t))

(defcustom org-pandoc-options-for-slideous nil
  "Pandoc options for slideous."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-slideous-hook nil
  "Hook called after processing slideous."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-slideous (&optional a s v b e)
  "Export to slideous."
  (interactive) (org-pandoc-export 'slideous a s v b e))

;;;###autoload
(defun org-pandoc-export-to-slideous-and-open (&optional a s v b e)
  "Export to slideous and open."
  (interactive) (org-pandoc-export 'slideous a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-slideous (&optional a s v b e)
  "Export as slideous."
  (interactive) (org-pandoc-export 'slideous a s v b e t))

(defcustom org-pandoc-options-for-slidy nil
  "Pandoc options for slidy."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-slidy-hook nil
  "Hook called after processing slidy."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-slidy (&optional a s v b e)
  "Export to slidy."
  (interactive) (org-pandoc-export 'slidy a s v b e))

;;;###autoload
(defun org-pandoc-export-to-slidy-and-open (&optional a s v b e)
  "Export to slidy and open."
  (interactive) (org-pandoc-export 'slidy a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-slidy (&optional a s v b e)
  "Export as slidy."
  (interactive) (org-pandoc-export 'slidy a s v b e t))

(defcustom org-pandoc-options-for-tei nil
  "Pandoc options for tei."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-tei-hook nil
  "Hook called after processing tei."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-tei (&optional a s v b e)
  "Export to tei."
  (interactive) (org-pandoc-export 'tei a s v b e))

;;;###autoload
(defun org-pandoc-export-to-tei-and-open (&optional a s v b e)
  "Export to tei and open."
  (interactive) (org-pandoc-export 'tei a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-tei (&optional a s v b e)
  "Export as tei."
  (interactive) (org-pandoc-export 'tei a s v b e t))

(defcustom org-pandoc-options-for-texinfo nil
  "Pandoc options for texinfo."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-texinfo-hook nil
  "Hook called after processing texinfo."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-texinfo (&optional a s v b e)
  "Export to texinfo."
  (interactive) (org-pandoc-export 'texinfo a s v b e))

;;;###autoload
(defun org-pandoc-export-to-texinfo-and-open (&optional a s v b e)
  "Export to texinfo and open."
  (interactive) (org-pandoc-export 'texinfo a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-texinfo (&optional a s v b e)
  "Export as texinfo."
  (interactive) (org-pandoc-export 'texinfo a s v b e t))

(defcustom org-pandoc-options-for-textile nil
  "Pandoc options for textile."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-textile-hook nil
  "Hook called after processing textile."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-textile (&optional a s v b e)
  "Export to textile."
  (interactive) (org-pandoc-export 'textile a s v b e))

;;;###autoload
(defun org-pandoc-export-to-textile-and-open (&optional a s v b e)
  "Export to textile and open."
  (interactive) (org-pandoc-export 'textile a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-textile (&optional a s v b e)
  "Export as textile."
  (interactive) (org-pandoc-export 'textile a s v b e t))

(defcustom org-pandoc-options-for-zimwiki nil
  "Pandoc options for zimwiki."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-zimwiki-hook nil
  "Hook called after processing zimwiki."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-zimwiki (&optional a s v b e)
  "Export to zimwiki."
  (interactive) (org-pandoc-export 'zimwiki a s v b e))

;;;###autoload
(defun org-pandoc-export-to-zimwiki-and-open (&optional a s v b e)
  "Export to zimwiki and open."
  (interactive) (org-pandoc-export 'zimwiki a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-zimwiki (&optional a s v b e)
  "Export as zimwiki."
  (interactive) (org-pandoc-export 'zimwiki a s v b e t))


;;; ox-pandoc main routine

(defvar org-pandoc-format nil)
(defvar org-pandoc-option-table nil)
(defvar org-pandoc-format-extensions-str nil)
(defvar org-pandoc-epub-meta nil)
(defvar org-pandoc-epub-css nil)

(defun org-pandoc-export (format a s v b e &optional buf-or-open)
  "General interface for Pandoc Export.
If BUF-OR-OPEN is nil, output to file.  0, then open the file.
t means output to buffer."
  (unless (derived-mode-p 'org-mode)
    (error "This command must be run on an org-mode buffer"))
  (unless (executable-find org-pandoc-command)
    (error "Pandoc (version 1.12.4 or later) can not be found"))
  (setq org-pandoc-format format)
  (let ((org-element-keyword-translation-alist org-pandoc-element-keyword-translation-alist))
	(org-export-to-file 'pandoc (org-export-output-file-name
									  (concat (make-temp-name ".tmp") ".org") s)
		   a s v b e (lambda (f) (org-pandoc-run-to-buffer-or-file f format s buf-or-open)))))

(defun org-pandoc--has-caption-p (element _info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal'."
  (org-element-property :caption element))


(defun org-pandoc--numbered-equation-p (element _info)
  "Non-nil when ELEMENT is a numbered latex equation environment.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal'."
  (let ((raw-value (org-element-property :value element))
        (case-fold-search t))
    (string-match-p
     (rx "\\begin{"
         (group-n 1 (or "align" "alignat" "eqnarray" "equation"
                        "flalign" "gather" "multline"))
         "}" (*? anything)
         "\\end{" (backref 1) "}" )
     raw-value)))

(defun org-pandoc-entity (entity _contents _info)
  "Transcode an ENTITY object for export with pandoc.
Support org's default entities ('org-entities') and user-defined
ones ('org-entities-user'). If the final format which pandoc will
produce is a TeX-based format, use the LaTeX representation.For
all other formats, output the UTF-8 symbol and let pandoc
handle its representation in the final format.
CONTENTS are the definition itself. INFO is a plist holding
contextual information."
  (if (member org-pandoc-format '(beamer beamer-pdf latex latex-pdf))
	  (org-element-property :latex entity)
	(org-element-property :utf-8 entity)))

(defun org-pandoc-latex-environ (latex-env contents info)
  "Transcode a latex environment for export with pandoc.
Works around a bug in
pandoc (https://github.com/jgm/pandoc/issues/1764, present in at
least up-to and including pandoc 1.18) to surround an AMSMath
latex environment in BLOB with plain TeX equation block
delimiters, '$$ .. $$' in order for pandoc to properly recognise
the maths environment as a latex equation.  Also adds surrounding
line-breaks so that pandoc treats the math environment as its own
paragraph.  This avoids having text before or after the math
environment ending up on the same line as the equation.
Additionally, adds a fake equation number if the environment
should have one.  CONTENTS is its contents, as a string or nil.
INFO is a plist holding contextual information."
  (let ((raw-value (org-export-expand latex-env contents t))
        (case-fold-search t)
        (replacement-str "\n$$\\1$$")
        (output nil))
    ;; If we're exporting to a TeX-based format, there's no need for
    ;; this hack
    (if (member org-pandoc-format '(beamer beamer-pdf latex latex-pdf))
        raw-value
      ;; Otherwise, add '$$' elements before and after the block to get
      ;; pandoc to process it.
      ;;
      ;; For numbered equation environments, we need to fake the
      ;; equation numbering before sending it to pandoc. Fake equation
      ;; numbers are typeset as "(%d)", offset from the main equation by
      ;; a "\qquad" space, ala pandoc-crossref
      ;; (https://github.com/lierdakil/pandoc-crossref)
      (when (org-pandoc--numbered-equation-p latex-env info)
          (setq replacement-str
                (format "\n$$\\1$$")))

      ;; For equations with a named links target (`#+NAME:' block), add
      ;; the target to the top of the equation
      (let ((name (org-element-property :name latex-env)))
        (when name
          (setq replacement-str
                (concat "\n<<" name ">>" replacement-str))))

      ;; Add '$$' elements before and after the block to get pandoc to
      ;; process it.
      (setq output
            (replace-regexp-in-string
             (rx (group-n 1 "\\begin{"
                          (group-n 2 (or "align" "alignat" "eqnarray"
                                         "equation" "flalign" "gather"
                                         "multline")
                                   (zero-or-one "*" ))
                          "}" (*? anything)
                          "\\end{" (backref 2) "}" ))
             replacement-str
             raw-value))

      ;; If we've added the '$$' delimiters, then also set the
      ;; :post-blank property to add a blank line after this current
      ;; latex equation environment
      (let ((post-blank (org-element-property :post-blank latex-env)))
        (unless (or (>= post-blank 1) (string-equal raw-value output))
          (org-element-put-property latex-env :post-blank 1)))

      ;; Return the latex equation with '$$' delimiters and possible
      ;; (faked) equation numbering.
      output)))

(defun org-pandoc-link (link contents info)
  "Transcode a LINK object.

The registered formatter for the 'pandoc backend is used. If none
exists, transcode using the registered formatter for the 'org
export backend. For fuzzy (internal) links, resolve the link
destination in order to determine the appropriate reference
number of the target Table/Figure/Equation etc. CONTENTS is the
description of the link, as a string, or nil. INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type link)))
    (cond
     ;; Try exporting with a registered formatter for 'pandoc
     ((org-export-custom-protocol-maybe link contents 'pandoc))
     ;; Try exporting with a registered formatter for 'org
     ((org-export-custom-protocol-maybe link contents 'org))

     ;; Otherwise, override fuzzy (internal) links that point to
     ;; numbered items such as Tables, Figures, Sections, etc.
     ((string= type "fuzzy")
      (let* ((path (org-element-property :path link))
             (destination (org-export-resolve-fuzzy-link link info))
             (dest-type (when destination (org-element-type destination)))
             (number nil))
        ;; Different link targets require different predicates to the
        ;; `org-export-get-ordinal' function in order to resolve to
        ;; the correct number. NOTE: Should be the same predicate
        ;; function as used to generate the number in the
        ;; caption/label/listing etc.
        (cond
         ((eq dest-type 'paragraph)   ; possible figure
          (setq number (org-export-get-ordinal
                        destination info nil #'org-html-standalone-image-p)))

         ((eq dest-type 'latex-environment)   ; Latex block, equation or similar
          (setq number (org-export-get-ordinal
                        destination info nil #'org-pandoc--numbered-equation-p)))

         ((string-prefix-p "citeproc_bib_item" path) ; Rendered citation footnote number
		  ) ;; leave it alone, it should already be correct
		 
         (t                           ; captioned items
          (setq number (org-export-get-ordinal
                        destination info nil #'org-pandoc--has-caption-p))))

        ;; Numbered items have the number listed in the link
        ;; description, , fall back on the text in `contents'
        ;; if there's no resolvable destination
        (cond
         ;; Numbered items have the number listed in the link description
         (number
          (format "[[#%s][%s]]" path
                  (if (atom number) (number-to-string number)
                    (mapconcat #'number-to-string number "."))))

         ;; Unnumbered headlines have the heading name in the link
         ;; description
         ((eq dest-type 'headline)
          (format "[[#%s][%s]]" path
                  (org-export-data
                   (org-element-property :title destination) info)))

         ;; No resolvable destination, fallback on the text in `contents'
         ((eq destination nil)
          (when (org-string-nw-p contents) contents))

         ;; Valid destination, but without a numbered caption/equation
         ;; and not a heading, fallback to standard org-mode link format
         (t
          (org-element-link-interpreter link contents)))))

     ;; Otherwise, fallback to standard org-mode link format
     ((org-element-link-interpreter link contents)))))

(defun org-pandoc-table (table contents info)
  "Transcode a TABLE element from Org to Pandoc.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  ;; Export the table
  (org-export-expand table contents t))

(defun org-pandoc-template (contents info)
  "Template processor for CONTENTS and INFO.
Option table is created in this stage."
  (setq org-pandoc-option-table (make-hash-table))
  ;; default options
  (org-pandoc-put-options org-pandoc-options)
  (org-pandoc-put-options
   (symbol-value (intern (format "org-pandoc-options-for-%s" org-pandoc-format))))
  ;; file options
  (-when-let (pandoc-options (plist-get info :pandoc-options))
    (org-pandoc-put-options
     (--map (let* ((_match (string-match "^\\([^:]+\\):\\(.+\\)$" it))
                   (name (intern (match-string 1 it)))
                   (value (match-string 2 it)))
              (cons name value))
            (split-string-and-unquote pandoc-options))))
  (setq org-pandoc-format-extensions-str
        (mapcar 'symbol-name org-pandoc-format-extensions))
  (-when-let (pandoc-extensions (plist-get info :pandoc-extensions))
    (dolist (extension (split-string pandoc-extensions))
      (push extension org-pandoc-format-extensions-str)))
  (setq org-pandoc-epub-css (plist-get info :epub-css))
  (setq org-pandoc-epub-meta
        (or (plist-get info :epub-meta)
            (concat
             (-when-let (epub-rights (or (plist-get info :epub-rights)
                                         org-pandoc-epub-rights))
               (concat "<dc:rights>" (url-insert-entities-in-string
                                      epub-rights) "</dc:rights>\n"))
             (-when-let (description (plist-get info :description))
               (concat "<dc:description>" description "</dc:description>\n"))
             (-when-let (keywords (plist-get info :keywords))
               (concat "<dc:subject>" keywords "</dc:subject>\n")))))
  (org-pandoc-put-options
   (--mapcat (-when-let (val (plist-get info (cdr it)))
               (list (cons (car it) (split-string-and-unquote val))))
             '((metadata . :pandoc-metadata)
               (variable . :pandoc-variables))))
  (let ((org-pandoc-valid-options
		 '((epub-embed-font .    :epub-embed-font)
               (epub-chapter-level . :epub-chapter-level)
               (epub-cover-image   . :epub-cover-image)
               (epub-stylesheet    . :epub-stylesheet)
               ;;(bibliography .       :bibliography)
			   )))
	;; Bibliography option only for org < 9.5
	(unless (featurep 'oc)
	  (setcar org-pandoc-valid-options '(bibliography . :bibliography) ))
	(org-pandoc-put-options
	 (--mapcat (-when-let (val (plist-get info (cdr it)))
               (list (cons (car it) (split-string val "\n"))))
			   org-pandoc-valid-options)))
  ;; 'ox-pandoc' is derived from 'ox-org'. If 'ox-org' defines its own
  ;; template, then this template function (org-pandoc-template) calls
  ;; original ox-org template at the end.
  (let ((org-template
         (cdr (assoc 'template
                     (org-export-get-all-transcoders 'org)))))
    (if org-template
        (funcall org-template contents info)
    contents)))

(defun org-pandoc-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Pandoc.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  ;; Export the paragraph verbatim. Like `org-org-identity', but also
  ;; preserves #+ATTR_* tags in the output.
  (org-export-expand paragraph contents t))

(defun org-pandoc-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Pandoc.
CONTENTS is the contents of the table. INFO is a plist holding
contextual information."
  ;; Export the src-block
  (org-export-expand src-block contents t))

(defun org-pandoc-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Pandoc.
This might be EXPORT_HTML or EXPORT_LATEX, and is simply
duplicated into the temporary org file that pandoc converts.
Special: if the block is BEGIN_EXPORT pandoc, pass the value to
pandoc. CONTENTS is the contents of the block. INFO is a plist
holding contextual information."
  (if  (string= (org-element-property :type export-block) "PANDOC")
	  (org-element-property :value export-block)
	(org-pandoc-identity export-block contents info)))

(defun org-pandoc-export-snippet (export-snippet contents info)
  "Transcode a @@format:snippet@@ from Org to Pandoc.
If it is an output format, such as latex or html, the snippet is
duplicated in full for pandoc to handle as Org's own exporters
would. If the snippet specifies 'pandoc' as the format, the inner
content of the snippet is passed to pandoc. CONTENTS is the
contents of the block. INFO is a plist holding contextual
information."
  (if (eq (org-export-snippet-backend export-snippet) 'pandoc)
	  (org-element-property :value export-snippet)
	(org-pandoc-identity export-snippet contents info)))

(defun org-pandoc-identity (blob contents _info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.
Like `org-org-identity', but also preserves #+ATTR_* tags in the
output."
  (org-export-expand blob contents t))

(defun org-pandoc-put-options (options)
  "Put alist OPTIONS to `org-pandoc-option-table'."
  (dolist (option options)
    (let* ((name (car option))
           (value (cdr option))
           (values
            (cond ((not (memq name org-pandoc-valid-options))
                   (error "Org-Pandoc: Improper Option Name! %s" name))
                  ((equal "t" value) t)
                  ((equal "nil" value) nil)
                  ((listp value) value)
                  ((memq name org-pandoc-colon-separated-options)
                   (split-string value ":"))
                  (t (list value)))))
      (if (memq name org-pandoc-file-options)
          (setq values
                (--map (if (file-exists-p it)
                           (if (= ?~ (string-to-char it)) (expand-file-name it) it)
                         (error "File (%s) can not be found" it)) values)))
      (puthash name values org-pandoc-option-table))))

(defun org-pandoc-run-to-buffer-or-file
    (input-file format subtreep &optional buffer-or-open)
  "Run pandoc to convert on org INPUT-FILE or buffer to FORMAT with SUBTREEP.

If BUFFER-OR-OPEN is t then target is current buffer. If nil, target is file.
If 0, target is file and converted file will automatically be opend."
  (let* ((pdf-p (string-match "-pdf$" (symbol-name format)))
         (output-file
          (unless (equal t buffer-or-open)
            (org-export-output-file-name
             (concat "."
                     (if pdf-p "pdf"
                       (symbol-name
                        (or (assoc-default format org-pandoc-extensions)
                            format))))
             subtreep)))
         (local-hook-symbol (intern (format "org-pandoc-after-processing-%s-hook"
                                            format)))
         css-temp-file meta-temp-file)
    ;; convert format "XXX-pdf" to "XXX"
    (when pdf-p (setq format (intern (substring (symbol-name format) 0 -4))))
    ;; special treatment for epub2/epub3
    (when (or (equal org-pandoc-format 'epub2)
              (equal org-pandoc-format 'epub3))
      (when org-pandoc-epub-css
        (setq css-temp-file (make-temp-file "org-pandoc" nil ".css"))
        (puthash 'epub-stylesheet
                 (append (gethash 'epub-stylesheet org-pandoc-option-table)
                         (list css-temp-file))
                 org-pandoc-option-table)
        (with-temp-file css-temp-file
          (insert org-pandoc-epub-css)))
      (when org-pandoc-epub-meta
        (setq meta-temp-file (make-temp-file "org-pandoc" nil ".xml"))
        (org-pandoc-put-options `((epub-metadata ,meta-temp-file)))
        (with-temp-file meta-temp-file
          (insert org-pandoc-epub-meta))))
    (let ((process
           (org-pandoc-run input-file output-file format
                           'org-pandoc-sentinel org-pandoc-option-table)))
      (process-put process 'files (list input-file meta-temp-file css-temp-file))
      (process-put process 'output-file output-file)
      (process-put process 'local-hook-symbol local-hook-symbol)
      (process-put process 'buffer-or-open buffer-or-open))))

(defun org-pandoc-sentinel (process message)
  "PROCESS sentinel with MESSAGE.
Called on completion of an asynchronous pandoc process."
  (cl-case (process-status process)
    (run)
    (signal
     ;; Warning.  Temporary files not removed (for now.)
     (display-warning 'ox-pandoc (format "Signal Received. %s" message)))
    (exit
     (dolist (file (process-get process 'files))
	   (if (and file (file-exists-p file)) (delete-file file))
	   )
     (let ((exit-status (process-exit-status process))
           (buffer (process-buffer process))
           (output-file (process-get process 'output-file))
           (local-hook-symbol (process-get process 'local-hook-symbol))
           (buffer-or-open (process-get process 'buffer-or-open)))
       (if (/= exit-status 0)
           (message "Error occured. \n%s"
                    (with-current-buffer buffer (buffer-string)))
         (if output-file
             (progn
               (kill-buffer buffer)
               (message "Exported to %s." output-file)
               (if (and (boundp local-hook-symbol)
                        (symbol-value local-hook-symbol))
                   (with-temp-file output-file
                     (insert-file-contents output-file)
                     (run-hooks local-hook-symbol)))
               (when (equal 0 buffer-or-open)
                 (org-open-file output-file)))
           ;; output to buffer
           (pop-to-buffer buffer)
		   (goto-char (point-min))
           (run-hooks local-hook-symbol)
           (set-auto-mode)))))))

(defun org-pandoc-run (input-file output-file format sentinel &optional options)
  "Run pandoc command with INPUT-FILE (org), OUTPUT-FILE, FORMAT and OPTIONS.
If BUFFER-OR-FILE is buffer, then output to specified buffer. OPTIONS is
a hashtable.  Pandoc runs asynchronously and SENTINEL is called
when the process completes."
  (let* ((format (symbol-name format))
         (output-format
          (car (--filter (string-prefix-p format it)
                         org-pandoc-format-extensions-str)))
         (args
          `("-f" "org"
            "-t" ,(or output-format format)
            ,@(and output-file
                   (list "-o" (expand-file-name output-file)))
            ,@(-mapcat (lambda (key)
                         (-when-let (vals (gethash key options))
                           (if (equal vals t) (setq vals (list t)))
                           (--map (concat "--" (symbol-name key)
                                          (when (not (equal it t)) (format "=%s" it)))
                                  vals)))
                       (ht-keys options))
            ,(expand-file-name input-file))))
    (message "Running pandoc with args: %s" args)
    (let ((process
           (apply 'start-process
                  `("pandoc" ,(generate-new-buffer "*Pandoc*")
                    ,org-pandoc-command ,@args))))
      (set-process-sentinel process sentinel)
      process)))

(defun org-pandoc-startup-check ()
  "Check the current pandoc version."
  (interactive)
  (catch 'check-suppressed
	(unless org-pandoc-check-version
	  (throw 'check-suppressed nil))
  (if (not (executable-find org-pandoc-command))
      (display-warning 'ox-pandoc "Pandoc command is not installed.")
    (let ((version (with-temp-buffer
                    (call-process org-pandoc-command nil t nil "-v")
                    (buffer-string))))
      (if (not (string-match "^pandoc.*? \\([0-9]+\\)\\.\\([0-9]+\\)" version))
          (display-warning 'ox-pandoc "Pandoc version number can not be retrieved.")
        (let ((major (string-to-number (match-string 1 version)))
              (_minor (string-to-number (match-string 2 version))))
          (unless (or (< 1 major)
                      ;;(and (= 1 major)
                      ;;     (< 12 minor))
                      )
            (display-warning 'ox-pandoc "This Pandoc (1.x) may not support new pandoc features."))))))))

(org-pandoc-startup-check)

(provide 'ox-pandoc)

;;; ox-pandoc.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
