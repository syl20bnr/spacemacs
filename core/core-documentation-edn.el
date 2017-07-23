;;; core-documentation-edn.el --- Spacemacs Core File -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017 Sylvain Benner & Contributors

;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; File structure was borrowed from ox-html.el by (Carsten Dominik
;; <carsten at orgmode dot org> and Jambunathan K
;; <kjambunathan at gmail dot com>).

;; This file is not part of GNU Emacs.

;;; Code:

(require 'url-util)
(require 'toc-org)
(require 'ox-publish)
(require 'dash)
(require 'f)

(defvar spacemacs--org-export-define-backend-funcs-alist
  '((bold . spacemacs//org-edn-bold)
    (center-block . spacemacs//org-edn-center-block)
    (clock . spacemacs//org-edn-clock)
    (code . spacemacs//org-edn-code)
    (drawer . spacemacs//org-edn-drawer)
    (dynamic-block . spacemacs//org-edn-dynamic-block)
    (entity . spacemacs//org-edn-entity)
    (example-block . spacemacs//org-edn-example-block)
    (export-block . spacemacs//org-edn-export-block)
    (export-snippet . spacemacs//org-edn-export-snippet)
    (fixed-width . spacemacs//org-edn-fixed-width)
    (footnote-definition . spacemacs//org-edn-footnote-definition)
    (footnote-reference . spacemacs//org-edn-footnote-reference)
    (headline . spacemacs//org-edn-headline)
    (horizontal-rule . spacemacs//org-edn-horizontal-rule)
    (inline-src-block . spacemacs//org-edn-inline-src-block)
    (inlinetask . spacemacs//org-edn-inlinetask)
    (inner-template . spacemacs//org-edn-inner-template)
    (italic . spacemacs//org-edn-italic)
    (item . spacemacs//org-edn-item)
    (keyword . spacemacs//org-edn-keyword)
    (latex-environment . spacemacs//org-edn-latex-environment)
    (latex-fragment . spacemacs//org-edn-latex-fragment)
    (line-break . spacemacs//org-edn-line-break)
    (link . spacemacs//org-edn-link)
    (node-property . spacemacs//org-edn-node-property)
    (paragraph . spacemacs//org-edn-paragraph)
    (plain-list . spacemacs//org-edn-plain-list)
    (plain-text . spacemacs//org-edn-plain-text)
    (planning . spacemacs//org-edn-planning)
    (property-drawer . spacemacs//org-edn-property-drawer)
    (quote-block . spacemacs//org-edn-quote-block)
    (radio-target . spacemacs//org-edn-radio-target)
    (section . spacemacs//org-edn-section)
    (special-block . spacemacs//org-edn-special-block)
    (src-block . spacemacs//org-edn-src-block)
    (statistics-cookie . spacemacs//org-edn-statistics-cookie)
    (strike-through . spacemacs//org-edn-strike-through)
    (subscript . spacemacs//org-edn-subscript)
    (superscript . spacemacs//org-edn-superscript)
    (table . spacemacs//org-edn-table)
    (table-cell . spacemacs//org-edn-table-cell)
    (table-row . spacemacs//org-edn-table-row)
    (target . spacemacs//org-edn-target)
    (template . spacemacs//org-edn-template)
    (timestamp . spacemacs//org-edn-timestamp)
    (underline . spacemacs//org-edn-underline)
    (verbatim . spacemacs//org-edn-verbatim)
    (verse-block . spacemacs//org-edn-verse-block))
  "plist of transcode functions names for `spacemacs-edn' backend.")

(org-export-define-backend 'spacemacs-edn
  spacemacs--org-export-define-backend-funcs-alist
  :filters-alist
  '((:filter-final-output . spacemacs//org-edn-final-function)))


;;; Transcode Functions

;;;; Bold

(defun spacemacs//org-edn-bold (_bold contents _info)
  "Transcode BOLD From Org to Spacemacs EDN.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-bold{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-bold)

;;;; Center Block

(defun spacemacs//org-edn-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-center-block{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-center-block)

;;;; Clock

(defun spacemacs//org-edn-clock (_clock _contents _info)
  "Transcode a CLOCK element From Org to Spacemacs EDN.))
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-clock")
  "")
(byte-compile 'spacemacs//org-edn-clock)

;;;; Code

(defun spacemacs//org-edn-code (code _contents _info)
  "Transcode CODE From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information.
NOTE: In Spacemacs ~code blocks~ are key sequences."
  (format "#Spacemacs/Org-kbd{:value %s}"
          (format "%S"
                  (vconcat
                   (mapcar
                    'spacemacs/org-edn-escape-string
                    (split-string
                     (org-element-property :value code)
                     " "
                     "\\s*"))))))
(byte-compile 'spacemacs//org-edn-code)

;;;; Drawer

(defun spacemacs//org-edn-drawer (_drawer _contents _info)
  "Transcode a DRAWER element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-drawer")
  "")
(byte-compile 'spacemacs//org-edn-drawer)

;;;; Dynamic Block

(defun spacemacs//org-edn-dynamic-block (_dynamic-block _contents _info)
  "Transcode a DYNAMIC-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-dynamic-block")
  "")
(byte-compile 'spacemacs//org-edn-dynamic-block)

;;;; Entity

(defun spacemacs//org-edn-entity (_entity _contents _info)
  "Transcode an ENTITY object From Org to Spacemacs EDN.))
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-entity")
  "")
(byte-compile 'spacemacs//org-edn-entity)

;;;; Example Block

(defun spacemacs//org-edn-example-block (example-block _contents _info)
  "Transcode a EXAMPLE-BLOCK element From Org to Spacemacs EDN.))
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "#Spacemacs/Org-example-block{:value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :value example-block))))
(byte-compile 'spacemacs//org-edn-example-block)

;;;; Export Block

(defun spacemacs//org-edn-export-block (_export-block _contents _info)
  "Transcode a EXPORT-BLOCK element From Org to Spacemacs EDN.))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-export-block")
  "")
(byte-compile 'spacemacs//org-edn-export-block)

;;;; Export Snippet

(defun spacemacs//org-edn-export-snippet (_export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object From Org to Spacemacs EDN.))
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-export-snippet")
  "")
(byte-compile 'spacemacs//org-edn-export-snippet)

;;;; Fixed Width

(defun spacemacs//org-edn-fixed-width (_fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element From Org to Spacemacs EDN.))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-fixed-width")
  "")
(byte-compile 'spacemacs//org-edn-fixed-width)

;;;; Footnote Reference

(defun spacemacs//org-edn-footnote-reference
    (_footnote-reference _contents _info)
  "Transcode a FOOTNOTE-REFERENCE element From Org to Spacemacs EDN.))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-footnote-reference")
  "")
(byte-compile 'spacemacs//org-edn-footnote-reference)

;;;; Headline

(defun spacemacs//org-edn-headline (headline contents info)
  "Transcode a HEADLINE element From Org to Spacemacs EDN.))
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((raw-value (org-element-property :raw-value headline))
         (headline-ht (if (plist-member info :headline-hash)
                          (plist-get info :headline-hash)
                        (let ((hh (make-hash-table :test 'equal)))
                          (plist-put info :headline-hash hh)
                          hh)))
         (gh-id (toc-org-hrefify-gh raw-value headline-ht))
         (level (org-element-property :level headline)))
    (when (and (= level 1)
               (string= raw-value "Description"))
      (if (plist-member info :file-has-description?)
          (error (concat "File \"%s\" has multiply top level "
                         "\"Description\" headlines")
                 (plist-get info :input-file))
        (plist-put info :file-has-description? 'true)))
    (puthash gh-id raw-value headline-ht)
    (format (concat "#Spacemacs/Org-headline{"
                    ":value \"%s\" "
                    ":gh-id \"%s\" "
                    ":nesting-id \"%s\" "
                    ":level %s "
                    ":contents [%s]}")
            (spacemacs/org-edn-escape-string raw-value)
            (spacemacs/org-edn-escape-string (string-remove-prefix "#" gh-id))
            (spacemacs/org-edn-escape-string
             (spacemacs/org-edn-headline-make-nesting-id headline))
            level
            contents)))
(byte-compile 'spacemacs//org-edn-headline)

;;;; Horizontal Rule

(defun spacemacs//org-edn-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE  object From Org to Spacemacs EDN.)))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-horizontal-rule")
  "")
(byte-compile 'spacemacs//org-edn-horizontal-rule)

;;;; Inline Src Block

(defun spacemacs//org-edn-inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-inline-src-block")
  "")
(byte-compile 'spacemacs//org-edn-inline-src-block)

;;;; Inlinetask

(defun spacemacs//org-edn-inlinetask (_inlinetask _contents _info)
  "Transcode an INLINETASK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-inlinetask")
  "")
(byte-compile 'spacemacs//org-edn-inlinetask)

;;;; Inner Template

(defun spacemacs//org-edn-inner-template (contents _info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (format "#Spacemacs/Org-inner-template{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-inner-template)

;;;; Italic

(defun spacemacs//org-edn-italic (_italic contents _info)
  "Transcode ITALIC From Org to Spacemacs EDN.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-italic{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-italic)

;;;; Item

(defun spacemacs//org-edn-item (item contents info)
  "Transcode an ITEM element From Org to Spacemacs EDN.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((type (org-element-property
               :type
               (org-export-get-parent item))))
    (unless (or (eq 'ordered type)
                (eq 'unordered type)
                (eq 'descriptive type))
      (error (concat "File \"%s\" contains plain list item of type \"%s\" but "
                     "it isn't implemented in spacemacs//org-edn-item")
             (plist-get info :input-file)
             type))
    (format (concat "#Spacemacs/Org-item{"
                    ":type %s "
                    ":bullet %s "
                    ":counter %s "
                    ":checkbox %s "
                    ":tag \"%s\" "
                    ":contents [%s]}")
            type
            (org-element-property :bullet item)
            (org-element-property :counter item)
            (org-element-property :checkbox item)
            (spacemacs/org-edn-escape-string
             (format "%s" (org-element-property :tag item)))
            contents)))
(byte-compile 'spacemacs//org-edn-item)

;;;; Keyword

(defun spacemacs//org-edn-keyword (keyword _contents _info)
  "Transcode a KEYWORD element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "#Spacemacs/Org-keyword{:key \"%s\" :value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :key keyword))
          (spacemacs/org-edn-escape-string
           (org-element-property :value keyword))))
(byte-compile 'spacemacs//org-edn-keyword)

;;;; Latex Environment

(defun spacemacs//org-edn-latex-environment (_latex-environment _contents _info)
  "Transcode a LATEX-ENVIRONMENT element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-latex-environment")
  "")
(byte-compile 'spacemacs//org-edn-latex-environment)

;;;; Latex Fragment

(defun spacemacs//org-edn-latex-fragment (_latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-latex-fragment")
  "")
(byte-compile 'spacemacs//org-edn-latex-fragment)

;;;; Line Break

(defun spacemacs//org-edn-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "#Spacemacs/Org-line-break{}")
(byte-compile 'spacemacs//org-edn-line-break)

;;;; Link

(defconst spacemacs--org-edn-git-url-root-regexp
  (format (concat "\\/\\/github\\.com\\/%s\\/%s\\/blob"
                  "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")
          (or spacemacs-repository-owner "syl20bnr")
          (or spacemacs-repository "spacemacs")))

(defun spacemacs//org-edn-link (link desc info)
  "Transcode a LINK object From Org to Spacemacs EDN.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (file (plist-get info :input-file))
         (raw-link (org-element-property :raw-link link))
         (local-link? (string= type "file"))
         (local-org-link? (and local-link?
                               (string-match-p
                                ".*\\.org\\(\\(::\\|#\\| \\).*\\)?$"
                                raw-link))))
    (when local-org-link? (plist-put
                           info
                           :spacemacs-edn-warnings
                           (concat
                            (plist-get info :spacemacs-edn-warnings)
                            (format (concat "The link \"%s\" "
                                            "in the file \"%s\" "
                                            "should target the org file at "
                                            "GitHub "
                                            "(anchors are supported)\n")
                                    raw-link
                                    file))))
    (cond ((string-match spacemacs--org-edn-git-url-root-regexp
                         raw-link)
           (let ((link (match-string 1 raw-link))
                 (target-id (string-remove-prefix
                             "#"
                             (match-string 2 raw-link))))
             (format (concat "#Spacemacs/Org-org-file-path{"
                             ":value \"%s\" "
                             ":raw-link \"%s\" "
                             ":target-headline-gh-id \"%s\" "
                             ":description [%s]}")
                     (spacemacs/org-edn-escape-string
                      (concat spacemacs-start-directory
                              (url-unhex-string link)))
                     (spacemacs/org-edn-escape-string
                      raw-link)
                     (spacemacs/org-edn-escape-string
                      (url-unhex-string target-id))
                     desc)))
          (local-link?
           (format "#Spacemacs/Org-file-path{:value \"%s\" :description [%s]}"
                   (spacemacs/org-edn-escape-string path)
                   desc))
          ((or (string= type "http")
               (string= type "https")
               (string= type "ftp"))
           (format "#Spacemacs/Org-web-link{:value \"%s\" :description [%s]}"
                   (spacemacs/org-edn-escape-string raw-link)
                   desc))
          ((string= type "custom-id")
           (format (concat "#Spacemacs/Org-internal-link{"
                           ":target-headline-gh-id \"%s\" :description [%s]}")
                   (spacemacs/org-edn-escape-string raw-link)
                   desc))
          (t (error
              (concat
               "Link \"%s\" in file \"%s\" "
               "has type \"%s\" "
               "but the type isn't implemented in spacemacs//org-edn-link")
              raw-link
              file
              type)))))
(byte-compile 'spacemacs//org-edn-link)

;;;; Node Property

(defun spacemacs//org-edn-node-property (_node-property _contents _info)
  "Transcode a NODE-PROPERTY element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-node-property")
  "")
(byte-compile 'spacemacs//org-edn-node-property)

;;;; Paragraph

(defun spacemacs//org-edn-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element From Org to Spacemacs EDN.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (format "#Spacemacs/Org-paragraph{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-paragraph)

;;;; Plain List

(defun spacemacs//org-edn-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element From Org to Spacemacs EDN.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (parent-hl
          (org-export-get-parent-headline
           plain-list))
         (parent-hl-parent-hl
          (org-export-get-parent-headline
           parent-hl))
         (file (plist-get info :input-file)))
    (unless (or (eq 'ordered type)
                (eq 'unordered type)
                (eq 'descriptive type))
      (error (concat "File \"%s\" contains plain list of type \"%s\" but "
                     "it isn't implemented in spacemacs//org-edn-node-property")
             (plist-get info :input-file)
             type))
    (if (and (= (or (org-element-property
                     :level
                     parent-hl)
                    -1)
                2)
             (string= (org-element-property
                       :raw-value
                       parent-hl)
                      "Features:")
             (= (or (org-element-property
                     :level
                     parent-hl-parent-hl)
                    -1)
                1)
             (string= (org-element-property
                       :raw-value
                       parent-hl-parent-hl)
                      "Description"))
        (if (plist-member info :file-has-feature-list?)
            (error (concat "File \"%s\" has multiply "
                           "\"Features:\" lists in the top "
                           "level \"Description\" headline")
                   file)
          (plist-put info :file-has-feature-list? 'true)
          (format "#Spacemacs/Org-feature-list{:type %s :contents [%s]}"
                  type
                  contents))
      (format "#Spacemacs/Org-plain-list{:type %s :contents [%s]}"
              type
              contents))))
(byte-compile 'spacemacs//org-edn-plain-list)

;;;; Plain Text

(defun spacemacs//org-edn-plain-text (text _info)
  "Transcode a TEXT string From Org to Spacemacs EDN.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-plain-text{:text \"%s\"}"
          (spacemacs/org-edn-escape-string text)))
(byte-compile 'spacemacs//org-edn-plain-text)

;;;; Planning

(defun spacemacs//org-edn-planning (_planning _contents _info)
  "Transcode a PLANNING element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-planning")
  "")
(byte-compile 'spacemacs//org-edn-planning)

;;;; Property Drawer

(defun spacemacs//org-edn-property-drawer (_property-drawer _contents _info)
  "Transcode a PROPERTY-DRAWER element From Org to Spacemacs EDN.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-property-drawer")
  "")
(byte-compile 'spacemacs//org-edn-property-drawer)

;;;; Quote Block

(defun spacemacs//org-edn-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-quote-block{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-quote-block)

;;;; Radio Target

(defun spacemacs//org-edn-radio-target (_radio-target _text _info)
  "Transcode a RADIO-TARGET object From Org to Spacemacs EDN.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-radio-target")
  "")
(byte-compile 'spacemacs//org-edn-radio-target)

;;;; Section

(defun spacemacs//org-edn-section (_section contents _info)
  "Transcode a SECTION element From Org to Spacemacs EDN.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-section{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-section)

;;;; Special Block

(defun spacemacs//org-edn-special-block (_special-block _contents _info)
  "Transcode a SPECIAL-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-special-block")
  "")
(byte-compile 'spacemacs//org-edn-special-block)

;;;; Src Block

(defun spacemacs//org-edn-src-block (src-block _contents _info)
  "Transcode a SRC-BLOCK element From Org to Spacemacs EDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (format "#Spacemacs/Org-src-block{:language \"%s\" :value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :language src-block))
          (spacemacs/org-edn-escape-string
           (org-element-property :value src-block))))
(byte-compile 'spacemacs//org-edn-src-block)

;;;; Statistics Cookie

(defun spacemacs//org-edn-statistics-cookie (_statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-statistics-cookie")
  "")
(byte-compile 'spacemacs//org-edn-statistics-cookie)

;;;; Strike-Through

(defun spacemacs//org-edn-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH From Org to Spacemacs EDN.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-strike-through{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-strike-through)

;;;; Subscript

(defun spacemacs//org-edn-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object From Org to Spacemacs EDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-subscript{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-subscript)

;;;; Superscript

(defun spacemacs//org-edn-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object From Org to Spacemacs EDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-superscript{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-superscript)

;;;; Table

(defun spacemacs//org-edn-table (table contents _info)
  "Transcode a TABLE element From Org to Spacemacs EDN.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((type (org-element-property :type table)))
    (unless (eq type 'org)
      (error "Table type \"%s\" isn't implemented in spacemacs//org-edn-table"
             type))
    (format "#Spacemacs/Org-table{:type %s :contents [%s]}"
            (org-element-property :type table)
            contents)))
(byte-compile 'spacemacs//org-edn-table)

;;;; Table Cell

(defun spacemacs//org-edn-table-cell (_table-cell contents _info)
  "Transcode a TABLE-CELL element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "#Spacemacs/Org-table-cell{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-table-cell)

;;;; Table Row

(defun spacemacs//org-edn-table-row (_table-row contents _info)
  "Transcode a TABLE-ROW element From Org to Spacemacs EDN.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  (format "#Spacemacs/Org-table-row{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-table-row)

;;;; Target

(defun spacemacs//org-edn-target (_target _contents _info)
  "Transcode a TARGET object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-target")
  "")
(byte-compile 'spacemacs//org-edn-target)

;;;; Template

(defun spacemacs//org-edn-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((has-description?
         (plist-member info :file-has-description?))
        (has-feature-list?
         (plist-member info :file-has-feature-list?)))
    (when-let ((has-file? (plist-get info :input-file))
               (file (file-truename
                      (plist-get info :input-file))))
      (when (and (string-prefix-p (file-truename
                                   (concat
                                    spacemacs-start-directory
                                    "layers/"))
                                  file)
                 (string-suffix-p "README.org"
                                  file
                                  t))
        (unless has-description?
          (plist-put
           info
           :spacemacs-edn-warnings
           (concat
            (plist-get info :spacemacs-edn-warnings)
            (format (concat "The layer README file \"%s\" "
                            "doesn't have top level "
                            "\"Description\" headline\n")
                    file))))
        (unless has-feature-list?
          (plist-put
           info
           :spacemacs-edn-warnings
           (concat
            (plist-get info :spacemacs-edn-warnings)
            (format (concat "The layer README.org file \"%s\" "
                            "doesn't have feature list in the "
                            "top level \"Description\" headline\n")
                    file))))))
    (format (concat "#Spacemacs/Org-template{"
                    ":export-data #inst \"%s\" "
                    ":file-has-description? %s "
                    ":file-has-feature-list? %s "
                    ":contents [%s]}")
            (format-time-string "%Y-%m-%dT%H:%M:%S.52Z" nil t)
            (if (plist-member info :file-has-description?)
                'true
              'false)
            (if (plist-member info :file-has-feature-list?)
                'true
              'false)
            contents)))
(byte-compile 'spacemacs//org-edn-template)

;;;; Timestamp

(defun spacemacs//org-edn-timestamp (_timestamp _contents _info)
  "Transcode a TIMESTAMP object From Org to Spacemacs EDN.)))))))))
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (error "\"%s\" not implemented"
         "spacemacs//org-edn-timestamp")
  "")
(byte-compile 'spacemacs//org-edn-timestamp)

;;;; Underline

(defun spacemacs//org-edn-underline (_underline contents _info)
  "Transcode UNDERLINE From Org to Spacemacs EDN.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-underline{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-underline)

;;;; Verbatim

(defun spacemacs//org-edn-verbatim (verbatim _contents _info)
  "Transcode VERBATIM From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "#Spacemacs/Org-verbatim{:value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :value verbatim))))
(byte-compile 'spacemacs//org-edn-verbatim)

;;;; Verse Block

(defun spacemacs//org-edn-verse-block (_verse-block contents _info)
  "Transcode a VERSE-BLOCK element From Org to Spacemacs EDN.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-verse-block{:contents [%s]}" contents))
(byte-compile 'spacemacs//org-edn-verse-block)


;;; Helper Functions

(defconst spacemacs-org-edn-special-chars '(("\t" . "\\t")
                                            ("\r" . "\\r")
                                            ("\n" . "\\n")))
(defsubst spacemacs/org-edn-escape-string (str)
  "Escape special characters in STR"
  (if str
      (with-temp-buffer
        (insert str)
        (format-replace-strings spacemacs-org-edn-special-chars)
        (buffer-string))
    ""))

(defsubst spacemacs/org-edn-headline-make-nesting-id (headline)
  "Make id for org HEADLINE by chaining headlines from parent to
child headline.
NOTE: Each headline is converted with `toc-org-hrefify-gh' but
without unification and \"#\" prefix."
  (let* ((res nil)
         (cur-node headline)
         (parent-node (org-export-get-parent cur-node)))
    (loop
     t
     (when (eq 'headline (car-safe cur-node))
       (setq res (add-to-list 'res
                              (string-remove-prefix
                               "#"
                               (toc-org-hrefify-gh
                                (org-element-property
                                 :raw-value
                                 cur-node))))))
     (if (not parent-node)
         (return res)
       (setq cur-node parent-node
             parent-node (org-export-get-parent cur-node))))
    (mapconcat 'identity res "/")))


;;; Filter Functions

(defsubst spacemacs//org-edn-final-function-tidy (contents)
  "Filter to compact output by removing newline symbols.
FIXME: Figure out where they come from :"
  (replace-regexp-in-string "\n" "" contents))

(defsubst spacemacs//org-edn-final-function-lint (info)
  "Warn about potential errors."
  (let ((warnings (plist-get info :spacemacs-edn-warnings)))
    (when (stringp warnings)
      (warn "%s" (string-remove-suffix "\n" warnings)))))

(defun spacemacs//org-edn-final-function (contents _backend info)
  "Call final functions for `space-edn' backend"
  (spacemacs//org-edn-final-function-lint info)
  (spacemacs//org-edn-final-function-tidy contents))
(byte-compile 'spacemacs//org-edn-final-function)


;;; End-user functions

(defun spacemacs/org-edn-publish-to-spacemacs-edn (plist filename pub-dir)
  "Publish an org file to Spacemacs EDN.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'spacemacs-edn
                      filename
                      ".edn"
                      plist
                      pub-dir))
(byte-compile 'spacemacs/org-edn-publish-to-spacemacs-edn)

(defun spacemacs/publish-docs-to-edn ()
  "Publish the documentation to doc/export/."
  (interactive)
  (let* ((publish-target (concat spacemacs-start-directory "export/"))
         (org-publish-project-alist
          `(("spacemacs-edn"
             :components ("spacemacs-news"
                          "spacemacs-doc"
                          "spacemacs-doc-static"
                          "layers-doc"
                          "layers-doc-static"))
            ("spacemacs-news"
             :base-directory ,spacemacs-news-directory
             :base-extension "org"
             :publishing-directory ,(concat publish-target "news/")
             :publishing-function spacemacs/org-edn-publish-to-spacemacs-edn
             :headline-levels 4)
            ("spacemacs-doc"
             :base-directory ,spacemacs-docs-directory
             :base-extension "org"
             :publishing-directory ,(concat publish-target "doc/")
             :publishing-function spacemacs/org-edn-publish-to-spacemacs-edn
             :headline-levels 4)
            ("layers-doc"
             :base-directory ,(concat spacemacs-start-directory "layers/")
             :base-extension "org"
             :recursive t
             :publishing-directory ,(concat publish-target "layers/")
             :publishing-function spacemacs/org-edn-publish-to-spacemacs-edn
             :exclude "local\\|dockerfiles\\|LAYERS.org")
            ("spacemacs-doc-static"
             :base-directory ,spacemacs-docs-directory
             :base-extension "png"
             :recursive t
             :publishing-directory ,(concat publish-target "doc/")
             :publishing-function org-publish-attachment)
            ("layers-doc-static"
             :base-directory ,(concat spacemacs-start-directory "layers/")
             :base-extension "jpg\\|png\\|gif"
             :recursive t
             :publishing-directory ,(concat publish-target "layers/")
             :publishing-function org-publish-attachment))))
    (org-publish-project "spacemacs-edn" t)))
(byte-compile 'spacemacs/publish-docs-to-edn)

(defconst spacemacs-publish-docs-to-edn-default-exclude-re
  (regexp-opt
   (append
    (mapcar (lambda (el)
              (file-truename
               (concat
                spacemacs-start-directory
                el)))
            `("export/"
              "private/"
              "tests/"
              "elpa/"))
    (f-glob (file-truename (concat spacemacs-start-directory
                                   ".*/")))
    '("LAYERS.org"
      "CHANGELOG.org")))
  "Default exclusion regexp for `spacemacs/publish-docs-to-edn-concurrently'")

(defun spacemacs/publish-docs-to-edn-concurrently
    (&optional pub-dir inst-num exclude-re)
  "Publish org files using `org-export-to-file' and `spacemacs-edn' backend.
If PUB-DIR isn't specified - publish to \"%SPACMACS-DIR%/export/\".
INST-NUM is a number of Emacs instances used for publishing(4 if not pacified).
EXCLUDE-RE is a exclusion REGEXP. If not specified
the value of `spacemacs-publish-docs-to-edn-default-exclude-re' is used.
NOTE: In --batch mode this function will wait for all instances to finish."
  (let ((exclude-re (or exclude-re
                        spacemacs-publish-docs-to-edn-default-exclude-re))
        (pub-dir (or pub-dir (concat spacemacs-start-directory "export/")))
        (org-files-fp-list-length 0)
        (org-files-fp-list '())
        (part-in (or inst-num 4))
        (org-files-fp-lists '())
        (emacs-fp (executable-find "emacs"))
        (uname (user-login-name))
        (comp-oses '(gnu/linux darwin))
        (spacemacs-init-fp (concat spacemacs-start-directory
                                   "init.el"))
        (spacemacs-core-doc-el-fp (concat spacemacs-start-directory
                                          "core/core-documentation-edn.el"))
        (instances-finished 0))
    (unless (memq system-type comp-oses)
      (user-error
       "Sorry. For now this function can be run only on \"%s\" systems"
       comp-oses))
    (unless emacs-fp
      (error "Can't find emacs executable"))
    (dolist (org-file-fp
             (directory-files-recursively
              spacemacs-start-directory
              "\\.org$"))
      (unless (string-match-p exclude-re (file-truename org-file-fp))
        (setq  org-files-fp-list-length
               (1+  org-files-fp-list-length))
        (add-to-list 'org-files-fp-list org-file-fp)))
    (setq org-files-fp-lists (-partition (round (/ org-files-fp-list-length
                                                   part-in))
                                         org-files-fp-list))
    (call-process-shell-command
     (format "\"%s\" -u \"%s\" -batch -l \"%s\" > /dev/null 2>&1"
             emacs-fp
             uname
             spacemacs-init-fp))
    (make-directory pub-dir t)
    (dolist (file-path-group org-files-fp-lists)
      (make-process :name "spacemacs-edn-concurrent-export"
                    :sentinel (lambda (p e)
                                (if (string-match-p "finished" e)
                                    (progn
                                      (message "Process %s has finished\n" p)
                                      (setq instances-finished
                                            (1+ instances-finished))
                                      (when (and
                                             (not noninteractive)
                                             (= part-in
                                                instances-finished))
                                        (run-with-idle-timer
                                         1
                                         nil
                                         'y-or-n-p
                                         (concat "Export finished! "
                                                 "Are you happy?"))))
                                  (error "Process %s was %s"
                                         p e)
                                  ;; stop waiting
                                  (setq part-in -1)))
                    :filter (lambda (&optional p s) (princ (format "\"%s\":%s" p s)))
                    :command
                    (list
                     emacs-fp
                     "-u"
                     uname
                     "-batch"
                     "-l"
                     spacemacs-core-doc-el-fp
                     "-eval"
                     (format
                      "%S"
                      `(let ((pub-dir ,pub-dir)
                             (file-list ',file-path-group))
                         (dolist (file file-list)
                           (let* ((target-file-name
                                   (concat
                                    pub-dir
                                    (string-remove-suffix
                                     ".org"
                                     (string-remove-prefix
                                      (file-truename
                                       spacemacs-start-directory)
                                      (file-truename
                                       file)))
                                    ".edn"))
                                  (target-file-dir
                                   (file-name-directory
                                    target-file-name)))
                             (make-directory target-file-dir t)
                             (message "Exporting \"%s\" into \"%s\""
                                      file
                                      target-file-name)
                             (with-temp-buffer
                               (find-file file)
                               (org-export-to-file
                                'spacemacs-edn
                                target-file-name)))))))))
    (let* ((org-publish-project-alist
            `(("spacemacs-edn-static"
               :components ("spacemacs-doc-static"
                            "spacemacs-layers-static"))
              ("spacemacs-doc-static"
               :base-directory ,(concat spacemacs-start-directory "doc/")
               :base-extension "jpg\\|jpeg\\|svg\\|png\\|gif"
               :recursive t
               :publishing-directory ,(concat pub-dir "doc/")
               :publishing-function org-publish-attachment)
              ("spacemacs-layers-static"
               :base-directory ,(concat spacemacs-start-directory "layers/")
               :base-extension "jpg\\|jpeg\\|svg\\|png\\|gif"
               :recursive t
               :publishing-directory ,(concat pub-dir "layers/")
               :publishing-function org-publish-attachment))))
      (org-publish-project "spacemacs-edn-static" t))
    (when noninteractive
      (while (> part-in instances-finished) (sleep-for 1)))))

(provide 'core-documentation-edn)
;;; core-documentation-edn.el ends here
