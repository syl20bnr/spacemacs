;;; _worker.el ---  Spacemacs docs export worker -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; File structure was borrowed from ox-html.el by (Carsten Dominik
;; <carsten at orgmode dot org> and Jambunathan K
;; <kjambunathan at gmail dot com>).
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and load-file-name
           noninteractive)
  (setq gc-cons-threshold 10000000000))

(eval-when-compile
  (require 'cl)
  (require 'url-util)
  (require 'subr-x))

(require 'ox)

(load-file
 (concat
  (file-name-directory
   (or load-file-name
       buffer-file-name))
  "../lib/toc-org.elc"))

(defconst spacemacs--root-dir
  (file-truename
   (concat
    (file-name-directory
     (or load-file-name (buffer-file-name)))
    "../../../"))
  "Root directory of Spacemacs")

(declare-function toc-org-hrefify-gh "../lib/toc-org.el" (str &optional hash))

(defconst spacemacs-repository "spacemacs"
  "Name of the Spacemacs remote repository.")
(defconst spacemacs-repository-owner "syl20bnr"
  "Name of the Spacemacs remote repository owner.")

(defconst spacemacs-readme-template-url
  (concat "https://github.com/syl20bnr/spacemacs/"
          "blob/develop/core/templates/README.org.template")
  "URL of README.org template")

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


;;; Helper Functions

(defsubst spacemacs//org-edn-format-payload (format-string args)
  "Format payload for JSON."
  (replace-regexp-in-string
   "\n"
   "\r"
   (if args (apply 'format format-string args) format-string)))

(defsubst spacemacs/org-edn-export-file (src-file file-path)
  "Emit request for copying file at FILE-PATH. SRC-FILE will
be sent as the source of request (useful for debugging)"
  (message "{\"type\":\"export\",\"text\":%S,\"source\":%S}"
           (spacemacs//org-edn-format-payload
            file-path)
           (spacemacs//org-edn-format-payload
            src-file)))

(defsubst spacemacs/org-edn-message (format-string &rest args)
  "Emit specified message."
  (message "{\"type\":\"message\",\"text\":%S}"
           (spacemacs//org-edn-format-payload
            format-string
            args)))

(defsubst spacemacs/org-edn-warn (format-string &rest args)
  "Emit specified warning."
  (message "{\"type\":\"warning\",\"text\":%S}"
           (spacemacs//org-edn-format-payload
            format-string
            args)))

(defsubst spacemacs/org-edn-error (format-string &rest args)
  "Emit specified error and exit with code 1."
  (message "{\"type\":\"error\",\"text\":%S}"
           (spacemacs//org-edn-format-payload
            format-string
            args))
  (kill-emacs 1))

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
  "Make id for org HEADLINE by chaining headlines from parent to)
child headline.
NOTE: Each headline is converted with `toc-org-hrefify-gh' but
without unification and \"#\" prefix."
  (let* ((res nil)
         (cur-node headline)
         (parent-node (org-export-get-parent cur-node)))
    (loop
     t
     (when (eq 'headline (car-safe cur-node))
       (push (string-remove-prefix
              "#"
              (toc-org-hrefify-gh
               (org-element-property
                :raw-value
                cur-node)))
             res))
     (if (not parent-node)
         (return res)
       (setq cur-node parent-node
             parent-node (org-export-get-parent cur-node))))
    (mapconcat 'identity res "/")))

(defun ask-user-about-lock (_ __)
  "Ignore locks on files"
  t)


;;; Transcode Functions

;;;; Bold

(defun spacemacs//org-edn-bold (_bold contents _info)
  "Transcode BOLD From Org to Spacemacs EDN.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-bold{:contents [%s]}" contents))

;;;; Center Block

(defun spacemacs//org-edn-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-center-block{:contents [%s]}" contents))

;;;; Clock

(defun spacemacs//org-edn-clock (_clock _contents _info)
  "Transcode a CLOCK element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-clock")
  "")

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

;;;; Drawer

(defun spacemacs//org-edn-drawer (_drawer _contents _info)
  "Transcode a DRAWER element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-drawer")
  "")

;;;; Dynamic Block

(defun spacemacs//org-edn-dynamic-block (_dynamic-block _contents _info)
  "Transcode a DYNAMIC-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-dynamic-block")
  "")

;;;; Entity

(defun spacemacs//org-edn-entity (_entity _contents _info)
  "Transcode an ENTITY object From Org to Spacemacs EDN.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-entity")
  "")

;;;; Example Block

(defun spacemacs//org-edn-example-block (example-block _contents _info)
  "Transcode a EXAMPLE-BLOCK element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "#Spacemacs/Org-example-block{:value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :value example-block))))

;;;; Export Block

(defun spacemacs//org-edn-export-block (_export-block _contents _info)
  "Transcode a EXPORT-BLOCK element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-export-block")
  "")

;;;; Export Snippet

(defun spacemacs//org-edn-export-snippet (_export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-export-snippet")
  "")

;;;; Fixed Width

(defun spacemacs//org-edn-fixed-width (_fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-fixed-width")
  "")

;;;; Footnote Reference

(defun spacemacs//org-edn-footnote-reference
    (_footnote-reference _contents _info)
  "Transcode a FOOTNOTE-REFERENCE element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-footnote-reference")
  "")

;;;; Headline

(defun spacemacs//org-edn-headline (headline contents info)
  "Transcode a HEADLINE element From Org to Spacemacs EDN.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((raw-value (org-element-property :raw-value headline))
         (headline-ht (if (plist-member info :headline-hash)
                          (plist-get info :headline-hash)
                        (let ((hh (make-hash-table :test 'equal)))
                          (plist-put info :headline-hash hh)
                          hh)))
         (gh-id (toc-org-hrefify-gh raw-value headline-ht))
         (level (org-element-property :level headline))
         (nesting-ids (plist-get info :nesting-ids))
         (nesting-id
          (spacemacs/org-edn-headline-make-nesting-id headline))
         (file (plist-get info :input-file)))
    (when (and (= level 1)
               (string= raw-value "Description"))
      (if (plist-member info :file-has-description?)
          (spacemacs/org-edn-error
           (concat "File \"%s\" has multiply top level "
                   "\"Description\" headlines")
           file)
        (plist-put info :file-has-description? 'true)))
    (if (member nesting-id nesting-ids)
        (spacemacs/org-edn-error
         (concat "Multiply identical nesting headlines \"%s\" in %S file. "
                 "Usually it happens when headlines have child headlines "
                 "with similar names")
         nesting-id
         file)
      (plist-put info :nesting-ids (push nesting-id nesting-ids)))

    (puthash gh-id raw-value headline-ht)
    (format
     (concat "#Spacemacs/Org-headline{"
             ":value \"%s\" "
             ":gh-id \"%s\" "
             ":nesting-id \"%s\" "
             ":level %s "
             ":contents [%s]}")
     (spacemacs/org-edn-escape-string raw-value)
     (spacemacs/org-edn-escape-string (string-remove-prefix "#" gh-id))
     (spacemacs/org-edn-escape-string nesting-id)
     level
     contents)))

;;;; Horizontal Rule

(defun spacemacs//org-edn-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE  object From Org to Spacemacs EDN.)))))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-horizontal-rule")
  "")

;;;; Inline Src Block

(defun spacemacs//org-edn-inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-inline-src-block")
  "")

;;;; Inlinetask

(defun spacemacs//org-edn-inlinetask (_inlinetask _contents _info)
  "Transcode an INLINETASK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-inlinetask")
  "")

;;;; Inner Template

(defun spacemacs//org-edn-inner-template (contents _info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (format "#Spacemacs/Org-inner-template{:contents [%s]}" contents))

;;;; Italic

(defun spacemacs//org-edn-italic (_italic contents _info)
  "Transcode ITALIC From Org to Spacemacs EDN.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-italic{:contents [%s]}" contents))

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
      (spacemacs/org-edn-error
       (concat "File \"%s\" contains plain list item of type \"%s\" but "
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

;;;; Keyword

(defun spacemacs//org-edn-keyword (keyword _contents _info)
  "Transcode a KEYWORD element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "#Spacemacs/Org-keyword{:key \"%s\" :value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :key keyword))
          (spacemacs/org-edn-escape-string
           (org-element-property :value keyword))))

;;;; Latex Environment

(defun spacemacs//org-edn-latex-environment (_latex-environment _contents _info)
  "Transcode a LATEX-ENVIRONMENT element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-latex-environment")
  "")

;;;; Latex Fragment

(defun spacemacs//org-edn-latex-fragment (_latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-latex-fragment")
  "")

;;;; Line Break

(defun spacemacs//org-edn-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "#Spacemacs/Org-line-break{}")

;;;; Link

(defconst spacemacs--org-edn-git-url-root-regexp
  (format (concat "\\/\\/github\\.com\\/%s\\/%s\\/blob"
                  "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")
          spacemacs-repository-owner
          spacemacs-repository))

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
    (when local-org-link?
      (plist-put
       info
       :spacemacs-edn-warnings
       (concat
        (plist-get info :spacemacs-edn-warnings)
        (format
         (concat "Link \"%s\" "
                 "in \"%s\" "
                 "should target the org file at "
                 "GitHub "
                 "(GitHub style anchors are supported)\n"
                 "See footnote of %S\n")
         raw-link
         file
         spacemacs-readme-template-url))))
    (cond
     ((string-match spacemacs--org-edn-git-url-root-regexp
                    raw-link)
      (let ((path (concat
                   spacemacs--root-dir
                   (url-unhex-string (match-string 1 raw-link))))
            (target-id (string-remove-prefix
                        "#"
                        (match-string 2 raw-link))))
        (unless (file-readable-p path)
          (spacemacs/org-edn-error
           (concat
            "File %S has a GitHub link to the file %S but "
            "it isn't readable locally.")
           file
           (file-truename path)))
        (format (concat "#Spacemacs/Org-org-file-path{"
                        ":value \"%s\" "
                        ":raw-link \"%s\" "
                        ":target-headline-gh-id \"%s\" "
                        ":description [%s]}")
                (spacemacs/org-edn-escape-string path)
                (spacemacs/org-edn-escape-string raw-link)
                (spacemacs/org-edn-escape-string (url-unhex-string
                                                  target-id))
                desc)))
     (local-link?
      (let ((path (url-unhex-string path)))
        (cond
         ((not (file-readable-p path))
          (spacemacs/org-edn-error
           "File %S has a link to file %S but it isn't readable"
           file
           (file-truename path)))
         ((not (string-prefix-p spacemacs--root-dir
                                (file-truename path)))
          (spacemacs/org-edn-error
           "File %S has a link to file %S but it's outside repository"
           file
           (file-truename path))))
        (when (not local-org-link?)
          (spacemacs/org-edn-export-file file (file-truename path)))
        (format "#Spacemacs/Org-file-path{:value \"%s\" :description [%s]}"
                (spacemacs/org-edn-escape-string path)
                desc)))
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
     (t (spacemacs/org-edn-error
         (concat
          "Link \"%s\" in file \"%s\" "
          "has type \"%s\" "
          "but the type isn't implemented in spacemacs//org-edn-link")
         raw-link
         file
         type)))))

;;;; Node Property

(defun spacemacs//org-edn-node-property (_node-property _contents _info)
  "Transcode a NODE-PROPERTY element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-node-property")
  "")

;;;; Paragraph

(defun spacemacs//org-edn-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element From Org to Spacemacs EDN.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (format "#Spacemacs/Org-paragraph{:contents [%s]}" contents))

;;;; Plain List

(defun spacemacs//org-edn-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element From Org to Spacemacs EDN.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (parent-type
          (symbol-name
           (car
             (org-export-get-parent
              plain-list))))
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
      (spacemacs/org-edn-error
       (concat "File \"%s\" contains plain list of type \"%s\" but "
               "it isn't implemented in spacemacs//org-edn-node-property")
       (plist-get info :input-file)
       type))
    (if (and (not
              ;; FIXME: We probably should use a better way to
              ;; tell apart nested features list and multiply
              ;; features list.
              (string= parent-type
                       "item"))
             (= (or (org-element-property
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
            (spacemacs/org-edn-error
             (concat "File \"%s\" has multiply "
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

;;;; Plain Text

(defun spacemacs//org-edn-plain-text (text _info)
  "Transcode a TEXT string From Org to Spacemacs EDN.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-plain-text{:text \"%s\"}"
          (spacemacs/org-edn-escape-string text)))

;;;; Planning

(defun spacemacs//org-edn-planning (_planning _contents _info)
  "Transcode a PLANNING element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-planning")
  "")

;;;; Property Drawer

(defun spacemacs//org-edn-property-drawer (_property-drawer _contents _info)
  "Transcode a PROPERTY-DRAWER element From Org to Spacemacs EDN.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-property-drawer")
  "")

;;;; Quote Block

(defun spacemacs//org-edn-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-quote-block{:contents [%s]}" contents))

;;;; Radio Target

(defun spacemacs//org-edn-radio-target (_radio-target _text _info)
  "Transcode a RADIO-TARGET object From Org to Spacemacs EDN.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-radio-target")
  "")

;;;; Section

(defun spacemacs//org-edn-section (_section contents _info)
  "Transcode a SECTION element From Org to Spacemacs EDN.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-section{:contents [%s]}" contents))

;;;; Special Block

(defun spacemacs//org-edn-special-block (_special-block _contents _info)
  "Transcode a SPECIAL-BLOCK element From Org to Spacemacs EDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-special-block")
  "")

;;;; Src Block

(defun spacemacs//org-edn-src-block (src-block _contents _info)
  "Transcode a SRC-BLOCK element From Org to Spacemacs EDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (format "#Spacemacs/Org-src-block{:language \"%s\" :value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :language src-block))
          (spacemacs/org-edn-escape-string
           (org-element-property :value src-block))))

;;;; Statistics Cookie

(defun spacemacs//org-edn-statistics-cookie (_statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-statistics-cookie")
  "")

;;;; Strike-Through

(defun spacemacs//org-edn-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH From Org to Spacemacs EDN.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-strike-through{:contents [%s]}" contents))

;;;; Subscript

(defun spacemacs//org-edn-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object From Org to Spacemacs EDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-subscript{:contents [%s]}" contents))

;;;; Superscript

(defun spacemacs//org-edn-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object From Org to Spacemacs EDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-superscript{:contents [%s]}" contents))

;;;; Table

(defun spacemacs//org-edn-table (table contents _info)
  "Transcode a TABLE element From Org to Spacemacs EDN.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((type (org-element-property :type table)))
    (unless (eq type 'org)
      (spacemacs/org-edn-error
       "Table type \"%s\" isn't implemented in spacemacs//org-edn-table"
       type))
    (format "#Spacemacs/Org-table{:type %s :contents [%s]}"
            (org-element-property :type table)
            contents)))

;;;; Table Cell

(defun spacemacs//org-edn-table-cell (_table-cell contents _info)
  "Transcode a TABLE-CELL element From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "#Spacemacs/Org-table-cell{:contents [%s]}" contents))

;;;; Table Row

(defun spacemacs//org-edn-table-row (_table-row contents _info)
  "Transcode a TABLE-ROW element From Org to Spacemacs EDN.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  (format "#Spacemacs/Org-table-row{:contents [%s]}" contents))

;;;; Target

(defun spacemacs//org-edn-target (_target _contents _info)
  "Transcode a TARGET object From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-target")
  "")

;;;; Template

(defun spacemacs//org-edn-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((has-description?
         (plist-member info :file-has-description?))
        (has-feature-list?
         (plist-member info :file-has-feature-list?)))
    (when (plist-get info :input-file)
      (let ((file (file-truename
                   (plist-get info :input-file))))
        (when (and (string-prefix-p (file-truename
                                     (concat
                                      spacemacs--root-dir
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
              (format
               (concat
                "File \"%s\" "
                "doesn't have top level "
                "\"Description\" headline\n"
                "See %S\n")
               file
               spacemacs-readme-template-url))))
          (unless has-feature-list?
            (plist-put
             info
             :spacemacs-edn-warnings
             (concat
              (plist-get info :spacemacs-edn-warnings)
              (format
               (concat "File \"%s\" "
                       "doesn't have \"Features:\"(With a colon) list in the "
                       "top level \"Description\" headline\n"
                       "See %S\n")
               file
               spacemacs-readme-template-url)))))))
    (format (concat "#Spacemacs/Org-template{"
                    ":export-data #inst \"%s\" "
                    ":file-has-description? %s "
                    ":file-has-feature-list? %s "
                    ":headlines [%s]"
                    ":contents [%s]}")
            (format-time-string "%Y-%m-%dT%H:%M:%S.52Z" nil t)
            (if (plist-member info :file-has-description?)
                'true
              'false)
            (if (plist-member info :file-has-feature-list?)
                'true
              'false)
            (plist-get info :nesting-ids)
            contents)))

;;;; Timestamp

(defun spacemacs//org-edn-timestamp (_timestamp _contents _info)
  "Transcode a TIMESTAMP object From Org to Spacemacs EDN.)))))))))
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-edn-error "\"%s\" not implemented"
                           "spacemacs//org-edn-timestamp")
  "")

;;;; Underline

(defun spacemacs//org-edn-underline (_underline contents _info)
  "Transcode UNDERLINE From Org to Spacemacs EDN.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "#Spacemacs/Org-underline{:contents [%s]}" contents))

;;;; Verbatim

(defun spacemacs//org-edn-verbatim (verbatim _contents _info)
  "Transcode VERBATIM From Org to Spacemacs EDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "#Spacemacs/Org-verbatim{:value \"%s\"}"
          (spacemacs/org-edn-escape-string
           (org-element-property :value verbatim))))

;;;; Verse Block

(defun spacemacs//org-edn-verse-block (_verse-block contents _info)
  "Transcode a VERSE-BLOCK element From Org to Spacemacs EDN.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (format "#Spacemacs/Org-verse-block{:contents [%s]}" contents))


;;; Filter Functions

(defsubst spacemacs//org-edn-final-function-tidy (contents)
  "Filter to compact output by removing newline symbols.
FIXME: Figure out where they come from :"
  (replace-regexp-in-string "\n" "" contents))

(defsubst spacemacs//org-edn-final-function-lint (info)
  "Warn about potential errors."
  (let ((warnings (plist-get info :spacemacs-edn-warnings)))
    (when (stringp warnings)
      (spacemacs/org-edn-warn
       "%s"
       (string-remove-suffix "\n" warnings)))))

(defun spacemacs//org-edn-final-function (contents _backend info)
  "Call final functions for `space-edn' backend"
  (spacemacs//org-edn-final-function-lint info)
  (spacemacs//org-edn-final-function-tidy contents))


;;; End-user functions

(defun spacemacs/export-docs-to-edn (exp-dir file-list)
  "Export org files in FILE-LIST into EXP-DIR."
  (unwind-protect
      (dolist (file file-list)
        (let* ((target-file-name (concat
                                  exp-dir
                                  (string-remove-suffix
                                   ".org"
                                   (string-remove-prefix
                                    (file-truename
                                     spacemacs--root-dir)
                                    (file-truename file)))
                                  ".edn"))
               (target-file-dir
                (file-name-directory target-file-name)))
          (make-directory target-file-dir t)
          (spacemacs/org-edn-message
           "Exporting \"%s\" into \"%s\""
           file
           target-file-name)
          (with-temp-buffer
            (find-file file)
            (org-export-to-file
                'spacemacs-edn
                target-file-name))
          (if (and (file-readable-p target-file-name)
                   (> (nth 7 (file-attributes target-file-name)) 0))
              (spacemacs/org-edn-message
               "Successfully exported \"%s\""
               file)
            (spacemacs/org-edn-error
             "Export finished but \"%s\" doesn't exist or empty"
             target-file-name))))))
