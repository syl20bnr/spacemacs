;;; ox-groff.el --- Groff Back-End for Org Export Engine

;; Copyright (C) 2011-2021  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Author: Luis R Anaya <papoanaya aroba hot mail punto com>
;; Keywords: outlines, hypermedia, calendar, wp

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
;;
;; This library implements a Groff Memorandum Macro back-end for Org
;; generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'groff "*Test Groff*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the Groff
;; export.  See ox.el for more details on how this exporter works.
;;
;; It introduces two new buffer keywords: "GROFF_CLASS" and
;; "GROFF_CLASS_OPTIONS".

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox)

(defvar orgtbl-exp-regexp)


;;; Define Back-End

(org-export-define-backend 'groff
  '((bold . org-groff-bold)
    (center-block . org-groff-center-block)
    (clock . org-groff-clock)
    (code . org-groff-code)
    (drawer . org-groff-drawer)
    (dynamic-block . org-groff-dynamic-block)
    (entity . org-groff-entity)
    (example-block . org-groff-example-block)
    (export-block . org-groff-export-block)
    (export-snippet . org-groff-export-snippet)
    (fixed-width . org-groff-fixed-width)
    (footnote-definition . org-groff-footnote-definition)
    (footnote-reference . org-groff-footnote-reference)
    (headline . org-groff-headline)
    (horizontal-rule . org-groff-horizontal-rule)
    (inline-src-block . org-groff-inline-src-block)
    (inlinetask . org-groff-inlinetask)
    (italic . org-groff-italic)
    (item . org-groff-item)
    (keyword . org-groff-keyword)
    (line-break . org-groff-line-break)
    (link . org-groff-link)
    (node-property . org-groff-node-property)
    (paragraph . org-groff-paragraph)
    (plain-list . org-groff-plain-list)
    (plain-text . org-groff-plain-text)
    (planning . org-groff-planning)
    (property-drawer . org-groff-property-drawer)
    (quote-block . org-groff-quote-block)
    (radio-target . org-groff-radio-target)
    (section . org-groff-section)
    (special-block . org-groff-special-block)
    (src-block . org-groff-src-block)
    (statistics-cookie . org-groff-statistics-cookie)
    (strike-through . org-groff-strike-through)
    (subscript . org-groff-subscript)
    (superscript . org-groff-superscript)
    (table . org-groff-table)
    (table-cell . org-groff-table-cell)
    (table-row . org-groff-table-row)
    (target . org-groff-target)
    (template . org-groff-template)
    (timestamp . org-groff-timestamp)
    (underline . org-groff-underline)
    (verbatim . org-groff-verbatim)
    (verse-block . org-groff-verse-block))
  :menu-entry
  '(?g "Export to GROFF"
       ((?g "As GROFF file" org-groff-export-to-groff)
	(?p "As PDF file" org-groff-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-groff-export-to-pdf t s v b)
		(org-open-file (org-groff-export-to-pdf nil s v b)))))))
  :options-alist
  '((:groff-class "GROFF_CLASS" nil org-groff-default-class t)
    (:groff-class-options "GROFF_CLASS_OPTIONS" nil nil t)
    (:groff-header-extra "GROFF_HEADER" nil nil newline)))



;;; User Configurable Variables

(defgroup org-export-groff nil
  "Options for exporting Org mode files to Groff."
  :tag "Org Export Groff"
  :group 'org-export)

;;; Preamble

(defcustom org-groff-default-class "internal"
  "The default Groff class."
  :group 'org-export-groff
  :type '(string :tag "Groff class"))

(defcustom org-groff-classes
  '(("file" ".MT 1"
     (:heading 'default :type "memo" :last-section "toc"))
    ("internal" ".MT 0"
     (:heading 'default :type "memo" :last-section "toc"))
    ("programmer" ".MT 2"
     (:heading 'default :type "memo" :last-section "toc"))
    ("engineer" ".MT 3"
     (:heading 'default :type "memo" :last-section "toc"))
    ("external" ".MT 4"
     (:heading 'default :type "memo" :last-section "toc"))
    ("letter" ".MT 5"
     (:heading 'default :type "memo" :last-section "sign"))
    ("custom" ".so file"
     (:heading custom-function :type "custom" :last-section "toc"))
    ("dummy" ""
     (:heading 'default :type "memo"))
    ("ms" "ms"
     (:heading 'default :type "cover" :last-section "toc"))
    ("se_ms" "se_ms"
     (:heading 'default :type "cover" :last-section "toc"))
    ("block" "BL"
     (:heading 'default :type "letter" :last-section "sign"))
    ("semiblock" "SB"
     (:heading 'default :type "letter" :last-section "sign"))
    ("fullblock" "FB"
     (:heading 'default :type "letter" :last-section "sign"))
    ("simplified" "SP"
     (:heading 'default :type "letter" :last-section "sign"))
    ("none" "" (:heading 'default :type "custom")))

  ;; none means, no Cover or Memorandum Type and no calls to AU, AT, ND and TL
  ;; This is to facilitate the creation of custom pages.

  ;; dummy means, no Cover or Memorandum Type but calls to AU, AT, ND and TL
  ;; are made. This is to facilitate Abstract Insertion.

  "This list describes the attributes for the documents being created.
   It allows for the creation of new "
  :group 'org-export-groff
  :type '(repeat
          (list (string :tag "Document Type")
                (string :tag "Header")
                (repeat :tag "Options" :inline t
                        (choice
                         (list :tag "Heading")
                         (function :tag "Hook computing sectioning"))))))

;;; Headline

(defconst org-groff-special-tags
  '("FROM" "TO" "ABSTRACT" "APPENDIX" "BODY" "NS"))

(defcustom org-groff-format-headline-function nil
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags as a list of strings (list of strings or nil).

The function result will be used in the section format string.

As an example, one could set the variable to the following, in
order to reproduce the default set-up:

\(defun org-groff-format-headline (todo todo-type priority text tags)
  \"Default format function for a headline.\"
  \(concat (when todo
            \(format \"\\fB%s\\fP \" todo))
	  \(when priority
            \(format \"[\\#%c] \" priority))
	  text
	  \(when tags
            \(format \" %s \"
              \(mapconcat 'identity tags \":\"))))"
  :group 'org-export-groff
  :type 'function)

;;; Timestamps

(defcustom org-groff-active-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-groff
  :type 'string)

(defcustom org-groff-inactive-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-groff
  :type 'string)

(defcustom org-groff-diary-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-groff
  :type 'string)

;;; Links

(defcustom org-groff-inline-image-rules
  '(("file" . "\\.\\(jpg\\|png\\|pdf\\|ps\\|eps\\|pic\\)\\'")
    ("fuzzy" . "\\.\\(jpg\\|png\\|pdf\\|ps\\|eps\\|pic\\)\\'"))
  "Rules characterizing image files that can be inlined into Groff.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extensions actually allowed
depend on the way the Groff file is processed.  When used with
pdfgroff, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-groff
  :type '(alist :key-type (string :tag "Type")
                :value-type (regexp :tag "Path")))

(defcustom org-groff-link-with-unknown-path-format "\\fI%s\\fP"
  "Format string for links with unknown path type."
  :group 'org-export-groff
  :type 'string)

;;; Tables

(defcustom org-groff-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-groff-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-groff-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-groff
  :type '(choice
          (string :tag "Format string")
          (const :tag "No formatting")))

;;; Text markup

(defcustom org-groff-text-markup-alist
   '((bold . "\\fB%s\\fP")
    (code . "\\fC%s\\fP")
    (italic . "\\fI%s\\fP")
    (strike-through . "\\fC%s\\fP")  ; Strike through and underline
    (underline . "\\fI%s\\fP")       ; need to be revised.
    (verbatim .   "protectedtexttt"))
  "Alist of Groff expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with it.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-groff
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))

;;; Drawers

(defcustom org-groff-format-drawer-function nil
  "Function called to format a drawer in Groff code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-groff-format-drawer-default \(name contents\)
  \"Format a drawer element for Groff export.\"
  contents\)"
  :group 'org-export-groff
  :type 'function)

;;; Inlinetasks

(defcustom org-groff-format-inlinetask-function nil
  "Function called to format an inlinetask in Groff code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-groff-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for Groff export.\"
  \(let ((full-title
	 \(concat
	  \(when todo
            \(format \"\\fB%s\\fP \" todo))
	  \(when priority (format \"[\\#%c] \" priority))
	  title
	  \(when tags
            \(format \":%s:\"
                    \(mapconcat 'identity tags \":\")))))
    \(format (concat \".DS L\\n\"
		    \"%s\\n\\n\"
		    \"%s\"
		    \".DE\")
	    full-title contents))"
  :group 'org-export-groff
  :type 'function)

;; Src blocks

(defcustom org-groff-source-highlight nil
  "Use GNU source highlight to embellish source blocks "
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-groff-source-highlight-langs
  '((emacs-lisp "lisp") (lisp "lisp") (clojure "lisp")
    (scheme "scheme")
    (c "c") (cc "cpp") (csharp "csharp") (d "d")
    (fortran "fortran") (cobol "cobol") (pascal "pascal")
    (ada "ada") (asm "asm")
    (perl "perl") (cperl "perl")
    (python "python") (ruby "ruby") (tcl "tcl") (lua "lua")
    (java "java") (javascript "javascript")
    (tex "latex")
    (shell-script "sh") (awk "awk") (diff "diff") (m4 "m4")
    (ocaml "caml") (caml "caml")
    (sql "sql") (sqlite "sql")
    (html "html") (css "css") (xml "xml")
    (bat "bat") (bison "bison") (clipper "clipper")
    (ldap "ldap") (opa "opa")
    (php "php") (postscript "postscript") (prolog "prolog")
    (properties "properties") (makefile "makefile")
    (tml "tml") (vala "vala") (vbscript "vbscript") (xorg "xorg"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-groff
  :type '(repeat
          (list
           (symbol :tag "Major mode       ")
           (string :tag "Listings language"))))

(defcustom org-groff-source-highlight-options nil
  "Association list of options for the groff listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-groff-source-highlight-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-groff
  :type '(repeat
          (list
           (string :tag "Listings option name ")
           (string :tag "Listings option value"))))

(defvar org-groff-custom-lang-environments nil
  "Alist mapping languages to language-specific Groff environments.

It is used during export of src blocks by the listings and
groff packages.  For example,

  \(setq org-groff-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during groff export it will use pythoncode as the source-highlight
language.")

;;; Plain text

(defcustom org-groff-special-char
  '(("(c)" . "\\\\(co")
    ("(tm)" . "\\\\(tm")
    ("(rg)" . "\\\\(rg"))
  "CONS list in which the value of the car
  is replace on the value of the CDR. "
  :group 'org-export-groff
  :type '(list
          (cons :tag "Character Substitute"
                (string :tag "Original Character Group")
                (string :tag "Replacement Character"))))

;;; Compilation

(defcustom org-groff-pdf-process
  '("pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
    "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
    "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf")

  "Commands to process a Groff file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file."
  :group 'org-export-pdf
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of pdfgroff"
                 ("pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (const :tag "3 runs of pdfgroff"
                 ("pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (function)))

(defcustom org-groff-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as Groff logfiles."
  :group 'org-export-groff
  :type '(repeat (string :tag "Extension")))

(defcustom org-groff-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-groff-organization "Org User"
  "Name of the organization used to populate the .AF command."
  :group 'org-export-groff
  :type 'string)

(defcustom org-groff-raster-to-ps nil
  "Command used to convert raster to EPS. Nil for no conversion. Make sure that
   `org-groff-inline-image-rules' is adjusted accordingly if not conversion is being
   done. In this case, remove the entries for jpg and png in the file and fuzzy lists."
  :group 'org-export-groff
  :type '(choice
         (repeat :tag "Shell Command Sequence" (string :tag "Shell Command"))
         (const :tag "sam2p" "a=%s;b=%s;sam2p ${a} ${b} ;grep -v BeginData ${b} > b_${b};mv b_${b} ${b}" )
         (const :tag "NetPNM"  "a=%s;b=%s;pngtopnm ${a} | pnmtops -noturn > ${b}" )
         (const :tag "None" nil)))

(defvar org-groff-registered-references nil)
(defvar org-groff-special-content nil)



;;; Internal Functions

(defun org-groff--caption/label-string (element info)
  "Return caption and label Groff string for ELEMENT.

INFO is a plist holding contextual information.  If there's no
caption nor label, return the empty string.

For non-floats, see `org-groff--wrap-label'."
  (let ((main (org-export-get-caption element))
	(short (org-export-get-caption element t))
	(label (org-element-property :name element)))
    (cond ((and (not main) (not label)) "")
	  ((not main) (format "\\fI%s\\fP" label))
	  ;; Option caption format with short name.
	  (short (format "%s\n.br\n - %s\n"
			 (org-export-data short info)
			 (org-export-data main info)))
	  ;; Standard caption format.
	  (t (format "\\fR%s\\fP" (org-export-data main info))))))

(defun org-groff--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-groff--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s\n.br\n" label) output))))

(defun org-groff--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-groff-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-groff-text-markup-alist))))
    (cond
     ;; No format string: Return raw text.
     ((not fmt) text)
     ((string= "protectedtexttt" fmt)
      (let ((start 0)
            (trans '(("\\" . "\\")))
            (rtn "")
            char)
        (while (string-match "[\\{}$%&_#~^]" text)
          (setq char (match-string 0 text))
          (if (> (match-beginning 0) 0)
              (setq rtn (concat rtn (substring text 0 (match-beginning 0)))))
          (setq text (substring text (1+ (match-beginning 0))))
          (setq char (or (cdr (assoc char trans)) (concat "\\" char))
                rtn (concat rtn char)))
        (setq text (concat rtn text))
        (format "\\fC%s\\fP" text)))
     ;; Else use format string.
     (t (format fmt text)))))


(defun org-groff--get-tagged-content  (tag info)
  (cdr  (assoc tag org-groff-special-content)))

(defun org-groff--mt-head (title contents attr info)
  (concat

   ;; 1. Insert Organization
   (let ((firm-option (plist-get attr :firm)))
     (cond
      ((stringp firm-option)
       (format ".AF \"%s\" \n" firm-option))
      (t (format ".AF \"%s\" \n" (or org-groff-organization "")))))

   ;; 2. Title
   (let ((title (if (plist-get info :with-title) title ""))
	 (subtitle1 (plist-get attr :subtitle1))
         (subtitle2 (plist-get attr :subtitle2)))

     (cond
      ((string= "" title)
       (format ".TL \"%s\" \"%s\" \n%s\n"
               (or subtitle1 "")
               (or subtitle2 "") " "))

      ((not (or subtitle1 subtitle2))
       (format ".TL\n%s\n"
               (or title "")))
      (t
       (format ".TL \"%s\" \"%s \" \n%s\n"
               (or subtitle1 "")
               (or subtitle2 "") title))))

   ;; 3. Author.
   ;; In Groff, .AU *MUST* be placed after .TL
   ;; If From, populate with data from From else
   ;;
   (let ((author (and (plist-get info :with-author)
                      (let ((auth (plist-get info :author)))
                        (and auth (org-export-data auth info)))))
         (email (and (plist-get info :with-email)
                     (org-export-data (plist-get info :email) info)))
         (from-data  (org-groff--get-tagged-content "FROM" info))

         (to-data  (org-groff--get-tagged-content "TO" info)))

     (cond
      ((and author from-data)
       (let ((au-line
              (mapconcat
               (lambda (from-line)
                 (format " \"%s\" " from-line))
               (split-string
                (setq from-data
                      (replace-regexp-in-string "\\.P\n" "" from-data)) "\n") "")))

         (concat
          (format ".AU \"%s\" " author) au-line "\n")))

      ((and author email (not (string= "" email)))
       (format ".AU \"%s\" \"%s\"\n" author email))

      (author (format ".AU \"%s\"\n" author))

      (t ".AU \"\" \n")))


   ;; 4. Author Title, if present
   (let ((at-item (plist-get attr :author-title)))
     (if (and at-item (stringp at-item))
         (format ".AT \"%s\" \n" at-item)
       ""))

   ;; 5. Date.
   (when (plist-get info :with-date)
     (let ((date (org-export-data (org-export-get-date info) info)))
       (and (org-string-nw-p date) (format ".ND \"%s\"\n" date))))

   ;;
   ;; If Abstract, then Populate Abstract
   ;;

   (let ((abstract-data (org-groff--get-tagged-content "ABSTRACT" info))
         (to-data (org-groff--get-tagged-content "TO" info)))
     (cond
      (abstract-data
       (format ".AS\n%s\n.AE\n" abstract-data))
      (to-data
       (format ".AS\n%s\n.AE\n" to-data))))))

(defun org-groff--letter-head (title contents attr info)
  (let ((author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       (and auth (org-export-data auth info)))))
        (email (and (plist-get info :with-email)
                    (org-export-data (plist-get info :email) info)))
        (from-data  (org-groff--get-tagged-content "FROM" info))
        (at-item (plist-get attr :author-title))
        (to-data  (org-groff--get-tagged-content "TO" info)))


    ;; If FROM then get data from FROM
    (if from-data
        (setq from-data
              (replace-regexp-in-string "\\.P\n" "" from-data))
      (setq from-data ""))

    (if to-data
        (setq to-data
              (replace-regexp-in-string "\\.P\n" "" to-data))
      (setq from-data ""))

    (concat
     (cond
      (from-data
       (format ".WA \"%s\" \"%s\" \n%s\n.WE\n" author (or at-item "") from-data))
      ((and author email (not (string= "" email)))
       (format ".WA \"%s\"\n \"%s\"\n.WE\n" author email))
      (author (format ".WA \"%s\"\n.WE\n" author))
      (t ".WA \"\" \n.WE\n"))

     ;; If TO then get data from TO

     (when to-data
       (format ".IA \n%s\n.IE\n" to-data)))))


;;; Template

(defun org-groff-template (contents info)
  "Return complete document string after Groff conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
         (attr (read
                (format "(%s)"
                        (mapconcat
                         #'identity
                         (list (plist-get info :groff-class-options))
                         " "))))
         (class (plist-get info :groff-class))
         (class-options (plist-get info :groff-class-options))
         (classes (assoc class org-groff-classes))
         (classes-options (car (last classes)))
         (heading-option (plist-get classes-options :heading))
         (type-option (plist-get classes-options :type))
         (last-option (plist-get classes-options :last-section))
         (hyphenate (plist-get attr :hyphenate))
         (justify-right (plist-get attr :justify-right))

         (document-class-string
          (progn
            (org-element-normalize-string
             (let* ((header (nth 1 (assoc class org-groff-classes)))
                    (document-class-item (if (stringp header) header "")))
               document-class-item)))))


    (concat
     (if justify-right
         (case justify-right
           ('yes ".SA 1 \n")
           ('no ".SA 0 \n")
           (t ""))
       "")

     (if hyphenate
         (case hyphenate
           ('yes ".nr Hy 1 \n")
           ('no ".nr Hy 0 \n")
           (t ""))
       "")

     (cond
      ((string= type-option "custom") "")

      ((and (stringp document-class-string)
            (string= type-option "cover"))

       (concat
        (format ".COVER %s\n" document-class-string)
        (org-groff--mt-head title contents attr info)
        ".COVEND\n"))

      ((string= type-option "memo")
       (concat
        (org-groff--mt-head title contents attr info)
        document-class-string))
      ((string= type-option "letter")
       (concat
        (org-groff--letter-head title contents attr info)
        (let ((sa-item (plist-get attr :salutation))
              (cn-item (plist-get attr :confidential))
              (sj-item (plist-get attr :subject))
              (rn-item (plist-get attr :reference))
              (at-item (plist-get attr :attention)))

          (concat

           (if (stringp sa-item)
               (format ".LO SA \"%s\" \n"  sa-item)
             ".LO SA\n")

           (when cn-item
             (if (stringp cn-item)
                 (format ".LO CN \"%s\"\n" cn-item)
               ".LO CN\n"))

           (when (and at-item (stringp at-item))
             (format ".LO AT \"%s\" \n"  at-item))
           (when (and title rn-item)
             (format ".LO RN \"%s\"\n" title))

           (when (and sj-item (stringp sj-item))
             (format ".LO SJ \"%s\" \n"  sj-item))


           ".LT " document-class-string  "\n"))))

      (t ""))

     contents

     (cond
      ((string= last-option "toc")
       ".TC")
      ((string= last-option "sign")
       (let ((fc-item (plist-get attr :closing)))
         (concat (if (stringp fc-item)
                     (format ".FC \"%s\" \n" fc-item)
                   ".FC\n")
                 ".SG\n")))
      (t ""))

     (progn
       (mapconcat
        (lambda (item)
          (when (string= (car item) "NS")
            (replace-regexp-in-string
                    "\\.P\n" "" (cdr item))))
        (reverse org-groff-special-content) "\n")))))



;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-groff-bold (bold contents info)
  "Transcode BOLD from Org to Groff.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-groff--text-markup contents 'bold))

;;; Center Block

(defun org-groff-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Groff.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-groff--wrap-label
   center-block
   (format ".DS C \n%s\n.DE" contents)))

;;; Clock

(defun org-groff-clock (clock contents info)
  "Transcode a CLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (format "\\fB%s\\fP " org-clock-string)
   (format org-groff-inactive-timestamp-format
           (concat (org-timestamp-translate (org-element-property :value clock))
                   (let ((time (org-element-property :duration clock)))
                     (and time (format " (%s)" time)))))))

;;; Code

(defun org-groff-code (code contents info)
  "Transcode a CODE object from Org to Groff.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-groff--text-markup (org-element-property :value code) 'code))

;;; Comments and Comment Blocks are ignored.

;;; Drawer

(defun org-groff-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
         (output (if (functionp org-groff-format-drawer-function)
                     (funcall org-groff-format-drawer-function
                              name contents)
                   ;; If there's no user defined function: simply
                   ;; display contents of the drawer.
                   contents)))
    (org-groff--wrap-label drawer output)))

;;; Dynamic Block

(defun org-groff-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-groff--wrap-label dynamic-block contents))

;;; Entity

(defun org-groff-entity (entity contents info)
  "Transcode an ENTITY object from Org to Groff.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :utf-8 entity))

;;; Example Block

(defun org-groff-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-groff--wrap-label
   example-block
   (format ".DS L\n%s\n.DE"
           (org-export-format-code-default example-block info))))

;;; Export Block

(defun org-groff-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "GROFF")
    (org-remove-indentation (org-element-property :value export-block))))

;;; Export Snippet

(defun org-groff-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'groff)
    (org-element-property :value export-snippet)))

;;; Fixed Width

(defun org-groff-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-groff--wrap-label
   fixed-width
   (format "\\fC\n%s\n\\fP"
           (org-remove-indentation
            (org-element-property :value fixed-width)))))

;;; Footnote Definition
;;
;; Footnote Definitions are ignored.
;;
;; Footnotes are handled automatically in GROFF.  Although manual
;; references can be added, not really required.

(defun org-groff-footnote-reference (footnote-reference contents info)
  ;; Changing from info to footnote-reference
  (let* ((raw (org-export-get-footnote-definition footnote-reference info))
		 (n (org-export-get-footnote-number footnote-reference info))
		 (data (org-trim (org-export-data raw info)))
         (ref-id (plist-get (nth 1 footnote-reference) :label)))
    ;; It is a reference
    (if (string-match "fn:rl" ref-id)
        (if (member ref-id org-groff-registered-references)
            (format "\\*[%s]" ref-id)
          (progn
            (push ref-id org-groff-registered-references)
            (format "\\*(Rf\n.RS \"%s\" \n%s\n.RF\n" ref-id  data)))
      ;; else it is a footnote
      (format "\\u\\s-2%s\\d\\s+2\n.FS %s\n%s\n.FE\n" n n data))))

;;; Headline

(defun org-groff-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Groff.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :groff-class))
         (level (org-export-get-relative-level headline info))
         (numberedp (org-export-numbered-headline-p headline info))
         ;; Section formatting will set two placeholders: one for the
         ;; title and the other for the contents.
         (classes (assoc class org-groff-classes))
         (classes-options (car (last classes)))
         (heading-option (plist-get classes-options :heading))
         (section-fmt
          (progn
            (cond
             ((and (symbolp heading-option)
                   (fboundp heading-option))
              (funcall heading-option level numberedp))
             ((> level 7) nil)
             (t (if numberedp
                    (concat ".H " (number-to-string level) " \"%s\"\n%s")
                  ".HU \"%s\"\n%s")))))
         ;; End of section-fmt
         (text (org-export-data (org-element-property :title headline) info))
         (todo
          (and (plist-get info :with-todo-keywords)
               (let ((todo (org-element-property :todo-keyword headline)))
                 (and todo (org-export-data todo info)))))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info)))
         (priority (and (plist-get info :with-priority)
                        (org-element-property :priority headline)))
         ;; Create the headline text along with a no-tag version.  The
         ;; latter is required to remove tags from table of contents.
         (full-text (if (functionp org-groff-format-headline-function)
                        ;; User-defined formatting function.
                        (funcall org-groff-format-headline-function
                                 todo todo-type priority text tags)
                      ;; Default formatting.
                      (concat
                       (when todo
                         (format "\\fB%s\\fP " todo))
                       (when priority (format " [\\#%c] " priority))
                       text
                       (when tags
                         (format " \\fC%s\\fP " (org-make-tag-string tags))))))
         (full-text-no-tag
          (if (functionp org-groff-format-headline-function)
              ;; User-defined formatting function.
              (funcall org-groff-format-headline-function
                       todo todo-type priority text nil)
            ;; Default formatting.
            (concat
             (when todo (format "\\fB%s\\fP " todo))
             (when priority (format " [\\#%c] " priority))
             text)))
         ;; Associate some \label to the headline for internal links.
         ;; 	 (headline-label
         ;; 	  (format "\\label{sec-%s}\n"
         ;; 		  (mapconcat 'number-to-string
         ;; 			     (org-export-get-headline-number headline info)
         ;; 			     "-")))
         (headline-label "")
         (pre-blanks
          (make-string (org-element-property :pre-blank headline) 10)))

    (cond
     ;; Case 1: Special Tag
     ((member (car  tags)  org-groff-special-tags)
      (cond
       ((string= (car tags) "BODY") contents)

       ((string= (car tags) "NS")
        (progn
          (push (cons (car tags)
                      (format ".NS \"%s\" 1 \n%s"
                              (car (org-element-property :title headline))
                              (or contents " ")))
                org-groff-special-content) nil))

       (t
        (progn
          (push (cons  (car tags) contents) org-groff-special-content)
          nil))))

     ;; Case 2: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)

     ;; Case 3: This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
             (concat
              ;; If the headline is the first sibling, start a list.
              (when (org-export-first-sibling-p headline info)
                (format "%s\n" (if numberedp ".AL 1\n" ".DL \n")))
              ;; Itemize headline
              ".LI\n" full-text "\n" headline-label pre-blanks contents)))
        ;; If headline is not the last sibling simply return
        ;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
        ;; blank line.
        (if (not (org-export-last-sibling-p headline info)) low-level-body
          (replace-regexp-in-string
           "[ \t\n]*\\'"
           (concat "\n.LE")
           low-level-body))))

     ;; Case 4. Standard headline.  Export it as a section.
     (t
      (format section-fmt full-text
              (concat headline-label pre-blanks contents))))))

;;; Horizontal Rule
;; Not supported

;;; Inline Babel Call
;;
;; Inline Babel Calls are ignored.

;;; Inline Src Block

(defun org-groff-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block)))
    (cond
     (org-groff-source-highlight
      (let* ((tmpdir temporary-file-directory)
             (in-file  (make-temp-name
                        (expand-file-name "srchilite" tmpdir)))
             (out-file (make-temp-name
                        (expand-file-name "reshilite" tmpdir)))
             (org-lang (org-element-property :language inline-src-block))
             (lst-lang (cadr (assq (intern org-lang)
                                   org-groff-source-highlight-langs)))

             (cmd (concat (expand-file-name "source-highlight")
                          " -s " lst-lang
                          " -f groff_mm_color "
                          " -i " in-file
                          " -o " out-file)))
        (if lst-lang
            (let ((code-block ""))
              (with-temp-file in-file (insert code))
              (shell-command cmd)
              (setq code-block  (org-file-contents out-file))
              (delete-file in-file)
              (delete-file out-file)
              code-block)
          (format ".DS I\n\\fC\\m[black]%s\\m[]\\fP\n.DE\n"
                  code))))

     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat ".DS I\n" "\\fC" code "\\fP\n.DE\n")))))

;;; Inlinetask

(defun org-groff-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-data (org-element-property :title inlinetask) info))
        (todo (and (plist-get info :with-todo-keywords)
                   (let ((todo (org-element-property :todo-keyword inlinetask)))
                     (and todo (org-export-data todo info)))))
        (todo-type (org-element-property :todo-type inlinetask))
        (tags (and (plist-get info :with-tags)
                   (org-export-get-tags inlinetask info)))
        (priority (and (plist-get info :with-priority)
                       (org-element-property :priority inlinetask))))
    ;; If `org-groff-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-groff-format-inlinetask-function)
        (funcall org-groff-format-inlinetask-function
                 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-groff--wrap-label
       inlinetask
       (let ((full-title
              (concat
               (when todo (format "\\fB%s\\fP " todo))
               (when priority (format " [\\#%c] " priority))
               title
               (when tags (format " \\fC%s\\fP " (org-make-tag-string tags))))))
         (format (concat "\n.DS I\n"
                         "%s\n"
                         ".sp"
                         "%s\n"
                         ".DE")
                 full-title contents))))))

;;; Italic

(defun org-groff-italic (italic contents info)
  "Transcode ITALIC from Org to Groff.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-groff--text-markup contents 'italic))

;;; Item

(defun org-groff-item (item contents info)
  "Transcode an ITEM element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((bullet (org-element-property :bullet item))
	 (type (org-element-property
		:type (org-element-property :parent item)))
         (checkbox (case (org-element-property :checkbox item)
                     (on "\\o'\\(sq\\(mu'")
                     (off "\\(sq")
                     (trans "\\o'\\(sq\\(mi'")))
         (tag (let ((tag (org-element-property :tag item)))
                ;; Check-boxes must belong to the tag.
                (and tag (format "%s"
                                 (concat checkbox
                                         (org-export-data tag info)))))))

	(cond
	 ((or checkbox tag)
	  (concat ".LI ""\"" (or tag (concat "\\ " checkbox)) "\""
              "\n"
              (org-trim (or contents " "))))
     ((eq type 'ordered)
      (concat ".LI"
              "\n"
              (org-trim (or contents " "))))
     (t
      (let* ((bullet (org-trim bullet))
             (marker (cond  ((string= "-" bullet) "\\(em")
                            ((string= "*" bullet) "\\(bu")
                            (t "\\(dg"))))
        (concat ".LI " marker "\n"
                (org-trim (or contents " "))))))))

;;; Keyword

(defun org-groff-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "GROFF") value)
     (t nil))))

;;; Line Break

(defun org-groff-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".br\n")

;;; Link
;; Inline images just place a call to .PSPIC or .PS/.PE
;;  and load the graph.

(defun org-groff-link--inline-image (link info)
  "Return Groff code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-element link))
         (path (let ((raw-path (org-element-property :path link)))
                 (if (not (file-name-absolute-p raw-path)) raw-path
                   (expand-file-name raw-path))))
         (attr (org-export-read-attribute :attr_groff link))
         (placement
          (let ((pos (plist-get attr :position)))
	    (cond ((string= pos 'center) "")
		  ((string= pos 'left) "-L")
		  ((string= pos 'right) "-R")
		  (t ""))))
	 (width  (or (plist-get attr :width) ""))
	 (height (or (plist-get attr :height) ""))
	 (caption (and (not (plist-get attr :disable-caption))
		       (org-groff--caption/label-string parent info))))
    ;; Now clear ATTR from any special keyword and set a default value
    ;; if nothing is left.  Return proper string.
    (concat
     (cond
      ((and org-groff-raster-to-ps
            (or  (string-match ".\.png$" path)
                 (string-match ".\.jpg$" path)))
       (let ((eps-path (concat path ".eps")))
         (shell-command (format org-groff-raster-to-ps path eps-path))
         (format "\n.DS L F\n.PSPIC %s \"%s\" %s %s\n.DE "
                 placement eps-path width height)))
      ((string-match ".\.pic$" path)
       (format "\n.PS\ncopy \"%s\"\n.PE" path))
      (t (format "\n.DS L F\n.PSPIC %s \"%s\" %s %s\n.DE "
                 placement path width height)))
     (and caption (format "\n.FG \"%s\"" caption)))))

(defun org-groff-link (link desc info)
  "Transcode a LINK object from Org to Groff.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."

  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))
         (imagep (org-export-inline-image-p
                  link org-groff-inline-image-rules))
         (path (cond
                ((member type '("http" "https" "ftp" "mailto"))
                 (concat type ":" raw-path))
                ((string= type "file") (org-export-file-uri raw-path))
                (t raw-path))))
    (cond
     ((org-export-custom-protocol-maybe link desc 'groff info))
     ;; Image file.
     (imagep (org-groff-link--inline-image link info))
     ;; import groff files
     ((and (string= type "file")
           (string-match ".\.groff$" raw-path))
      (concat ".so " raw-path "\n"))
     ;; Radio link: transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination) desc
          (format "\\fI [%s] \\fP"
		  (org-export-get-reference destination info)))))

     ;; Links pointing to a headline: find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (case (org-element-type destination)
          ;; Id link points to an external file.
          (plain-text
           (if desc (format "%s \\fBat\\fP \\fIfile://%s\\fP" desc destination)
             (format "\\fI file://%s \\fP" destination)))
          ;; Fuzzy link points nowhere.
          ('nil
           (format org-groff-link-with-unknown-path-format
                   (or desc
                       (org-export-data
                        (org-element-property :raw-link link) info))))
          ;; LINK points to a headline.  If headlines are numbered and
          ;; the link has no description, display headline's number.
          ;; Otherwise, display description or headline's title.
          (headline
           (let ((label ""))
             (if (and (plist-get info :section-numbers) (not desc))
                 (format "\\fI%s\\fP" label)
               (format "\\fI%s\\fP"
                       (or desc
                           (org-export-data
                            (org-element-property :title destination) info))))))
          ;; Fuzzy link points to a target.  Do as above.
          (otherwise
           (let ((ref (org-export-get-reference destination info)))
             (if (not desc) (format "\\fI%s\\fP" ref)
               (format "%s \\fBat\\fP \\fI%s\\fP" desc ref)))))))
     ;; External link with a description part.
     ((and path desc) (format "%s \\fBat\\fP \\fI%s\\fP" path desc))
     ;; External link without a description part.
     (path (format "\\fI%s\\fP" path))
     ;; No path, only description.  Try to do something useful.
     (t (format org-groff-link-with-unknown-path-format desc)))))

;;; Node Property

(defun org-groff-node-property (node-property contents info)
  "Transcode a NODE-PROPERTY element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))

;;; Paragraph

(defun org-groff-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Groff.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (plist-get (nth 1 paragraph) :parent)))
    (when parent
      (let* ((parent-type (car parent))
             (fixed-paragraph "")
             (class (plist-get info :groff-class))
             (class-options (plist-get info :groff-class-options))
             (classes (assoc class org-groff-classes))
             (classes-options (car (last classes)))
             (paragraph-option (plist-get classes-options :paragraph)))
        (cond
         ((and (symbolp paragraph-option)
               (fboundp paragraph-option))
          (funcall paragraph-option parent-type parent contents))
         ((and (eq parent-type 'item)
               (plist-get (nth 1 parent) :bullet))
          (setq fixed-paragraph (concat "" contents)))
         ((eq parent-type 'section)
          (setq fixed-paragraph (concat ".P\n" contents)))
         ((eq parent-type 'footnote-definition)
          (setq fixed-paragraph (concat "" contents)))
         (t (setq fixed-paragraph (concat "" contents))))
        fixed-paragraph))))

;;; Plain List

(defun org-groff-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Groff.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attr (mapconcat #'identity
                          (org-element-property :attr_groff plain-list)
                          " "))
         (groff-type (cond
                      ((eq type 'ordered) ".AL")
                      ((eq type 'unordered) ".BL")
                      ((eq type 'descriptive) ".VL 2.0i"))))
    (org-groff--wrap-label
     plain-list
     (format "%s\n%s\n.LE" groff-type contents))))

;;; Plain Text

(defun org-groff-plain-text (text info)
  "Transcode a TEXT string from Org to Groff.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
(let ((output text))
  ;; Protect various characters.
  (setq output (replace-regexp-in-string
		"\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
		"$\\" output nil t 1))
  ;; Activate smart quotes.  Be sure to provide original TEXT string
  ;; since OUTPUT may have been modified.
  (when (plist-get info :with-smart-quotes)
    (setq output (org-export-activate-smart-quotes output :utf-8 info text)))
  ;; Handle Special Characters
  (if org-groff-special-char
      (dolist (special-char-list org-groff-special-char)
	(setq output
	      (replace-regexp-in-string (car special-char-list)
					(cdr special-char-list) output))))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq output (replace-regexp-in-string
		  "\\(\\\\\\\\\\)?[ \t]*\n" ".br\n" output)))
  ;; Return value.
  output))

;;; Planning

(defun org-groff-planning (planning contents info)
  "Transcode a PLANNING element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (mapconcat
    'identity
    (delq nil
          (list
           (let ((closed (org-element-property :closed planning)))
             (when closed
               (concat
                (format "\\fR %s \\fP" org-closed-string)
                (format org-groff-inactive-timestamp-format
                        (org-timestamp-translate closed)))))
           (let ((deadline (org-element-property :deadline planning)))
             (when deadline
               (concat
                (format "\\fB %s \\fP" org-deadline-string)
                (format org-groff-active-timestamp-format
                        (org-timestamp-translate deadline)))))
           (let ((scheduled (org-element-property :scheduled planning)))
             (when scheduled
               (concat
                (format "\\fR %s \\fP" org-scheduled-string)
                (format org-groff-active-timestamp-format
                        (org-timestamp-translate scheduled)))))))
    "")
   ""))

;;;; Property Drawer

(defun org-groff-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to Groff.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (format "\\fC\n%s\\fP" contents)))

;;; Quote Block

(defun org-groff-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-groff--wrap-label
   quote-block
   (format ".DS I\n.I\n%s\n.R\n.DE" contents)))

;;; Radio Target

(defun org-groff-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Groff.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "%s - %s" (org-export-get-reference radio-target info) text))

;;; Section

(defun org-groff-section (section contents info)
  "Transcode a SECTION element from Org to Groff.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;; Special Block

(defun org-groff-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type special-block)))
    (org-groff--wrap-label
     special-block
     (format "%s\n" contents))))

;;; Src Block

(defun org-groff-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
         (label (org-element-property :name src-block))
         (code (org-element-property :value src-block))
         (custom-env (and lang
                          (cadr (assq (intern lang)
                                      org-groff-custom-lang-environments))))
         (num-start (org-export-get-loc src-block info))
         (retain-labels (org-element-property :retain-labels src-block))
         (caption (and (not (org-export-read-attribute
			     :attr_groff src-block :disable-caption))
		       (org-groff--caption/label-string src-block info))))

    (cond
     ;; Case 1.  No source fontification.
     ((not org-groff-source-highlight)
      (concat
       (format ".DS I\n\\fC%s\\fP\n.DE\n"
	       (org-export-format-code-default src-block info))
       (and caption (format ".EX \"%s\" " caption))))

     ;; Case 2.  Source fontification.
     (org-groff-source-highlight
      (let* ((tmpdir temporary-file-directory)
	     (in-file  (make-temp-name
			(expand-file-name "srchilite" tmpdir)))
	     (out-file (make-temp-name
			(expand-file-name "reshilite" tmpdir)))

	     (org-lang (org-element-property :language src-block))
	     (lst-lang (cadr (assq (intern org-lang)
				   org-groff-source-highlight-langs)))

	     (cmd (concat "source-highlight"
			  " -s " lst-lang
			  " -f groff_mm_color "
			  " -i " in-file
			  " -o " out-file)))

	(concat
	 (if lst-lang
	     (let ((code-block ""))
	       (with-temp-file in-file (insert code))
	       (shell-command cmd)
	       (setq code-block  (org-file-contents out-file))
	       (delete-file in-file)
	       (delete-file out-file)
	       (format "%s\n"  code-block))
	   (format ".DS I\n\\fC\\m[black]%s\\m[]\\fP\n.DE\n"
		   code))
	 (and caption (format ".EX \"%s\" " caption))))))))


;;; Statistics Cookie

(defun org-groff-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;; Strike-Through

(defun org-groff-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Groff.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-groff--text-markup contents 'strike-through))

;;; Subscript

(defun org-groff-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Groff.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\d\\s-2%s\\s+2\\u" contents))

;;; Superscript "^_%s$

(defun org-groff-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Groff.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\u\\s-2%s\\s+2\\d" contents))


;;; Table
;;
;; `org-groff-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to  `org-groff-table--org-table' function,
;; depending of the type of the table.
;;
;; `org-groff-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-groff-table (table contents info)
  "Transcode a TABLE element from Org to Groff.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-groff-tables-verbatim
        (let ((attr (read (format "(%s)"
                 (mapconcat
                  #'identity
                                   (org-element-property :attr_groff table) " ")))))
          (and attr (plist-get attr :verbatim))))

    (format ".DS L\n\\fC%s\\fP\n.DE"
            ;; Re-create table, without affiliated keywords.
            (org-trim
             (org-element-interpret-data
              `(table nil ,@(org-element-contents table))))))

   ;; Case 2: Standard table.
   (t (org-groff-table--org-table table contents info))))

(defun org-groff-table--align-string (divider table info)
  "Return an appropriate Groff alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
  (let (alignment)
    ;; Extract column groups and alignment from first (non-rule) row.
    (org-element-map
	(org-element-map table 'table-row
	  (lambda (row)
	    (and (eq (org-element-property :type row) 'standard) row))
	  info 'first-match)
	'table-cell
      (lambda (cell)
	(let* ((borders (org-export-table-cell-borders cell info))
	       (raw-width (org-export-table-cell-width cell info))
	       (width-cm (when raw-width (/ raw-width 5)))
	       (width (if raw-width (format "w(%dc)"
					    (if (< width-cm 1) 1 width-cm)) "")))
	  ;; Check left border for the first cell only.
	  ;; Alignment is nil on assignment

	  (when (and (memq 'left borders) (not alignment))
	    (push "|" alignment))
	  (push
	   (case (org-export-table-cell-alignment cell info)
	     (left (concat "l" width divider))
	     (right (concat "r" width divider))
	     (center (concat "c" width divider)))
	   alignment)
	  (when (memq 'right borders) (push "|" alignment))))
      info)
    (apply 'concat (reverse alignment))))

(defun org-groff-table--org-table (table contents info)
  "Return appropriate Groff code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((attr (org-export-read-attribute :attr_groff table))
	 (label (org-element-property :name table))
         (caption (and (not (plist-get attr :disable-caption))
		       (org-groff--caption/label-string table info)))
         (divider (if (plist-get attr :divider) "|" " "))

         ;; Determine alignment string.
         (alignment (org-groff-table--align-string divider table info))

         ;; Extract others display options.

         (lines (org-split-string contents "\n"))

         (attr-list
	  (delq nil
		(list (and (plist-get attr :expand) "expand")
		      (let ((placement (plist-get attr :placement)))
			(cond ((string= placement 'center) "center")
			      ((string= placement 'left) nil)
			      (t (if org-groff-tables-centered "center" ""))))
		      (or (plist-get attr :boxtype) "box"))))

         (title-line  (plist-get attr :title-line))
         (long-cells (plist-get attr :long-cells))

         (table-format
          (concat
           (or (car attr-list) "")
           (or
            (let (output-list)
	      (when (cdr attr-list)
		(dolist (attr-item (cdr attr-list))
                  (setq output-list (concat output-list
					    (format ",%s" attr-item)))))
              output-list) "")))
         (first-line
          (when lines (org-split-string (car lines) "\t"))))
    ;; Prepare the final format string for the table.


    (cond
     ;; Others.
     (lines
      (concat ".TS\n " table-format ";\n"
	      (format "%s.\n"
		      (let ((final-line ""))
			(when title-line
			  (dotimes (i (length first-line))
			    (setq final-line (concat final-line "cb" divider))))

			(setq final-line (concat final-line "\n"))

			(if alignment
			    (setq final-line (concat final-line alignment))
			  (dotimes (i (length first-line))
			    (setq final-line (concat final-line "c" divider))))
			final-line))

	      (format "%s.TE\n"
		      (let ((final-line "")
			    (long-line "")
			    (lines (org-split-string contents "\n")))

			(dolist (line-item lines)
			  (setq long-line "")

			  (if long-cells
			      (progn
				(if (string= line-item "_")
				    (setq long-line (format "%s\n" line-item))
				  ;; else string =
				  (let ((cell-item-list (org-split-string line-item "\t")))
				    (dolist (cell-item cell-item-list)

				      (cond  ((eq cell-item (car (last cell-item-list)))
					      (setq long-line (concat long-line
								      (format "T{\n%s\nT}\t\n"  cell-item))))
					     (t
					      (setq long-line (concat long-line
								      (format "T{\n%s\nT}\t"  cell-item))))))
				    long-line))
				;; else long cells
				(setq final-line (concat final-line long-line)))

			    (setq final-line (concat final-line line-item "\n"))))
			final-line))

	      (if caption (format ".TB \"%s\"" caption) ""))))))

;;; Table Cell

(defun org-groff-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Groff
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (progn
    (concat (if (and contents
                     org-groff-table-scientific-notation
                     (string-match orgtbl-exp-regexp contents))
                ;; Use appropriate format string for scientific
                ;; notation.
                (format org-groff-table-scientific-notation
                        (match-string 1 contents)
                        (match-string 2 contents))
              contents)
            (when (org-export-get-next-element table-cell info) "\t"))))


;;; Table Row

(defun org-groff-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Groff
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((attr (mapconcat 'identity
                            (org-element-property
                             :attr_groff (org-export-get-parent table-row))
                            " "))
           ;; TABLE-ROW's borders are extracted from its first cell.
           (borders
            (org-export-table-cell-borders
             (car (org-element-contents table-row)) info)))
      (concat
       ;; Mark horizontal lines
       (cond  ((and (memq 'top borders) (memq 'above borders)) "_\n"))
       contents
       (cond
        ;; When BOOKTABS are activated enforce bottom rule even when
        ;; no hline was specifically marked.
        ((and (memq 'bottom borders) (memq 'below borders)) "\n_")
        ((memq 'below borders) "\n_"))))))

;;; Target

(defun org-groff-target (target contents info)
  "Transcode a TARGET object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\fI%s\\fP" (org-export-get-reference target info)))

;;; Timestamp

(defun org-groff-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-groff-plain-text
		(org-timestamp-translate timestamp) info)))
    (case (org-element-property :type timestamp)
      ((active active-range)
       (format org-groff-active-timestamp-format value))
      ((inactive inactive-range)
       (format org-groff-inactive-timestamp-format value))
      (t (format org-groff-diary-timestamp-format value)))))

;;; Underline

(defun org-groff-underline (underline contents info)
  "Transcode UNDERLINE from Org to Groff.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-groff--text-markup contents 'underline))

;;; Verbatim

(defun org-groff-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Groff.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-groff--text-markup (org-element-property :value verbatim) 'verbatim))

;;; Verse Block

(defun org-groff-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Groff.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".DS C\n.ft HI\n%s\n.ft\n.DE" contents))


;;; Interactive functions

(defun org-groff-export-to-groff
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Groff file.

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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".groff" subtreep))
	(org-groff-registered-references nil)
	(org-groff-special-content nil))
    (org-export-to-file 'groff outfile
      async subtreep visible-only body-only ext-plist)))

(defun org-groff-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to Groff then process through to PDF.

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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".groff" subtreep)))
    (org-export-to-file 'groff outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-groff-compile file)))))

;; Port to Emacs 26 and earlier.
(defun org-groff--time-sec (time)
  (if (fboundp 'time-convert)
      (time-convert time 'integer)
    (cl-subseq (or time (current-time)) 0 2)))

(defun org-groff-compile (file)
  "Compile a Groff file.

FILE is the name of the file being compiled.  Processing is done
through the command specified in `org-groff-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory file)))
	 (full-name (file-truename file))
	 (out-dir (file-name-directory full-name))
	 (time (org-groff--time-sec nil))
	 ;; Properly set working directory for compilation.
	 (default-directory (if (file-name-absolute-p file)
				(file-name-directory full-name)
			      default-directory))
         errors)
    (message (format "Processing Groff file %s ..." file))
    (save-window-excursion
      (cond
       ;; A function is provided: Apply it.
       ((functionp org-groff-pdf-process)
	(funcall org-groff-pdf-process (shell-quote-argument file)))
       ;; A list is provided: Replace %b, %f and %o with appropriate
       ;; values in each command before applying it.  Output is
       ;; redirected to "*Org PDF Groff Output*" buffer.
       ((consp org-groff-pdf-process)
	(let ((outbuf (get-buffer-create "*Org PDF Groff Output*")))
	  (mapc
	   (lambda (command)
	     (shell-command
	      (replace-regexp-in-string
	       "%b" (shell-quote-argument base-name)
	       (replace-regexp-in-string
		"%f" (shell-quote-argument full-name)
		(replace-regexp-in-string
		 "%o" (shell-quote-argument out-dir) command t t)
		t t) t t)
	      outbuf))
	   org-groff-pdf-process)
	  ;; Collect standard errors from output buffer.
	  (setq errors (org-groff-collect-errors outbuf))))
       (t (error "No valid command to process to PDF")))
      (let ((pdffile (concat out-dir base-name ".pdf")))
	;; Check for process failure.  Provide collected errors if
	;; possible.
	(if (or (not (file-exists-p pdffile))
		;; Only compare times up to whole seconds as some
		;; filesystems (e.g. HFS+) do not retain any finer
		;; granularity.
		(time-less-p (org-groff--time-sec
			      (nth 5 (file-attributes pdffile)))
			     time))
	    (error (concat (format "PDF file %s wasn't produced" pdffile)
			   (when errors (concat ": " errors))))
	  ;; Else remove log files, when specified, and signal end of
	  ;; process to user, along with any error encountered.
	  (when org-groff-remove-logfiles
	    (dolist (ext org-groff-logfiles-extensions)
	      (let ((file (concat out-dir base-name "." ext)))
		(when (file-exists-p file) (delete-file file)))))
	  (message (concat "Process completed"
			   (if (not errors) "."
			     (concat " with errors: " errors)))))
	;; Return output file name.
	pdffile))))

(defun org-groff-collect-errors (buffer)
  "Collect some kind of errors from \"groff\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
      nil)))


(provide 'ox-groff)
;;; ox-groff.el ends here
