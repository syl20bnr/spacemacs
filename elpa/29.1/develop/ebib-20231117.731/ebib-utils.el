;;; ebib-utils.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2023 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

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

;; This file is part of Ebib, a BibTeX database manager for Emacs.  It contains
;; general macros, utilities and variables.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'bibtex)
(require 'format-spec)
(require 'message) ; for `message-unquote-tokens'.
(require 'parsebib)
(require 'ebib-db)

;; Make a bunch of variables obsolete.
(make-obsolete-variable 'ebib-entry-types "The variabale `ebib-entry-types' is obsolete; see the manual for details." "Ebib 2.5.4")
(make-obsolete-variable 'ebib-default-entry 'ebib-default-entry-type "Ebib 2.5.4")
(make-obsolete-variable 'ebib-additional-fields 'ebib-extra-fields "Ebib 2.5.4")
(make-obsolete-variable 'ebib-biblatex-inheritance 'ebib-biblatex-inheritances "Ebib 2.5.4")

;; Make sure we can call bibtex-generate-autokey.
(declare-function bibtex-generate-autokey "bibtex" nil)

;;;;;;;;;;;;;;;;;;;;;;
;; global variables ;;
;;;;;;;;;;;;;;;;;;;;;;

;; User customisation.

(defgroup ebib nil "Ebib: a BibTeX database manager." :group 'tex)

(defgroup ebib-windows nil "Ebib window management." :group 'ebib)

(defcustom ebib-default-entry-type "Article"
  "The default entry type.
This is the entry type assigned to newly created entries."
  :group 'ebib
  :type 'string)

(defcustom ebib-preload-bib-files nil
  "List of BibTeX files to load automatically when Ebib starts.
This option allows you to specify which `.bib' file(s) Ebib
should load automatically when it starts up.  Specify one file per
line.  You can complete a partial filename with `M-TAB`."
  :group 'ebib
  :type '(repeat (file :must-match t)))

(defcustom ebib-local-bibfiles t
  "List of a buffer's .bib file(s).
A value of t means that this variable has not been initialized
yet in the relevant buffer.  A value of nil means that there are
no local .bib files.  This variable can be used as a file-local
variable."
  :group 'ebib
  :type '(choice (const :tag "Use `ebib-preload-bib-files'" t)
		 (repeat :tag "Specify .bib files" (file :must-match t))))
(put 'ebib-local-bibfiles 'safe-local-variable (lambda (v) (null (seq-remove #'stringp v))))

(defcustom ebib-bib-search-dirs '("~")
  "List of directories to search for BibTeX files.
This is a list of directories Ebib searches for `.bib' files to
be preloaded.  Note that only the directories themselves are
searched, not their subdirectories.  The directories in this list
are also searched when the function `ebib' is passed a file
name (e.g., from an Eshell command line)."
  :group 'ebib
  :type '(repeat :tag "Search directories for BibTeX files" (string :tag "Directory")))

(defcustom ebib-default-directory nil
  "The default directory for Ebib.
This mainly determines which directory is the default directory
when reading a file name from the user.  Possible values are nil,
which means to use the directory Ebib is started in, the symbol
`first-bib-dir', which means to use the first directory listed in
`ebib-bib-serch-dirs', or a directory path.

In order for this option to take effect, you need to restart Ebib."
  :group 'ebib
  :type '(choice (const :tag "Use Ebib startup directory" nil)
                 (const :tag "Use first .bib directory" first-bib-dir)
                 (directory :tag "Use a specific directory")))

(defcustom ebib-create-backups t
  "Create a backup file.
The first time a BibTeX file is saved, a backup file is created
when it is first saved.  Note that Ebib uses
`make-backup-file-name' to create the name for the backup file."
  :group 'ebib
  :type '(choice (const :tag "Create backups" t)
                 (const :tag "Do not create backups" nil)))

(defcustom ebib-extra-fields '((biblatex "crossref"
					 "xdata"
                                         "annotation"
                                         "abstract"
                                         "keywords"
                                         "file"
                                         "timestamp")
                               (BibTeX "crossref"
                                       "annote"
                                       "abstract"
                                       "keywords"
                                       "file"
                                       "timestamp"
                                       "url"
                                       "doi"))
  "List of the extra fields for BibTeX entries.
Extra fields are fields that are available for all entry types.
Depending on the bibliography style, the value of these fields
may appear in the bibliography, but you may also define fields
that are just for personal use.

Note, before adding fields to this list, check if the field you
want to add is among the fields that are hidden by default.  See
the option \"Hidden Fields\" (`ebib--hidden-fields') for details."
  :group 'ebib
  :type '(set (cons :tag "Dialect" (const biblatex) (repeat :tag "Extra fields" (string :tag "Field")))
              (cons :tag "Dialect" (const BibTeX) (repeat :tag "Extra fields" (string :tag "Field")))))

(defcustom ebib-hidden-fields '("addendum"
                                "afterword"
                                "annotator"
                                "archiveprefix"
                                "bookauthor"
                                "booksubtitle"
                                "booktitleaddon"
                                "chapter"
                                "commentator"
                                "edition"
                                "editora"
                                "editorb"
                                "editorc"
                                "eid"
                                "eprint"
                                "eprintclass"
                                "eprinttype"
                                "eventdate"
                                "eventtitle"
                                "foreword"
                                "holder"
                                "howpublished"
                                "introduction"
                                "isbn"
                                "isrn"
                                "issn"
                                "issue"
                                "issuesubtitle"
                                "issuetitle"
                                "issuetitleaddon"
                                "journaltitleadddon"
                                "journalsubtitle"
                                "language"
                                "location"
                                "mainsubtitle"
                                "maintitle"
                                "maintitleaddon"
                                "month"
                                "origlanguage"
                                "pagetotal"
                                "part"
                                "primaryclass"
                                "remark"
                                "subtitle"
                                "timestamp"
                                "titleaddon"
                                "translator"
                                "urldate"
                                "venue"
                                "version"
                                "volumes")
  "List of fields that are not displayed.
The fields in this list are not displayed by default.  Since
biblatex defines a large number of fields, it is not practical to
display them all in the entry buffer.  You can make these fields
temporarily visible with the command
`\\<ebib-index-mode-map>\\[ebib-toggle-hidden]' or through the
menu."
  :group 'ebib
  :type '(repeat (string :tag "Field")))

(defcustom ebib-layout 'full
  "Ebib window layout.
This option defines how Ebib displays the buffers it uses.  Possible values are:

- `full' (default): Use the entire frame.  The existing windows
are hidden and reappear when Ebib is lowered.

- `window': Use the current window.  This window is split up to
display the index and entry buffers.

- `custom': Use part of the current frame.  The width of the Ebib
windows can be set with the option `ebib-width'.

- `index-only': Display only the index window.  The entry buffer
is displayed when the user edits an entry or after pressing
\\<ebib-index-mode-map>\\[ebib-select-and-popup-entry]."
  :group 'ebib-windows
  :type '(choice (const :tag "Use full frame" full)
                 (const :tag "Use current window" window)
                 (const :tag "Use right part of the frame" custom)
                 (const :tag "Display only index window" index-only)))

(defcustom ebib-width 80
  "Width of the Ebib windows.
The width can be absolute or relative; if it is absolute, it
specifies the number of columns that the Ebib windows occupies.
If it is relative, the with must be a value between 0 and 1
specifying the width relative to the width of the window that is
selected when Ebib is started.

This option only takes effect if `ebib-layout' is set to `custom'."
  :group 'ebib-windows
  :type '(choice (integer :tag "Absolute width")
                 (float :tag "Relative width" :value 0.3)))

(defcustom ebib-popup-entry-window nil
  "Create a popup window to display the entry window.
If `ebib-layout' is set to `index-only', Ebib will use an
existing window to display the entry buffer when needed.  By
setting this option, however, you can tell Ebib to use the
function `display-buffer-pop-up-window' to show the entry buffer,
which (usually) means that a new window will be created.

Note that setting this option has no effect unless `ebib-layout'
is set to `index-only'."
  :group 'ebib-windows
  :type 'boolean)

(defcustom ebib-window-vertical-split nil
  "Display the index buffer at the left of the frame.
Setting this option makes Ebib display the index buffer at the
left side of the frame rather than at the top.  The width of the
window will be `ebib-index-window-size', which you will probably
have to set to a larger value."
  :group 'ebib-windows
  :type 'boolean)

(defcustom ebib-index-window-size 10
  "The size of the index buffer window.
This is either the height of the window, or, if
`ebib-window-vertical-split' is set, the width of the window.
The rest of the frame is used for the entry buffer, unless
`ebib-layout' is set to `index-only'."
  :group 'ebib-windows
  :type 'integer)

(defcustom ebib-index-mode-line '("%e"
                                  mode-line-front-space
                                  ebib--mode-line-modified
                                  mode-line-buffer-identification
                                  (:eval (if (and ebib--cur-db (ebib-db-dependent-p ebib--cur-db))
                                             (format " [%s]" (ebib-db-get-filename (ebib-db-get-main ebib--cur-db) t))))
                                  (:eval (format "  (%s)" (ebib--get-dialect ebib--cur-db)))
                                  (:eval (if (and ebib--cur-db (ebib--get-key-at-point))
                                             "     Entry %l"
                                           "     No Entries"))
                                  (:eval (if (and ebib--cur-db (ebib-db-get-filter ebib--cur-db))
                                             (format "  |%s|" (ebib--filters-pp-filter (ebib-db-get-filter ebib--cur-db)))
                                           "")))
  "The mode line for the index window.
The mode line of the index window shows some Ebib-specific
information.  You can customize this information if you wish, or
disable the Ebib-specific mode line altogether."
  :group 'ebib-windows
  :type '(choice (const :tag "Use standard mode line" nil)
                 (sexp :tag "Customize mode line")))

(defcustom ebib-entry-mode-line '((:eval (ebib--format-entry-info-for-modeline)))
  "The mode line for the entry buffer.
The mode line of the entry window shows the entry key.  You can
customize this information if you wish, or disable the
Ebib-specific mode line altogether."
  :group 'ebib-windows
  :type '(choice (const :tag "Disable mode line" nil)
                 (sexp :tag "Customize mode line")))


(defvar ebib--mode-line-modified '(:eval (ebib--mode-line-modified-p))
  "Mode line construct for database's modified status.")
(put 'ebib--mode-line-modified 'risky-local-variable t)

(defcustom ebib-modified-char "M"
  "Character indicating the modified status in the mode line."
  :group 'ebib-windows
  :type 'string)

(defcustom ebib-index-columns '(("Entry Key" 40 t)
                                ("Author/Editor" 40 t)
                                ("Year" 6 t)
                                ("Title" 50 t))
  "Columns to display in the index buffer.
Each column consists of the BibTeX field to be displayed, which
is also the column's label, the column's maximum width and a flag
indicating whether sorting on this column is possible.

Any BibTeX or biblatex field can be used as a label.  There are
also two special labels: \"Entry Key\" and \"Author/Editor\".
The label \"Entry Key\" displays the entry's BibTeX key, and the
label \"Author/Editor\" displays the contents of the Author
field, or, if that is empty, the contents of the Editor field.

If the sorting flag is t, a sorting function is looked up in
`ebib-field-sort-functions-alist', defaulting to
`string-collate-lessp' if none is found. If it is nil, sorting is
impossible.

Note that the default sort field is the first field in this
option.  See also `ebib-index-default-sort'."
  :group 'ebib
  :type '(repeat (list  (string :tag "Field")
                        (integer :tag "Width")
                        (boolean :tag "Sort"))))

(defcustom ebib-index-default-sort nil
  "Default sort field and direction.
If set, the index buffer is sorted on the specified field and in
the specified direction.  Note that it is not necessary that the
default sort field is visible in the index buffer."
  :group 'ebib
  :type '(choice (const :tag "Use First Index Column" nil)
                 (cons :tag "User-Defined Sort"
                       (string :tag "Sort Field")
                       (choice (const :tag "Ascending Sort" ascend)
                               (const :tag "Descending Sort" descend)))))

(defun ebib-compare-numerical-strings (a b)
  "Return t if A represents a number less than B represents.
A and B are strings (e.g. \"3\" and \"11\")."
  (< (string-to-number a) (string-to-number b)))

(defcustom ebib-field-sort-functions-alist '(("Chapter" . ebib-compare-numerical-strings)
					     ("Sortyear" . ebib-compare-numerical-strings)
					     ("Volume" . ebib-compare-numerical-strings)
					     ("Volumes" . ebib-compare-numerical-strings))
  "Alist of bib(la)tex fields and functions for sorting the index.
Field names are case-insensitive.  Sort functions should take two
arguments and should return t if the first is to be sorted before
the second."
  :group 'ebib
  :type '(alist :key-type string :value-type symbol))

(defcustom ebib-index-column-separator "  "
  "Separator between columns in the index buffer."
  :group 'ebib
  :type 'string)

(defcustom ebib-field-transformation-functions '(("Title" . ebib-clean-TeX-markup-from-entry)
                                                 ("Doi" . ebib-display-www-link)
                                                 ("Url" . ebib-display-www-link)
                                                 ("Note" . ebib-notes-display-note-symbol))
  "Functions transforming field contents to appropriate forms.
Each function should accept three arguments, the field to be
displayed, the key of the entry being displayed, and the database
that contains the entry, and should return a string to be
displayed in the ebib index buffer.  In principle, this string
should be the contents of the field transformed in some way, but
it may actually be anything.  In fact, it is not necessary for
the field to be an actual Bib(La)TeX field, as long as the
transformation function returns something that can be displayed."
  :group 'ebib
  :type '(repeat (cons (string :tag "Field")
                       (function :tag "Transform function"))))

(make-obsolete-variable 'ebib-TeX-markup-replace-alist 'parsebib-TeX-markup-replace-alist "Ebib 2.37")

(defcustom ebib-uniquify-keys nil
  "Create unique keys.
When adding new entries to the database, Ebib does not allow
duplicate keys.  By setting this option, you can tell Ebib to
automatically create a unique key by adding `b', `c'... to it.
This applies when Ebib automatically generates keys for new
entries (see `ebib-autogenerate-keys'), when merging `.bib'
files, and when changing a key."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-autogenerate-keys t
  "Automatically generate keys for new entries.
With this option set, Ebib does not ask for a key when you add a
new entry.  Instead, it gives the entry a temporary key and
assigns a proper key when you finish editing the entry.  This
option uses the function `bibtex-generate-autokey', which has a
number of user-customizable options.  See that function's
documentation for details."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-reference-templates '(("Article"       . "{Author} ({Date|Year}). {\"Title\".} {Journaltitle|Journal}, {Volume}{:Issue}, {Pages}. {Doi|Url.}")
                                      ("Book"          . "{Author|Editor} ({Date|Year}). {\"Title\".} {Publisher.} {Doi|Url.}")
                                      ("MvBook"        . "{Author|Editor} ({Date|Year}). {\"Title\".} {Publisher.} {Doi|Url.}")
                                      ("InBook"        . "{Author} ({Date|Year}). {\"Title\".} In: {Editor}, {\"Booktitle\"}. {Publisher.} {Pages.} {Doi|Url.}")
                                      ("Collection"    . "{Editor} ({Date|Year}). {\"Title\".} {Publisher.} {Doi|Url.}")
                                      ("MvCollection"  . "{Editor} ({Date|Year}). {\"Title\".} {Publisher.} {Doi|Url.}")
                                      ("InCollection"  . "{Author} ({Date|Year}). {\"Title\".} In: {Editor}, {\"Booktitle\"}. {Publisher.} {Pages.} {Doi|Url.}")
                                      ("Misc"          . "{Author|Editor} ({Date|Year}). {\"Title\".} {Howpublished.}")
                                      ("Online"        . "{Author|Editor} ({Date|Year}). {\"Title\".} {Doi|Url.}")
                                      ("Proceedings"   . "{Editor} ({Date|Year}). {\"Title\".} {Organization.}")
                                      ("MvProceedings" . "{Editor} ({Date|Year}). {\"Title\".} {Organization.}")
                                      ("InProceedings" . "{Author} ({Date|Year}). {\"Title\".} In: {Editor}, {\"Booktitle\"}. {Publisher.} {Pages.} {Doi|Url.}")
                                      ("Thesis"        . "{Author} ({Date|Year}). {\"Title\".} {Type,} {Institution.} ")
                                      ("Unpublished"   . "{Author} ({Date|Year}). {\"Title\".} {Howpublished.} {Note.}"))
  "Templates for copying references to the kill ring.
Each template is a string containing literal text and directives
in braces {}.  A directive should contain a field name and is
replaced with the contents of that field.  A directive may
contain multiple fields separated by a pipe bar | (meaning
\"or\"), in which case the contents of the first non-empty field
replaces the directive.  If all fields are empty, the directive
is simply discarded.

A directive may additionally contain punctuation before or after
the field name (or multiple field names).  If the directive is
discarded because its field is empty, any punctuation inside the
braces is discarded as well."
  :group 'ebib
  :type '(repeat (cons (string :tag "Entry type")
                       (string :tag "Template string"))))

(defcustom ebib-citation-template "{Author|Editor} ({Date|Year})"
  "Template used for copying a citation to the kill ring.
For details of the template format, see the user option
                                `ebib-reference-templates'."
  :group 'ebib
  :type '(string :tag "Template"))

(defvar org-link-file-path-type)

(defcustom ebib-link-file-path-type (if (boundp 'org-link-file-path-type) org-link-file-path-type 'adaptive)
  "How the path name in file links should be stored.
Valid values are the same as `org-link-file-path-type'."
  :group 'ebib
  :type '(choice (const relative)
                 (const absolute)
                 (const noabbrev)
	         (const adaptive))
  :safe #'symbolp)

(defun ebib--process-reference-template (template key db)
  "Process TEMPLATE for KEY in DB.
TEMPLATE is a reference template (see the option
`ebib-reference-templates').  Fields in the template are replaced
with the contents of the corresponding fields in the entry KEY in
database DB."
  ;; Define some transformations; Perhaps these should be user-customizable.
  (let ((transformations '(("Title" . parsebib-clean-TeX-markup)
                           ("Date" . ebib-biblatex-date-to-year))))
    (cl-flet ((create-replacements (match)
                                   (save-match-data
                                     (string-match "{\\([^A-Za-z]*\\)\\([A-Za-z|]+\\)\\([^A-Za-z]*\\)}" match)
                                     (let* ((pre (match-string 1 match))
                                            (fields (match-string 2 match))
                                            (post (match-string 3 match))
                                            (field (seq-find (lambda (field)
                                                               (ebib-get-field-value field key db 'noerror nil 'xref))
                                                             (split-string fields "|" t)))
                                            (value (if field (ebib-get-field-value field key db 'noerror 'unbraced 'xref))))
                                       (if value (concat pre
                                                         (funcall (alist-get field transformations #'identity nil #'cl-equalp) value)
                                                         post)
                                         "")))))
      (replace-regexp-in-string "{.*?}" #'create-replacements template nil t))))

(defcustom ebib-citation-commands '((latex-mode
                                     (("cite"   "\\cite%<[%A]%>[%A]{%(%K%,)}")
                                      ("paren"  "\\parencite%<[%A]%>[%A]{%(%K%,)}")
                                      ("foot"   "\\footcite%<[%A]%>[%A]{%(%K%,)}")

                                      ("text"   "\\textcite%<[%A]%>[%A]{%(%K%,)}")
                                      ("smart"  "\\smartcite%<[%A]%>[%A]{%(%K%,)}")
                                      ("super"  "\\supercite{%K}")
                                      ("auto"   "\\autocite%<[%A]%>[%A]{%(%K%,)}")

                                      ("cites"  "\\cites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                                      ("parens" "\\parencites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                                      ("foots"  "\\footcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")

                                      ("texts"  "\\textcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                                      ("smarts" "\\smartcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                                      ("supers" "\\supercites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                                      ("autos"  "\\autoscites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")

                                      ("author" "\\citeauthor%<[%A]%>[%A]{%(%K%,)}")
                                      ("title"  "\\citetitle%<[%A]%>[%A]{%(%K%,)}")
                                      ("year"   "\\citeyear%<[%A]%>[%A][%A]{%K}")
                                      ("date"   "\\citedate%<[%A]%>[%A]{%(%K%,)}")

                                      ("full"   "\\fullcite%<[%A]%>[%A]{%(%K%,)}")))
                                    (org-mode
                                     (("ebib" "[[ebib:%K][%D]]")))
                                    (markdown-mode
                                     (("text" "@%K%< [%A]%>")
                                      ("paren" "[%(%<%A %>@%K%<, %A%>%; )]")
                                      ("year" "[-@%K%< %A%>]"))))
  "A list of format strings used to insert citations into text buffers.
Each item in this option consists of a major mode and a list of
identifier + format strings pairs.  The identifiers (which can be
any string) are used to select the citation command in
`ebib-insert-citation' and `ebib-push-citation'.  The format
strings are used to construct the citation command that is
inserted in the buffer.

The major mode can also be specified as `any', which defines
citation commands that are available in buffers that do not have
any of the major modes listed in this option.

The format string template can contain a number of formatting
directives:

%K: the key of the entry.
%A: an argument; prompts the user.
%D: a description; prompts the user.
%<...%>: optional material surrounding %A.
%(...%): a repeater, which must contain %K.

%A is used for arguments to the citation command, which are
elements such as page numbers, etc.  %A accommodates optional
arguments in LaTeX-based citations and, similarly, optional
material in Pandoc Markdown citations.  %D can be used to provide
a description as used in Org-mode links.  The user is prompted
for this description, but if possible a default is provided,
which can be accepted by hitting RET.

Optional material around %A is only included if the user provides
some string for %A.  If not, the optional material is omitted.

The command `ebib-push-citation' can be used on multiple
entries (by marking them in the index buffer).  If the template
contains a repeater, the material inside this repeater is
processed for each key individually.  If there is no repeater,
all keys are substituted for %K using a separator for which the
user is prompted.

The repeater can optionally contain a separator, which must be
placed between % and ); to use comma as a separator, the format
shring should contain \"%(%K%,)\".  If the separator is not
provided, the user is prompted to supply one."
  :group 'ebib
  :type '(repeat (list :tag "Mode" (symbol :tag "Mode name")
                       (repeat (list :tag "Citation command"
                                     (string :tag "Identifier")
                                     (string :tag "Format string"))))))

(defcustom ebib-citation-insert-multiple nil
  "Allow insertion of multiple citations.
If set, use `ebib-read-entry-multiple' in `ebib-insert-citation'.
This allows inserting multiple keys in one citation command but
has the disadvantage that it is not possible to provide a default
description for citations in Org buffers."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-citation-description-function 'ebib-author-year-description
  "Function for creating a description to be used in citations.
This function is called to provide a description to substitute
for the %D directive in `ebib-citation-commands', and also when
creating Org links with `org-store-link', provided the library
`org-ebib' is loaded.

The default value of this option provides an author/year
description composed of the author or editor field of the entry
and the year field, combined as \"Author (Year)\".  A second
option is to use the Title field on an entry for the link
description.

It is also possible to specify a user-defined function.  This
function should take two arguments: the key of the entry for
which a description is to be created, and the database that
contains the entry."
  :group 'ebib
  :type '(choice (function-item :tag "Author/Year" ebib-author-year-description)
                 (function-item :tag "Title" ebib-title-description)
                 (function :tag "Custom Function")))

(defun ebib-author-year-description (key db)
  "Provide an author/year description for an Org Ebib link.
KEY is the key of the entry to provide the link for, DB the
database that contains the entry."
  (format "%s (%s)"
          (ebib--get-field-value-for-display "Author/Editor" key db)
          (ebib--get-field-value-for-display "Year" key db)))

(defun ebib-title-description (key db)
  "Provide a title description for an Org Ebib link.
KEY is the key of the entry to provide the link for, DB the
database that contains the entry."
  (ebib-get-field-value "Title" key db "(Untitled)" 'unbraced 'xref))

(defcustom ebib-multiline-major-mode 'text-mode
  "The major mode of the multiline edit buffer."
  :group 'ebib
  :type '(function :tag "Mode function"))

(defcustom ebib-multiline-display-function 'ebib-multiline-display-as-is
  "The way in which multiline field values are shown in the index buffer.
By default, the value is shown as-is, but never more than
`ebib-multiline-display-max-lines' lines.  The option \"First
Paragraph\" displays the first paragraph (also with a maximum of
`ebib-multiline-display-max-lines' lines), but in addition
refills the text, which is useful if you use long lines in your
multiline values.

It is possible to provide a custom function for this option.
This function should take a (multiline) string as argument and
return a list of lines to be displayed."
  :group 'ebib
  :type '(choice (function-item :tag "Show Value As-Is" ebib-multiline-display-as-is)
                 (function-item :tag "Show First Paragraph" ebib-multiline-display-paragraph)
                 (function :tag "Custom Function")))

(defun ebib-multiline-display-as-is (string)
  "Reduce the multiline text STRING.
The text is split into lines and returned.  No other
modifications are made."
  (split-string string "\n"))

(defun ebib-multiline-display-paragraph (string)
  "Reduce the multiline text STRING.
The text is filled to account for the possibility that the
original text is unfilled.  Return value is a list of strings,
each a single line."
  (split-string (with-temp-buffer
                  (insert string)
                  (goto-char (point-min))
                  (forward-paragraph)
                  (delete-region (point) (point-max))
                  (fill-region (point-min) (point-max))
                  (buffer-string))
                "\n" t))

(defcustom ebib-multiline-display-max-lines 10
  "The maximum number of lines to display for multiline field values."
  :group 'ebib
  :type 'integer)

(defcustom ebib-sort-order nil
  "The fields on which the BibTeX entries are to be sorted in the BibTeX file.
This option is described in the manual/info file in the section
\"Sorting the .bib file\"."
  :group 'ebib
  :type '(repeat (repeat :tag "Sort level" (string :tag "Sort field"))))

;; Entry type and field aliases defined by Biblatex.
(defconst ebib--field-aliases '(("location" . "address")
                                ("annotation" . "annote")
                                ("eprinttype" . "archiveprefix")
                                ("journaltitle" . "journal")
                                ("sortkey" . "key")
                                ("file" . "pdf")
                                ("eprintclass" . "primaryclass")
                                ("institution" . "school"))
  "List of field aliases for Biblatex.")

(defconst ebib--type-aliases '(("Conference" . "InProceedings")
                               ("Electronic" . "Online")
                               ("MastersThesis" . "Thesis")
                               ("PhDThesis" . "Thesis")
                               ("TechReport" . "Report")
                               ("WWW" . "Online"))
  "List of entry type aliases for Biblatex.")

(defcustom ebib-bibtex-dialect 'BibTeX
  "The default BibTeX dialect.
A `.bib' file/database without explicit dialect setting is
assumed to use this dialect.  Possible values are those listed in
`bibtex-dialect-list'."
  :group 'ebib
  :type `(choice :tag "BibTeX Dialect"
                 ,@(mapcar (lambda (d) `(const ,d))
                           bibtex-dialect-list)))

(defcustom ebib-biblatex-inheritances '(;; Source                        Target
                                        ("all"                           "all"
					 (("ids"                         . none)
					  ("crossref"                    . none)
					  ("xdata"                       . none)
					  ("xref"                        . none)
					  ("entryset"                    . none)
					  ("entrysubtype"                . none)
					  ("execute"                     . none)
					  ("label"                       . none)
					  ("options"                     . none)
					  ("presort"                     . none)
					  ("related"                     . none)
					  ("relatedoptions"              . none)
					  ("relatedstring"               . none)
					  ("relatedtype"                 . none)
					  ("shorthand"                   . none)
					  ("shorthandintro"              . none)
					  ("sortkey"                     . none)))

                                        ;; Source                        Target
					("mvbook, book"                  "inbook, bookinbook, suppbook"
					 (("author"                      . "author")
					  ("author"                      . "bookauthor")))

                                        ;; Source                        Target
					("mvbook"                        "book, inbook, bookinbook, suppbook"
					 (("title"                       . "maintitle")
					  ("subtitle"                    . "mainsubtitle")
					  ("titleaddon"                  . "maintitleaddon")
					  ("shorttitle"                  . none)
					  ("sorttitle"                   . none)
					  ("indextitle"                  . none)
					  ("indexsorttitle"              . none)))

                                        ;; Source                        Target
					("mvcollection, mvreference"     "collection, reference, incollection, inreference, suppcollection"
					 (("title"                       . "maintitle")
					  ("subtitle"                    . "mainsubtitle")
					  ("titleaddon"                  . "maintitleaddon")
					  ("shorttitle"                  . none)
					  ("sorttitle"                   . none)
					  ("indextitle"                  . none)
					  ("indexsorttitle"              . none)))

                                        ;; Source                        Target
					("mvproceedings"                 "proceedings, inproceedings"
					 (("title"                       . "maintitle")
					  ("subtitle"                    . "mainsubtitle")
					  ("titleaddon"                  . "maintitleaddon")
					  ("shorttitle"                  . none)
					  ("sorttitle"                   . none)
					  ("indextitle"                  . none)
					  ("indexsorttitle"              . none)))

                                        ;; Source                        Target
					("book"                          "inbook, bookinbook, suppbook"
					 (("title"                       . "booktitle")
					  ("subtitle"                    . "booksubtitle")
					  ("titleaddon"                  . "booktitleaddon")
					  ("shorttitle"                  . none)
					  ("sorttitle"                   . none)
					  ("indextitle"                  . none)
					  ("indexsorttitle"              . none)))

                                        ;; Source                        Target
					("collection, reference"         "incollection, inreference, suppcollection"
					 (("title"                       . "booktitle")
					  ("subtitle"                    . "booksubtitle")
					  ("titleaddon"                  . "booktitleaddon")
					  ("shorttitle"                  . none)
					  ("sorttitle"                   . none)
					  ("indextitle"                  . none)
					  ("indexsorttitle"              . none)))

                                        ;; Source                        Target
					("proceedings"                   "inproceedings"
					 (("title"                       . "booktitle")
					  ("subtitle"                    . "booksubtitle")
					  ("titleaddon"                  . "booktitleaddon")
					  ("shorttitle"                  . none)
					  ("sorttitle"                   . none)
					  ("indextitle"                  . none)
					  ("indexsorttitle"              . none)))

                                        ;; Source                        Target
					("periodical"                    "article, suppperiodical"
					 (("title"                       . "journaltitle")
					  ("subtitle"                    . "journalsubtitle")
					  ("shorttitle"                  . none)
					  ("sorttitle"                   . none)
					  ("indextitle"                  . none)
					  ("indexsorttitle"              . none))))
  "Inheritance scheme for cross-referencing.
This option defines inheritances for BibLaTeX.  Inheritances are
specified for pairs of source and target entry type, where the
source is the cross-referenced entry and the target the
cross-referencing entry.  For each pair, specify the fields in
the source and the fields in the target that they correspond
with.

Inheritances valid for all entry types can be defined by
specifying \"all\" as the entry type.  Entry types (both source
and target) may also be a (comma-separated) list of entry types.

If no inheritance rule is set up for a given entry type+field
combination, the field inherits from the same-name field in the
cross-referenced entry.  If no inheritance should take place, set
the target field to \"No inheritance\".

All entries made in this option should be in lower case.

Note that this option is only relevant for BibLaTeX.  If the
BibTeX dialect is set to `BibTeX', this option is ignored."
  :group 'ebib
  :type '(repeat (list (string :tag "Source entry type(s)")
                       (string :tag "Target entry type(s)")
                       (repeat (cons :tag "Inheritance"
                                     (string :tag "Source field")
                                     (choice (string :tag "Target field)")
                                             (const :tag "No inheritance" none)))))))

(defcustom ebib-save-xrefs-first t
  "Save entries with a crossref field first in the BibTeX-file.
For BibTeX's cross-referencing to work, the cross-referencing
entries must appear in the `.bib` file before the
cross-referenced entries.  This option tells Ebib to save all
entries with a `crossref` field first, so that BibTeX's
crossreferencing options work as intended.

Note: this option is not compatible with setting the option
`ebib-sort-order'.  If you want to use the latter, unset this
one."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-save-indent-as-bibtex nil
  "If non-nil, use `bibtex-mode' variables for indentation when saving.
By default, when saving a `.bib' file, Ebib uses a single TAB
character to indent fields.  When this option is set, Ebib uses
the values of the variables `bibtex-entry-offset' and
`bibtex-field-indentation' to compute the indentation and indents
using spaces."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-use-timestamp nil
  "Add a timestamp to new entries.
If this option is set, Ebib will add a `timestamp` field to every
new entry, recording the date and time it was added to the
database.  See the section \"Timestamps\" in the manual/info file for
details.

Note that the `timestamp' field is normally hidden.  You can make
it visible with \\[ebib--toggle-hidden] in the index buffer or by
customizing the option `ebib--hidden-fields'."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-timestamp-format "%Y-%m-%d %T (%Z)"
  "Format of the time string used in the timestamp.
This option specifies the format string that is used to create
the timestamp.  The default value produces a timestamp of the
form \"2007-03-12 01:03:26 (CET)\".  This option uses the Emacs
function `format-time-string` to create the timestamp.  See that
function's documentation for details on customizing the format
string."
  :group 'ebib
  :type 'string)

(make-obsolete-variable 'ebib-url-field "The default URL field can no longer be customized" "Ebib 2.27")

(defcustom ebib-url-regexp "\\\\url{\\(.*\\)}\\|https?://[^ ';<>\"\n\t\f]+"
  "Regular expression to extract URLs from a field.
This is the regular expression that Ebib uses to search for URLs
in a field.  With the default value, Ebib considers everything
that is in a LaTeX `\\url{...}' command as a URL, and furthermore
every string of text that starts with `http://' or `https://' and
does not contain whitespace or one of the characters ' \" ; < or >.

Note that the semicolon is added for consistency: it makes it
possible to use the same separator in the `url' field as in the
`file' field."
  :group 'ebib
  :type 'string)

(defcustom ebib-url-download-transformations '(("https?://arxiv.org/abs/" . ebib-transform-arXiv-url)
                                               ("https?://ling.auf.net/lingBuzz/" . ebib-transform-lingbuzz-url)
                                               ("https?://www.jstor.org/" . ebib-transform-jstor-url)
                                               ("https?://\\(www.\\)?aclweb.org/anthology/" . ebib-transform-aclanthology-url)
                                               ("https?://link.aps.org/" . ebib-transform-aps-url))
  "Transformations to apply to a URL before attempting to download a pdf file.
Each entry consists of a matcher and a transformation function.
The matcher is a regular expression that is matched (with
`string-match-p') against a given URL.  If the URL matches, the
transformation function is applied to the URL."
  :group 'ebib
  :type '(repeat (cons :tag "Transformation"
                       (regexp :tag "Matcher") (function :tag "Function"))))

(defun ebib-transform-arXiv-url (url)
  "Transform an arXiv URL to the URL of its correspnoding pdf file."
  (concat (replace-regexp-in-string (regexp-quote "/abs/") "/pdf/" url t t) ".pdf"))

(defun ebib-transform-lingbuzz-url (url)
  "Transform a lingbuzz URL to the URL of its corresponding pdf file."
  (concat url "/current.pdf"))

(defun ebib-transform-jstor-url (url)
  "Transfrom a JSTOR URL to the URL of its corresponding pdf file."
  (save-match-data
    (string-match "\\(https?://www.jstor.org/stable/\\)\\([0-9]*\\)" url)
    (format "%spdf/%s.pdf" (match-string 1 url) (match-string 2 url))))

(defun ebib-transform-aps-url (url)
  "Transform an APS URL to the URL of its corresponding pdf file."
  (save-match-data
    (cond
     ((string-match (regexp-quote "PhysRevA") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/pra/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevB") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prb/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevC") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prc/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevD") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prd/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevE") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/pre/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevFluids") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prfluids/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevLett") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prl/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevAccelBeams") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prab/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevApplied") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prapplied/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevMaterials") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prmaterials/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevResearch") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prresearch/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevX") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prx/pdf/" url t t))
     ((string-match (regexp-quote "RevModPhys") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/rmp/pdf/" url t t))
     ((string-match (regexp-quote "PhysRevPhysEducRes") url)
      (replace-regexp-in-string (regexp-quote "/link.aps.org/doi/") "/journals.aps.org/prper/pdf/" url t t)))))

(defun ebib-transform-aclanthology-url (url)
  "Transform an ACL anthology URL to the URL of its correspnoding pdf file."
  (if (string-match-p "\\.pdf$" url)
	  url
	(concat (string-remove-suffix "/" url) ".pdf")))

(defcustom ebib-browser-command nil
  "Command to call the browser with.
If this option is unset, Ebib uses the Emacs function
`browse-url' to start a browser.  If you prefer not to use this,
you can set this option to the executable name of your preferred
browser.  For this to work, the browser that you use must be able
to handle a URL on the command line."
  :group 'ebib
  :type '(choice (const :tag "Use standard browser" nil)
                 (string :tag "Specify browser command")))

(make-obsolete-variable 'ebib-doi-field "The default DOI field can no longer be customized" "Ebib 2.27")

(make-obsolete-variable 'ebib-file-field "The standard file field can no longer be customized" "Ebib 2.27")

(defcustom ebib-import-directory "~/Downloads/"
  "Directory to import files from.
When using the command `ebib-import-file', this directory is
offered as the default directory."
  :group 'ebib
  :type 'directory)

(defcustom ebib-file-associations '(("pdf" . "xpdf")
                                    ("ps" . "gv"))
  "List of file associations.
Lists of (EXT . HANDLER), where EXT is a file extension like
\"pdf\" or \"ps\" (without dot), and HANDLER is a method to
handle files with those extensions.

HANDLER can be a string, in which case it should be the name of
an executable, which is searched for in the variable `exec-path'.
The string can also contain command-line options to be passed to
the executable.  In this case, the string should also contain
\"%s\", which will be replaced with the path to the file being
opened.

HANDLER can also be a Emacs Lisp function, which should take one
argument, the path to the file being opened.

When you open a file for which no HANDLER is defined, or for
which HANDLER is empty, the file is opened in Emacs using the
function `find-file'."
  :group 'ebib
  :type '(repeat (cons :tag "File association"
                       (string :tag "Extension")
                       (choice (const :tag "Open in Emacs" nil)
                               (string :tag "Run external command")
			       (function :tag "Run Emacs Lisp function")))))

(defcustom ebib-filename-separator "; "
  "Separator for filenames in the \"file\" field'.
The contents of the file field is split up using this separator,
each chunk is assumed to be a filename.

Note that the default value of this option consists of
semicolon-space.  This means you can have semicolons in your file
names, as long as they're not followed by a space."
  :group 'ebib
  :type 'string)

(defcustom ebib-file-search-dirs '("~")
  "List of directories to search when viewing external files.
Note that searching is not recursive: only the files listed here
are searched, not their subdirectories."
  :group 'ebib
  :type '(repeat :tag "Search directories" (string :tag "Directory")))

(defcustom ebib-truncate-file-names t
  "Truncate file names in the file field.
If t, file names entered in the file field are truncated relative
to the directories in `ebib-file-search-dirs'."
  :group 'ebib
  :type '(choice (const :tag "Truncate File Names" t)
                 (const :tag "Do not Truncate File Names" nil)))

(defcustom ebib-name-transform-function 'identity
  "Function for transforming keys into file names.
When `ebib-view-file' is called but no filename is listed in the
file field, the entry key is converted to a filename using this
function."
  :group 'ebib
  :type '(choice (const :tag "Do not apply any function" identity)
                 (function :tag "Apply function")))

(defcustom ebib-file-name-mod-function 'ebib-dont-change-file-name
  "Function to modify a file name in the file field.
This function should take two arguments, the first being the file
name (absolute or relative), the second either t or nil.  If t,
the file name should be modified for storing, if nil the
modifications should be undone so that the file name can be
passed to an external viewer."
  :group 'ebib
  :type '(choice (const :tag "Do not modify file names" ebib-dont-change-file-name)
                 (function :tag "Modification function")))

(defun ebib-dont-change-file-name (file _)
  "Return FILE unchanged.
This function is the default value for `ebib-file-name-mod-function'."
  file)

(defcustom ebib-local-variable-indentation ""
  "Indentation of the local variable block."
  :group 'ebib
  :type '(string :tag "Indentation"))

(defcustom ebib-print-preamble '("\\documentclass{article}")
  "Preamble used for the LaTeX file for printing the database.
This option specifies the preamble that is to be added to the
LaTeX file Ebib creates for printing the database as index cards.
You can set your own `\\usepackage' commands, or anything else
you may need.  See the section \"Printing the Database\" in the
manual/info file for details."
  :group 'ebib
  :type '(repeat (string :tag "Add to preamble")))

(defcustom ebib-print-newpage nil
  "Print each entry on a separate page.
With this option set, Ebib puts every entry on a separate page
when printing index cards.  Otherwise the entries are separated by
a small amount of whitespace only."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-print-multiline nil
  "Include multiline field values when printing the database.
When this options is set, Ebib includes multiline field values
when you print index cards.  Otherwise multiline values are
excluded, which saves space."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-latex-preamble '((biblatex "\\documentclass{article}"
                                           "\\usepackage[style=authoryear-comp,sortlocale=auto]{biblatex}")
                                 (BibTeX "\\documentclass{article}"
                                         "\\bibliographystyle{plain}"))
  "Preamble for the LaTeX file used for BibTeXing the database."
  :group 'ebib
  :type '(set (cons :tag "Dialect" (const biblatex) (repeat :tag "Preamble" (string :tag "Line")))
              (cons :tag "Dialect" (const BibTeX) (repeat :tag "Preamble" (string :tag "Line")))))

(defcustom ebib-print-tempfile ""
  "Temporary file for printing the database.
If set, the commands to print the database (`ebib--print-database'
and `ebib--latex-database') write to this file.  Otherwise you are
asked for a file name."
  :group 'ebib
  :type '(file))

(defcustom ebib-allow-identical-fields nil
  "Handle multiple occurrences of a single field gracefully.
Sometimes BibTeX entries from external sources use multiple
identical fields for some reason (e.g., multiple `keyword'
fields).  Normally, only the last value is read by Ebib, but with
this option set, all values are combined into a single field.  See
the section \"Multiple Identical Fields\" in the manual/info
file."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-bibtex-extensions '(".bib" ".bibtex")
  "List of possible filename extensions of BibTeX files.
When loading a BibTeX filename without extension, Ebib tries to
find a file by adding these extensions.  When creating a new file,
the first extension is added if the filename provided does not
already have an extension.  If you want to create BibTeX files
without extension, add the empty string \"\" to this list or
unset the option entirely."
  :group 'ebib
  :type '(repeat (string :tag "Extension")))

(defcustom ebib-hide-cursor t
  "Hide the cursor in the Ebib buffers.
Normally, the cursor is hidden in Ebib buffers, with the
highlight indicating which entry, field or string is active.  By
unsetting this option, you can make the cursor visible.  Note
that changing this option does not take effect until you restart
Ebib (not Emacs)."
  :group 'ebib
  :type '(choice (const :tag "Hide the cursor" t)
                 (const :tag "Show the cursor" nil)))

(make-obsolete-variable 'ebib-edit-author/editor-without-completion 'ebib-fields-with-completion "Ebib 2.27")
(make-obsolete-variable 'ebib-fields-with-completion 'ebib-field-edit-functions "Ebib 2.34")

(defcustom ebib-field-edit-functions '((("abstract" "addendum" "note" "annotation")
					. ebib--edit-field-as-multiline)
				       (("afterword" "annotator" "author" "bookauthor"
					 "commentator" "editor" "editora" "editorb"
					 "editorc" "foreword" "holder" "introduction"
					 "sortname" "translator")
					. ebib--edit-list-field)
				       (("bookpagination" "pagination") . ebib--edit-pagination-field)
				       (("crossref" "xref")           . ebib--edit-ref-field)
				       (("editortype" "editoratype" "editorbtype" "editorctype")
					. ebib--edit-editor-type-field)
				       (("execute")                   . ebib--edit-TeX-code-field)
				       (("file")		      . ebib--edit-file-field)
				       (("institution")               . ebib--edit-list-field)
				       (("journal" "journaltitle")    . ebib--edit-literal-field)
				       (("keywords")		      . ebib--edit-separated-values-field)
				       (("langid")                    . ebib--edit-lang-id-field)
				       (("language" "origlanguage")   . ebib--edit-language-field)
				       (("location" "origlocation")   . ebib--edit-literal-field)
				       (("organization")	      . ebib--edit-list-field)
				       (("publisher" "origpublisher") . ebib--edit-list-field)
				       (("pubstate")                  . ebib--edit-pubstate-field)
				       (("related" "xdata" "entryset") . ebib--edit-ref-list-field)
				       (("venue")                     . ebib--edit-literal-field))
  "Alist of completion functions for fields.
The car of each element is a list of fields, which must all have
the same BibLaTeX type.  When any of the fields is edited, the
completion function in the cdr of the element is called with
three arguments: the field to be edited, the complete list of
fields in the car and the original contents of the field as a
string as a string if non-empty, or nil otherwise.

If the original contents of the field being edited were braced,
`ebib-edit-field' will preserve this bracing, but will pass an
unbraced string to the relevant function.  As such, the functions
should not handle bracing.

Functions should prompt for a value, using the name of the edited
field in the prompt string, and completing on values of the
fields in the list where appropriate.  They should return a
string, suitable as a field value."
  :group 'ebib
  :type '(repeat (cons (repeat :tag "Fields" (string))
		       (function :tag "Completion function"))))

(defcustom ebib-multiline-fields '("annote" "annotation" "abstract")
  "Fields that are edited as multiline fields by default."
  :group 'ebib
  :type '(repeat (string :tag "Field")))

(defcustom ebib-save-dependent-after-citation t
  "Save a dependent database after `ebib-insert-citation'.
Calling `ebib-insert-citation' in a text buffer associated with a
dependent database may insert a citation from the main database and
add this citation to the dependent database.  In such a case, the
database needs to be saved before running (e.g.) LaTeX on the
file.  If this option is set, the database is automatically saved."
  :group 'ebib
  :type '(choice (const :tag "Save dependent database after citation" t)
                 (const :tag "Only save dependent databases manually" nil)))

(defgroup ebib-faces nil "Faces for Ebib." :group 'ebib)

(defface ebib-highlight-extend-face `((t (:inherit highlight
                                                   ,@(and (>= emacs-major-version 27) '(:extend t)))))
  "Face used for highlighting lines."
  :group 'ebib-faces)

(defface ebib-highlight-face '((t (:inherit highlight)))
  "Face used for highlighting things."
  :group 'ebib-faces)

(defface ebib-crossref-face '((t (:inherit font-lock-comment-face)))
  "Face used to indicate values inherited from crossreferenced entries."
  :group 'ebib-faces)

(defface ebib-alias-face '((t (:inherit warning)))
  "Face used to indicate values inherited from crossreferenced entries."
  :group 'ebib-faces)

(defface ebib-abbrev-face '((t  (:inherit font-lock-doc-face)))
  "Face used to indicate expanded abbreviated/concatenated strings."
  :group 'ebib-faces)

(defface ebib-marked-face '((t (:inverse-video t)))
  "Face to indicate marked entries."
  :group 'ebib-faces)

(defface ebib-modified-face '((t (:inherit error)))
  "Face indicating modified status."
  :group 'ebib-faces)

(defface ebib-field-face '((t (:inherit font-lock-keyword-face)))
  "Face for field names."
  :group 'ebib-faces)

(defface ebib-warning-face '((t (:inherit error)))
  "Face for marking potential problems with the database.
Currently, the following problems are marked:

* Crossreferences to entry keys that do not exist.
* Keywords that have not been saved."
  :group 'ebib-faces)

(defface ebib-link-face '((t (:inherit link)))
  "Face used for marking links."
  :group 'ebib-faces)

(defface ebib-display-author-face '((t (:inherit font-lock-keyword-face)))
  "Face used for the author/editor in completions."
  :group 'ebib-faces)

(defface ebib-display-year-face '((t (:inherit font-lock-variable-name-face)))
  "Face used for the year in completions."
  :group 'ebib-faces)

(defface ebib-display-bibfile-face '((t (:inherit font-lock-comment-face)))
  "Face used for the bibliography file in completions."
  :group 'ebib-faces)

;; Generic for all databases.

;; Constants and variables that are set only once.
(defvar ebib--initialized nil "T if Ebib has been initialized.")

(defvar ebib--buffer-alist nil "Alist of Ebib buffers.")
(defvar ebib--empty-index-buffer-name " Ebib (no file)" "Name of the empty Ebib index buffer.")

;; General bookkeeping.
(defvar ebib--field-history nil "Minibuffer field name history.")
(defvar ebib--filters-history nil "Minibuffer history for filters.")
(defvar ebib--citation-history nil "Minibuffer history for citation commands.")
(defvar ebib--key-history nil "Minibuffer history for entry keys.")
(defvar ebib--keywords-history nil "Minibuffer history for keywords.")

(defvar ebib--saved-window-config nil "Stores the window configuration when Ebib is called.")
(defvar ebib--window-before nil "The window that was active when Ebib was called.")
(defvar ebib--buffer-before nil "The buffer that was active when Ebib was called.")
(defvar ebib--frame-before nil "The frame that was active when Ebib was called.")
(defvar ebib--needs-update nil "Non-nil if the Ebib windows need updating when calling `ebib'.")
(defvar ebib--export-filename nil "Filename to export entries to.")
(defvar ebib--push-buffer nil "Buffer to push entries to.")
(defvar ebib--search-string nil "Stores the last search string.")
(defvar ebib--multiline-buffer-list nil "List of multiline edit buffers.")
(defvar-local ebib--multiline-info nil "Information about the multiline text being edited.")
(defvar ebib--log-error nil "Indicates whether an error was logged.")
(defvar-local ebib--dirty-index-buffer nil "Non-nil if the current index buffer is no longer up-to-date.")

;; The databases.

;; The master list and the current database.
(defvar ebib--databases nil "List of structs containing the databases.")
(defvar ebib--cur-db nil "The database that is currently active.")

;; Bookkeeping required when editing field values or @String definitions.

(defvar ebib--hide-hidden-fields t "If set to T, hidden fields are not shown.")

;; The prefix key and the multiline key are stored in a variable so that the
;; user can customise them.
(defvar ebib--prefix-key ?\;)
(defvar ebib--multiline-key ?\|)

;; This is an AUCTeX variable, but we want to check its value, so let's
;; keep the compiler from complaining.
(eval-when-compile
  (defvar TeX-master))

;; General functions.

(defun ebib--buffer (buffer)
  "Return the buffer object referred to by BUFFER.
BUFFER is a symbol referring to a buffer in
`ebib--buffer-alist'."
  (cdr (assq buffer ebib--buffer-alist)))

(defmacro with-current-ebib-buffer (buffer &rest body)
  "Make BUFFER current and execute BODY.
BUFFER is a symbol referring to a buffer in
`ebib--buffer-alist'."
  (declare (indent defun)
           (debug t))
  `(with-current-buffer (cdr (assq ,buffer ebib--buffer-alist))
     ,@body))

(defun ebib--mark-index-dirty (db)
  "Mark the index buffer of DB as dirty.
An index buffer is dirty if it does not reflect the contents of
its database.  If DB has no index buffer yet, do nothing."
  (let ((buffer (ebib-db-get-buffer db)))
    (if buffer
        (with-current-buffer buffer
          (setq ebib--dirty-index-buffer t)))))

(defmacro with-ebib-window-nondedicated (window &rest body)
  "Execute BODY with WINDOW non-dedicated.
Restore the dedicated status after executing BODY."
  (declare (indent defun)
           (debug t))
  (let ((w (make-symbol "win")))
    `(let* ((,w ,window)
            (dedicated (window-dedicated-p ,w)))
       (unwind-protect
           (progn
             (set-window-dedicated-p ,w nil)
             ,@body)
         (set-window-dedicated-p ,w dedicated)))))

;; We sometimes (often, in fact ;-) need to do something with a string, but
;; take special action (or do nothing) if that string is empty.
;; `ebib--ifstring' makes that easier:

(defmacro ebib--ifstring (bindvar then &rest else)
  "Create a string and test its value.

BINDVAR should be of the form (<var> <value>), where <var> is a
variable name (unquoted symbol) which will be let-bound to the
result of evaluating <value>.  If VALUE is a nonempty string,
THEN (a single sexpr) is executed and its return value returned.
If VALUE is either \"\" or nil, the forms in ELSE are executed
and the return value of its last form is returned."
  (declare (indent 2)
           (debug ((symbolp form) form body)))
  `(let ,(list bindvar)
     (if (not (or (null ,(car bindvar))
                  (equal ,(car bindvar) "")))
         ,then
       ,@else)))

(eval-and-compile
  (defun ebib--execute-helper (env)
    "Helper function for `ebib--execute-when'."
    (cond
     ((eq env 'entries)
      '(ebib-db-has-entries ebib--cur-db))
     ((eq env 'marked-entries)
      '(and ebib--cur-db
            (ebib-db-marked-entries-p ebib--cur-db)))
     ((eq env 'database)
      'ebib--cur-db)
     ((eq env 'real-db)
      '(and ebib--cur-db
            (not (ebib-db-get-filter ebib--cur-db))
            (not (ebib-db-dependent-p ebib--cur-db))))
     ((eq env 'filtered-db)
      '(and ebib--cur-db
            (ebib-db-get-filter ebib--cur-db)))
     ((eq env 'dependent-db)
      '(and ebib--cur-db
            (ebib-db-dependent-p ebib--cur-db)))
     ((eq env 'no-database)
      '(not ebib--cur-db))
     ((eq env 'default) t)
     (t (error "`ebib--execute-when': malformed condition %s" env)))))

(defmacro ebib--execute-when (&rest forms)
  "Macro to facilitate writing Ebib functions.
This functions essentially like a `cond' clause: the basic format
is (ebib--execute-when FORMS ...), where each FORM is built up
as (CONDITION BODY).  CONDITION is a symbol (not quoted) that
specifies under which condition BODY is to be executed.  Valid
symbols are:

`entries': execute if there are entries in the database;
`marked-entries': execute if there are marked entries in the database;
`database': execute if there is a database;
`no-database': execute if there is no database;
`real-db': execute if there is a database that is not filtered nor a dependent;
`filtered-db': execute if there is a filtered database;
`dependent-db': execute if there is a dependent database;
`default': execute if all else fails.

Just like with `cond', only one form is actually executed, the
first one that matches.

CONDITION can also be a list starting with `and' or `or' followed
by two or more condition symbols."
  (declare (indent defun)
           (debug (&rest (sexp form))))
  `(cond
    ,@(mapcar (lambda (form)
                (let ((condition (car form)))
                  (cons (cond
                         ((symbolp condition)
                          (ebib--execute-helper condition))
                         ((listp condition)
                          (unless (memq (car condition) '(and or))
                            (error "`ebib--execute-when': malformed condition"))
                          (cons (car condition) (mapcar (lambda (env)
                                                          (ebib--execute-helper env))
                                                        (cdr condition)))))
                        (cdr form))))
              forms)))

(defun ebib--mode-line-modified-p (&optional db)
  "Return a string describing the modified status of DB.
DB defaults to the current database."
  (or db (setq db ebib--cur-db))
  (when db  ; Note that `ebib--cur-db' may also be nil!
    (if (not (ebib-db-modified-p db))
        " "
      (propertize ebib-modified-char
                  'face 'ebib-modified-face
                  'help-echo "Database modified\nmouse-1: Save database"
                  'local-map '(keymap (mode-line keymap (mouse-1 . ebib-save-current-database)))))))

(defun ebib--log (type format-string &rest args)
  "Write a message to Ebib's log buffer.
TYPE (a symbol) is the type of message: `log' writes the message
to the log buffer only; `message' writes the message to the log
buffer and outputs it with the function `message'; `warning' logs
the message and sets the variable `ebib--log-error' to 0; finally,
`error' logs the message and sets the variable `ebib--log-error'
to 1. The latter two can be used to signal the user to check the
log for warnings or errors.

FORMAT-STRING and ARGS function as in `format'.  A time stamp is
inserted with the log message and a final newline is added as
well.  The return value is always nil."
  (with-current-ebib-buffer 'log
    (cond
     ((eq type 'warning)
      (or ebib--log-error ; If ebib--error-log is already set to 1, we don't want to overwrite it!
          (setq ebib--log-error 0)))
     ((eq type 'error)
      (setq ebib--log-error 1))
     ((eq type 'message)
      (apply #'message format-string args)))
    (insert (format "%s: " (format-time-string "%d %b %Y, %H:%M:%S")))
    (insert (apply #'format (concat (if (eq type 'error)
                                        (propertize format-string 'face 'font-lock-warning-face)
                                      format-string)
                                    "\n")
                   args))))

(defun ebib--read-file-to-list (filename)
  "Return the contents of FILENAME as a list of lines."
  (if (and filename                               ; Protect against `filename' being nil.
           (file-readable-p filename))
      (with-temp-buffer
        (insert-file-contents filename)
        (split-string (buffer-string) "\n" 'omit-nulls))))    ; Nulls are empty lines in this case.

(defun ebib--locate-bibfile (file &optional dirs)
  "Locate and/or expand FILE to an absolute filename in DIRS.
First try to locate BibTeX file FILE with `locate-file' and with
`ebib-bibtex-extensions' as possible suffixes.  If this does not
yield a result, expand FILE with `expand-file-name', adding the
first extension in `ebib-bibtex-extensions' if FILE has no
filename suffix."
  (or (locate-file file (or dirs "/") (append '("") ebib-bibtex-extensions))
      (expand-file-name (if (file-name-extension file)
                            file
                          (concat file (car ebib-bibtex-extensions))))))

(defun ebib--get-file-modtime (file)
  "Return the modification time of FILE.
If FILE cannot be read, return nil."
  (if (file-readable-p file)
      (nth 5 (file-attributes file))))

(defun ebib--ensure-extension (filename ext)
  "Ensure FILENAME has an extension.
Return FILENAME if it alread has an extension, otherwise return
FILENAME appended with EXT.  Note that EXT should start with a
dot."
  (if (file-name-extension filename)
      filename
    (concat filename ext)))

(defun ebib--create-file-name-from-key (key ext)
  "Create a filename from KEY and EXT.
KEY is modified as per `ebib-name-transform-function'.  EXT is
the extension and should not contain a dot.

If KEY matches the regexp \"<new-entry[0-9]+>\", an error is
raised."
  (if (string-match-p "<new-entry[0-9]+>" key)
      (error "Cannot create file name from temporary key")
    (concat (funcall ebib-name-transform-function key)
            "."
            ext)))

(defun ebib--expand-file-name (file)
  "Search and expand FILE.
FILE is a file name, possibly with a partial file path.  It is
expanded relative to `ebib-file-search-dirs'.  If the file cannot
be found, the non-directory part is searched for as well.  As a
last resort, FILE is expanded relative to `default-directory'.
If FILE is an absolute file name, expand it with
`expand-file-name' and return the result."
  (if (file-name-absolute-p file)
      (expand-file-name file)
    (let* ((unmod-file (funcall ebib-file-name-mod-function file nil))
           (ebib-file-search-dirs (append ebib-file-search-dirs
                                          (list (file-name-directory (ebib-db-get-filename ebib--cur-db))))))
      (or (locate-file unmod-file ebib-file-search-dirs)
          (locate-file (file-name-nondirectory unmod-file) ebib-file-search-dirs)
          (expand-file-name unmod-file)))))

(defun ebib--split-files (files)
  "Split FILES (a string) into separate files.
Return value is a list of strings.  The files in FILES should be
separated by `ebib-filename-separator'."
  (split-string files (regexp-quote ebib-filename-separator) t))

(defun ebib--select-file (files n key)
  "Split FILES into separate files and return the Nth.
FILES should be a string of file names separated by
`ebib-filename-separator'.  If there is only one file name in
FILES, it is returned regardless of the value of N.  If N is nil,
the user is asked to enter a number, unless there is only one
file in FILES, in which case that one is chosen automatically.
If FILES is nil, a file name is created on the basis of KEY.  See
the function `ebib--create-file-name-from-key' for details."
  (if (not files)
      (ebib--create-file-name-from-key key "pdf")
    (let ((file-list (ebib--split-files files)))
      (cond
       ((= (length file-list) 1)
        (setq n 1))
       ((null n)
        (setq n (string-to-number (read-string (format "Select file [1-%d]: " (length file-list)))))))
      (unless (<= 1 n (length file-list))  ; Unless n is within range.
        (error "[Ebib] No such file (%d)" n))
      (nth (1- n) file-list))))

(defun ebib--split-urls (urls)
  "Split URLS (a string) into separate URLs.
Return value is a list of strings.  The URLs in URLS are
  separated using `ebib-url-regexp'."
  (let ((start 0)
        (result nil))
    (while (string-match ebib-url-regexp urls start)
      (push (match-string 0 urls) result)
      (setq start (match-end 0)))
    (nreverse result)))

(defun ebib--select-url (urls n)
  "Select a URL from URLS.
URLS is a string containing one or more URLs.  URLS is split
using `ebib-url-regexp' and the Nth URL is returned.  If N is
nil, the user is asked which URL to select, unless there is only
one.  If URLS is nil or does not contain any valid URLs, raise an
error."
  (unless urls
    (error "[Ebib] No URLs found"))
  (setq urls (ebib--split-urls urls))
  (unless urls
    (error "[Ebib] No valid URLs found"))
  (cond
   ((= (length urls) 1)
    (setq n 1))
   ((null n) ; The user didn't provide a numeric prefix argument.
    (setq n (string-to-number (read-string (format "Select URL to open [1-%d]: " (length urls)))))))
  (unless (<= 1 n (length urls))  ; Unless n is within range.
    (error "[Ebib] No such URL (%d)" n))
  (let ((url (nth (1- n) urls)))
    (if (string-match "\\\\url{\\(.*?\\)}" url) ; See if the url is contained in \url{...}.
        (setq url (match-string 1 url)))
    url))

(defun ebib-create-org-identifier (key _)
  "Create a unique identifier for KEY for use in an org file.
The key is prepended with the string \"Custom_id:\", so that it
can be used in a :PROPERTIES: block."
  (format ":Custom_id: %s" key))

(defun ebib-create-org-title (key db)
  "Return a title for an Org mode note for KEY in DB.
The title is formed from the title of the entry.  Newlines are
removed from the resulting string."
  (ebib--ifstring (title (ebib-clean-TeX-markup-from-entry "title" key db))
      (remove ?\n (format "%s" title))
    "(No Title)"))

(defun ebib-create-org-description (key db)
  "Return a description for an Org mode note for KEY in DB.
The title is formed from the author(s) or editor(s) of the entry,
its year and its title.  Newlines are removed from the resulting
string."
  (let ((author (or (ebib-get-field-value "author" key db 'noerror 'unbraced 'xref)
                    (ebib-get-field-value "editor" key db 'noerror 'unbraced 'xref)
                    "(No Author)"))
        (year (ebib-get-year-for-display key db))
        (title (or (ebib-get-field-value "title" key db 'noerror 'unbraced 'xref)
                   "(No Title)")))
    (remove ?\n (format "%s (%s): %s" author year title))))

(defun ebib-create-org-cite (key _db)
  "Return a citation for an Org mode note for KEY in DB."
  (format "cite:%s" key))

(defun ebib-create-org-link (key db)
  "Create an org link for KEY in DB.
Check the entry designated by KEY whether it has a file, a doi or
a URL (in that order) and use the first element found to create
an org link.  If none of these elements is found, return the
empty string."
  (cond
   ((ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)
    (ebib-create-org-file-link key db))
   ((ebib-get-field-value "doi" key db 'noerror 'unbraced 'xref)
    (ebib-create-org-doi-link key db))
   ((ebib-get-field-value "url" key db 'noerror 'unbraced 'xref)
    (ebib-create-org-url-link key db))
   (t "")))

(defun ebib-create-org-file-link (key db)
  "Create an org link to the file in entry KEY in DB.
The file is taken from the \"file\" filed in the entry designated
by KEY in the current database.  If that field contains more than
one file name, the user is asked to select one.  If
the \"file\" field is empty, return the empty string."
  (let ((files (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)))
    (if files
        (let* ((absolute-path (ebib--expand-file-name (ebib--select-file files nil key)))
               (relative-path (file-relative-name absolute-path default-directory))
               (abbreviate-path (abbreviate-file-name absolute-path))
               (final-path
                (cl-case ebib-link-file-path-type
                  (relative relative-path)
                  (adaptive (if (string-match (concat "^" (regexp-quote default-directory))
                                              absolute-path)
                                relative-path
                              abbreviate-path))
                  (otherwise absolute-path))))
          (format "[[file:%s]]" final-path))
      "")))

(defun ebib-create-org-doi-link (key db)
  "Create an org link to the DOI in entry KEY in DB.
The file is taken from the \"doi\" in the entry designated by KEY
in the current database.  If the \"doi\" field is empty, return
the empty string."
  (let ((doi (ebib-get-field-value "doi" key db 'noerror 'unbraced 'xref)))
    (if doi (format "[[doi:%s]]" doi) "")))

(defun ebib-create-org-url-link (key db)
  "Create an org link to the URL in entry KEY in DB.
The URL is taken from \"url\" in the entry designated by KEY in
the current database.  If that field contains more than one url,
the user is asked to select one.  If \"url\" is empty, return the
empty string."
  (let* ((urls (ebib-get-field-value "url" key db 'noerror 'unbraced 'xref))
         (url (ebib--select-url urls nil)))
    (if url (format "[[%s]]" url) "")))

(defun ebib-format-template (template specifiers &rest args)
  "Format TEMPLATE using SPECIFIERS.
SPECIFIERS is an alist of characters and symbols.  Each symbol
should be the name of a function that takes ARGS as arguments and
returns a string which is substituted for the specifier in
TEMPLATE.  Specs in SPECIFIERS that do not occur in TEMPLATE are
ignored."
  ;; First remove specs that do not occur in TEMPLATE.  In principle,
  ;; `format-spec' ignores all specs that do not occur in the template, but we
  ;; do not want to apply the functions of specs that are not needed.
  (setq specifiers (cl-remove-if-not (lambda (elt)
                                       (string-match-p (format "%%%c" (car elt)) template))
                                     specifiers))
  (format-spec template (delq nil (mapcar (lambda (spec)
                                            (let* ((replacer (cdr spec))
                                                   (replacement (if (fboundp replacer)
                                                                    (ignore-errors (apply (cdr spec) args))
                                                                  (symbol-value replacer))))
                                              (if replacement
                                                  (cons (car spec) replacement))))
                                          specifiers))))

(defun ebib--multiline-p (string)
  "Return non-nil if STRING is multiline."
  (if (stringp string)
      (string-match-p "\n" string)))

(defun ebib--first-line (string)
  "Return the first line of a multiline STRING."
  (car (split-string string "\n")))

(defun ebib--match-all-in-string (match-str string)
  "Highlight all the occurrences of MATCH-STR in STRING.
The return value is a list of two elements: the first is the
modified string, the second either t or nil, indicating whether a
match was found at all."
  (cl-do ((counter 0 (match-end 0)))
      ((not (string-match match-str string counter)) (cl-values string (not (= counter 0))))
    (add-text-properties (match-beginning 0) (match-end 0) '(face ebib-highlight-face) string)))

(defun ebib--looking-at-goto-end (regexp &optional match)
  "Return non-nil if text after point matches REGEXP and move point.
MATCH acts just like the argument to MATCH-END, and defaults to
0."
  (or match (setq match 0))
  (let ((case-fold-search t))
    (if (looking-at regexp)
        (goto-char (match-end match)))))

(defun ebib--special-field-p (field)
  "Return t if FIELD is a special field.
Special fields are those whose names start and end with an equal sign."
  (string-match-p "\\`=[[:alpha:]]*=\\'" field))

(defun ebib--local-vars-to-list (str)
  "Convert STR to a list of local variables.
STR must start with \"Local Variables:\" and end with \"End:\".
The return value is a list of lists, where each sublist has the
form (\"<variable>\" \"<value>\"). If STR is not a local variable
block, the return value is nil."
  (let ((vars (split-string str "[{}\n]+" t "[{} \t\r]+")))
    (when (and (string= (car vars) "Local Variables:")
               (string= (car (last vars)) "End:"))
      (mapcar (lambda (elt)
                (split-string elt ": " t "[ \t]+"))
              (seq-subseq vars 1 -1)))))

(defun ebib--get-dialect (db)
  "Get the dialect of DB.
If DB has no dialect, return the default dialect, as stored in
`ebib-bibtex-dialect'."
  (or (and db (ebib-db-get-dialect db))
      ebib-bibtex-dialect))

(defun ebib--local-vars-add-dialect (vars dialect &optional overwrite)
  "Expand local variable block VARS with DIALECT.
VARS is a list as returned by `ebib--local-vars-to-list'.
DIALECT must be a symbol, possible values are listed in
`bibtex-dialect-list'.  If OVERWRITE is non-nil, overwrite an
existing dialect variable, otherwise do nothing.  The return
value is the (un)modified list."
  (let ((item (seq-find (lambda (elt) (string= (car elt) "bibtex-dialect")) vars)))
    (if item
        (when overwrite
          (setcdr item (list (symbol-name dialect))))
      (setq vars (push (list "bibtex-dialect" (symbol-name dialect)) vars)))
    vars))

(defun ebib--local-vars-delete-dialect (vars)
  "Delete the dialect definition from VARS.
VARS is a list as returned by `ebib--local-vars-to-list'.  VARS is
not modified, instead the new list is returned."
  (seq-remove (lambda (elt)
                (string= (car elt) "bibtex-dialect"))
              vars))

;; The numeric prefix argument is 1 if the user gave no prefix argument at all.
;; The raw prefix argument is not always a number.  So we need to do our own
;; conversion.
(defun ebib--prefix (num)
  "Return NUM if it is a number, otherwise return nil.
This can be used to check if the user provided a numeric prefix
argument to a function or not."
  (when (numberp num)
    num))

;;; Functions for database access
;;
;; We can mostly use the ebib-db-* functions directly, but we don't want to handle
;; the braces in `ebib-db.el', so we define some extra access functions here.

(defun ebib--store-entry (entry-key fields db &optional timestamp if-exists)
  "Store the entry defined by ENTRY-KEY and FIELDS into DB.
Optional argument TIMESTAMP indicates whether a timestamp is to
be added to the entry.  Note that for a timestamp to be added,
`ebib-use-timestamp' must also be set to T. IF-EXISTS is as for
`ebib-db-set-entry'.

If storing the entry was successful, return the key under which
the entry is actually stored (which, if IF-EXISTS is `uniquify',
may differ from ENTRY-KEY); otherwise return nil.  Depending on
the value of IF-EXISTS, storing an entry may also result in an
error."
  (let ((actual-key (ebib-db-set-entry entry-key fields db if-exists)))
    (when (and actual-key timestamp ebib-use-timestamp)
      (ebib-set-field-value "timestamp" (format-time-string ebib-timestamp-format) actual-key db 'overwrite))
    actual-key))

(defun ebib-set-field-value (field value key db &optional if-exists nobrace)
  "Set FIELD to VALUE in entry KEY in database DB.

IF-EXISTS determines what to do if the field already exists.  If
it is `overwrite', the existing value is overwritten.  If it is
`noerror', the value is not stored and the function returns nil.
If it is nil (or any other value), an error is raised.

IF-EXISTS can also be the symbol `append' or a string.  In this
case, the new value is appended to the old value, separated by a
space or by the string.  Before appending, braces/double quotes
are removed from both values.  Whether braces are added to the
new value depends on the value of NOBRACE.

If NOBRACE is t, the value is stored without braces.  If it is
nil, braces are added if not already present.  NOBRACE may also be
the symbol `as-is', in which case the value is stored as is.

A field can be removed from the entry by passing nil as VALUE and
setting IF-EXISTS to `overwrite'.

Return t upon success, or nil if the value could not be stored."
  (if (eq if-exists 'append)
      (setq if-exists " "))
  (let ((old-value (ebib-db-get-field-value field key db 'norerror)))
    ;; If the field has a value, decide what to do:
    (if old-value
        (cond
         ((eq if-exists 'overwrite)
          (setq old-value nil))
         ((stringp if-exists)
          (setq value (concat (ebib-unbrace old-value) if-exists (ebib-unbrace value)))
          (setq old-value nil))
         ((not (eq if-exists 'noerror))
          (error "[Ebib] Field `%s' exists in entry `%s'; cannot overwrite" field key))))
    ;; If there is (still) an old value, do nothing.
    (unless old-value
      ;; Otherwise overwrite the existing entry. Note that to delete a field, we
      ;; set its value to nil, rather than removing it altogether from the
      ;; database. In `ebib--display-fields', such fields are ignored, and they're
      ;; not saved.
      (if (and value nobrace)
          (unless (eq nobrace 'as-is)
            (setq value (ebib-unbrace value)))
        (setq value (ebib-brace value)))
      (ebib-db-set-field-value field value key db 'overwrite))))

(defcustom ebib-expand-strings t
  "Whether to expand abbreviated/concatenated fields for display.
If set to t, string abbreviations and concatenations are always
expanded.  If a list of strings, then fields which match one of
the members (case-insensitively) will be expanded.  If a field
with an alias is expanded, values in the alias field are expanded
too.  If only the alias name is in the list, only the alias is
expanded.  If set to nil, nothing is ever expanded.

Expanded values are shown in `ebib-abbrev-face'.

When editing a field, the actual (unexpanded) value is always
shown."
  :group 'ebib
  :type '(choice (const :tag "Always expand" t)
                 (const :tag "Never expand" nil)
		 (repeat :tag "Expand only listed fields" (string :tag "Field"))))

(defun ebib-get-entry (key db &optional noerror xref)
  "Return entry KEY in database DB as an alist.
The entry is returned as an alist of (FIELD . VALUE) pairs.
Trigger an error if KEY does not exist, unless NOERROR is t.  If
XREF is non-nil, add any fields that are inherited from a
cross-referenced entry to the alist.  Note that inherited fields
are not marked in any way, so they cannot be distinguished in the
return value."
  (if-let ((entry (ebib-db-get-entry key db noerror))
	   (xref)
           (inheritances (if (eq (ebib--get-dialect db) 'BibTeX)
                             'BibTeX
                           ebib-biblatex-inheritances))
	   (xref-key-alist (ebib--get-xref-alist key db)))
      ;; Try inheriting from each key in `xref-key-alist' in turn,
      ;; until `value' is non-nil
      (cl-loop for (xref-type . xref-key) in xref-key-alist
	       do
	       (when-let (((ebib-db-has-key xref-key db))
			  (xref-entry (ebib-get-entry xref-key db noerror 'xref))
			  (source-type (ebib-db-get-field-value "=type=" xref-key db noerror))
			  ;; When the cross-reference is from an xdata field, check if entry
			  ;; referred to is an @Xdata entry. Otherwise anything is fine.
			  ((if (cl-equalp xref-type "xdata")
			       (cl-equalp source-type "xdata")
			     t)))
		 (setq entry (parsebib--get-xref-fields entry xref-entry inheritances)))
	       finally return entry)
    entry))

(defun ebib--propertize-xdata-warnings (parts xkey xfield &optional xindex help-echo)
  "Return a granular xdata ref string with warning properties.
PARTS is a list containing any the symbols `key' `field' and
`index', in any order.  XKEY is the key of the XData entry being
referred to.  XFIELD is the field referred to in that entry,
.XINDEX is the index of the item in the value of the field, if
provided.  See the BibLaTeX manual, section 3.13.6 for more
details.

Returns a string \"xdata-XKEY-XFIELD[-XINDEX]\". XINDEX and the
preceding dash are only included if XINDEX is non-nil. Each of the
items in PARTS is highlighted in `ebib-warning-face'. Dashes between
two adjacent highlighted parts are also highlighted. All highlighted
parts also have their `help-echo' text property set to HELP-ECHO.

Intended as a convenience function for use in
`ebib--replace-granular-xdata-references'."
  (concat
   "xdata="
   (apply 'propertize `(,xkey ,@(when (member 'key parts) `(face ebib-warning-face help-echo ,help-echo))))
   (apply 'propertize `("-" ,@(when (and (member 'key parts) (member 'field parts))
				`(face ebib-warning-face help-echo ,help-echo))))
   (apply 'propertize `(,xfield ,@(when (member 'field parts) `(face ebib-warning-face help-echo ,help-echo))))
   (when xindex
     (concat
      (apply 'propertize `("-" ,@(when (and (member 'field parts) (member 'index parts))
				   `(face ebib-warning-face help-echo ,help-echo))))
      (apply 'propertize `(,xindex ,@(when (member 'index parts) `(face ebib-warning-face help-echo ,help-echo))))))))

(defun ebib--replace-granular-xdata-references (string db)
  "Replace xdata references within STRING with their values.
Replace all strings of the form \"xdata=-KEY-FIELD[-INDEX]\" with
the value of FIELD in entry with KEY.  If INDEX is present, parse
the value as a list (splitting on \"and\" and removing
surrounding whitespace) and return the element at INDEX,
beginning at 1.

See BibLaTeX manual, Sec. 3.13.6 for more details.

DB is the database to use when finding the database for each
KEY (see `ebib--find-db-for-key').

If successful, replaced text has the text property `ebib--xref', with
the key of the entry inherited from as its value.

If unsuccessful, text is propertized appropriately:
- if the entry at KEY does not exist, or is not an xdata entry,
  highlight key with `ebib-warning-face' and set the help-echo
  property to an explanatory message.
- if INDEX is present, but the field is not indexable (i.e. is known
  to be of a type which is not a list), highlight FIELD-INDEX and set
  help-echo saying that FIELD is not indexable
- If INDEX is present but there are not enough items, highlight just
  INDEX, and set appropriate help-echo message."
  (let ((xdata-regexp (rx "xdata="
			  (group (one-or-more (in alnum  ?_ ?: ?\; ?! ??))) ;; Key of @XData entry
			  "-"
			  (group (one-or-more (in alpha))) ;; Field in @XData entry
			  (optional "-" (group (one-or-more (in digit)))))) ;; Optional index into field value
	(get-xdata-function
	 (lambda (match-text)
	   (let ((xdata-key (match-string 1 match-text))
		 (xdata-field (match-string 2 match-text))
		 (xdata-index (match-string 3 match-text)))
	     ;; Does the entry exist?
	     (if (not (ebib-db-get-entry xdata-key db t))
		 (ebib--propertize-xdata-warnings '(key) xdata-key xdata-field nil (format "No entry with key `%s'" xdata-key))
	       (let* ((xdata-db (ebib--find-db-for-key xdata-key db))
		      (xdata-value (ebib-unbrace (ebib-get-field-value
						  xdata-field xdata-key xdata-db 'noerror 'xref 'expand-strings))))
		 (cond ;; Is it an xdata entry?
		  ((not (cl-equalp "xdata" (ebib-db-get-field-value "=type=" xdata-key xdata-db 'noerror)))
		   (ebib--propertize-xdata-warnings '(key) xdata-key xdata-field (or xdata-index) (format "`%s' is not an @XData entry" xdata-key)))
		  ;; Is there a value?
		  ;; TODO check that this is the right format
		  ((not xdata-value)
		   (ebib--propertize-xdata-warnings
		    '(key field) xdata-key xdata-field (or xdata-index)
		    (format "No value for `%s' in entry `%s'" xdata-field xdata-key)))
		  ;; Is there an index?
		  (xdata-index
		   ;; Is the field indexable?
		   (if (or (member
			    xdata-field
			    '("address" "afterword" "annotator" "author"
			      "bookauthor" "commentator" "editor"
			      "editora" "editorb" "editorc" "foreword"
			      "holder" "institution" "introduction"
			      "language" "location" "organization"
			      "origlanguage" "origlocation" "origpublisher"
			      "pageref" "publisher" "school" "shortauthor"
			      "shorteditor" "sortname" "translator"))
			   ;; ...or is it field which only the user uses (valid because
			   ;; BibLaTeX ignores fields it doesn't know about)...
			   (not (member xdata-field (mapcar #'car bibtex-biblatex-field-alist))))
		       (if-let (val (nth (- (string-to-number xdata-index) 1)
					 (save-match-data
					   (split-string xdata-value "and" t "[[:space:]]*"))))
			   (propertize val 'ebib--xref xdata-key)
			 (ebib--propertize-xdata-warnings '(index) xdata-key xdata-field xdata-index "Not enough items in field"))
		     (ebib--propertize-xdata-warnings '(field index) xdata-key xdata-field xdata-index (format "`%s' is not an indexable field" xdata-field))))
		  ;; Everything is good, no index, just return the value
		  (t (propertize xdata-value 'ebib--xref xdata-key)))))))))
    (replace-regexp-in-string xdata-regexp get-xdata-function string)))

(defun ebib--get-xref-alist (key db)
  "Return an alist of cross-referencing keys in entry KEY, in DB.
Only keys entered as values in the \"crossref\", \"xdata\" and
\"xref\" fields are considered (keys used in \"granular\" xdata
entries like \"xdata=fookey-foofield\" are not considered).

The car of each element is the string name of the field
referencing the key (\"xdata\" or \"crossref\").  The cdr is the
key itself.

Keys in the \"xdata\" field are listed first, in the same order
as in the entry. The crossref key is listed last.  This reflects
the precedence given to these keys by BibLaTeX's inheritance
system."
  (let* ((xdata-field-value (ebib-get-field-value "xdata" key db 'noerror 'unbraced))
	 (xdata-key-list (when xdata-field-value
			   (save-match-data (split-string xdata-field-value ",[[:space:]]*"))))
	 (crossref-key (ebib-get-field-value "crossref" key db 'noerror 'unbraced))
	 (xref-key (ebib-get-field-value "xref" key db 'noerror 'unbraced)))
    ;; Alist of name of refering field, and bibkey to which it refers. Order
    ;; matters here -- earlier xdata keys take precedence, then later ones, then
    ;; the crossref key (this follows the BibLaTeX implementation).  As a
    ;; precaution, cross-referencing fields that reference the entry itself are
    ;; removed, because they would throw Ebib in an infinite loop.
    (cl-remove-if (lambda (e)
                    (string= key (cdr e)))
                  `(,@(mapcar (lambda (key) `("xdata" . ,key)) xdata-key-list)
                    ,@(when crossref-key `(("crossref" . ,crossref-key)))
                    ,@(when xref-key `(("xref" . ,xref-key)))))))

(defun ebib-get-field-value (field key db &optional noerror unbraced xref expand-strings)
  "Return the value of FIELD in entry KEY in database DB.
If FIELD or KEY does not exist, trigger an error, unless NOERROR
is non-nil.  In this case, if NOERROR is a string, return NOERROR,
otherwise return nil.  If UNBRACED is non-nil, return the value
without braces.

If XREF is non-nil, the field value may be retrieved from a
cross-referenced entry.  If the result is non-nil, the returned
text has the text property `ebib--xref', which has as value the
key of the entry from which the field value was retrieved.

If EXPAND-STRINGS is non-nil and the field is unbraced then
abbreviation strings and concatenation are expanded.  The result
has the text property `ebib--expanded' with value t.

Similarly, the value can be retrieved from an alias field.  (See
the variable `ebib--field-aliases').  In this case, the returned
string has the text property `ebib--alias' with value t."
  (let* ((value (ebib-db-get-field-value field key db 'noerror))
         (type (ebib-db-get-field-value "=type=" key db 'noerror))
         (xref-key-alist)
         (alias))
    (when (and (not value) xref)      ; Check if there's a cross-reference.
      (setq xref-key-alist (cl-remove-if
			    ;; Don't consider entries in the `xref'
			    ;; field -- they don't inherit data.
			    (lambda (x) (cl-equalp (car x) "xref"))
			    (ebib--get-xref-alist key db)))
      (when xref-key-alist
	;; Try inheriting from each key in `xref-key-alist' in turn,
	;; until `value' is non-nil
	(cl-loop for (xref-type . xref-key) in xref-key-alist until value
		 do
		 (when-let ((xref-db (ebib--find-db-for-key xref-key db))
			    (source-type (ebib-db-get-field-value "=type=" xref-key xref-db 'noerror))
			    (xref-field (ebib--get-xref-field field type source-type (ebib-db-get-dialect db)))
			    ;; When the cross-reference is from an xdata field, check if entry
			    ;; referred to is an @Xdata entry. Otherwise anything is fine.
			    ((if (cl-equalp xref-type "xdata")
				 (cl-equalp source-type "xdata")
			       t))
			    (xref-value (ebib-get-field-value xref-field xref-key xref-db 'noerror nil 'xref)))
		   (setq value (propertize xref-value 'ebib--xref xref-key))))))
    (when (and (not value)
               (eq (ebib--get-dialect db) 'biblatex))                   ; Check if there is a field alias
      (setq alias (cdr (assoc-string field ebib--field-aliases 'case-fold)))
      (if alias
          (setq value (ebib-db-get-field-value alias key db 'noerror))))
    (unless (or value noerror)
      (error "[Ebib] Field `%s' does not exist in entry `%s'" field key))
    (unless (= 0 (length value)) ; (length value) == 0 if value is nil or if value is "".
      (setq value (copy-sequence value)) ; Copy the value so we can add text properties.
      (when (and (ebib-unbraced-p value)
		 expand-strings
		 ;; Fields consisting of only numerical values can be
		 ;; written without bracing or quotes, and not be
		 ;; expanded. Only continue if the value is not so.
		 (not (string-match "\\`[[:digit:]:/-]+\\'" value))
		 (or (eq ebib-expand-strings t)
		     (assoc-string field ebib-expand-strings 'case-fold)
		     (assoc-string alias ebib-expand-strings 'case-fold)))
	(setq value (ebib--expand-string value ebib--cur-db 'noerror)))
      (when (and value xref)
	(setq value (ebib--replace-granular-xdata-references value db)))
      (when unbraced
        (setq value (ebib-unbrace value)))
      (when alias
        (add-text-properties 0 (length value) '(ebib--alias t) value)))
    (when (and (not value)
               (stringp noerror))
      (setq value noerror))
    value))

;; Functions for writing out Bib(La)TeX entries.
(defun ebib--format-entry (key db &optional timestamp sort)
  "Write entry KEY in DB into the current buffer in BibTeX format.
If TIMESTAMP is non-nil and `ebib-use-timestamp' is set, a
timestamp is added to the entry, possibly overwriting an existing
timestamp.  If SORT is non-nil, the fields are sorted before
formatting the entry."
  (let* ((entry (copy-alist (ebib-db-get-entry key db 'noerror)))
         (type (cdr (assoc "=type=" entry))))
    (when entry
      (if (and timestamp ebib-use-timestamp)
          (setcdr (assoc-string "timestamp" entry 'case-fold) (format-time-string (concat "{" ebib-timestamp-format "}"))))
      (setq entry (seq-filter (lambda (field)
                                (and (cdr field) ; Remove fields with value nil. See `ebib-set-field-value'.
                                     (not (ebib--special-field-p (car field)))))
                              entry))
      (setq entry (if sort
                      (cl-sort entry #'string< :key #'car)
                    ;; When reading, fields are stored with `push', so if we don't
                    ;; sort, we need to reverse them to get the original order
                    ;; back.  See github issues #42, #55, #62.
                    (reverse entry)))
      (insert (format "@%s{%s,\n" type key))
      (let ((indent (if ebib-save-indent-as-bibtex
                        (make-string (+ bibtex-entry-offset bibtex-field-indentation) ?\s)
                      "\t")))
        (insert (mapconcat (lambda (field)
                             (format "%s%s = %s" indent (car field) (cdr field)))
                           entry
                           ",\n")))
      (insert "\n}\n\n"))))

(defun ebib--format-comments (db)
  "Write the @Comments of DB into the current buffer in BibTeX format."
  (mapc (lambda (c)
          (insert (format "@Comment%s\n\n" c)))
        (ebib-db-get-comments db)))

(defun ebib--format-main (db)
  "Write DB's main database to the current buffer."
  (let ((main (ebib-db-get-main db)))
    (when main
      (insert (format "@Comment{\nebib-main-file: %s\n}\n\n"
                      (ebib-db-get-filename main))))))

(defun ebib--format-strings (db)
  "Write the @Strings of DB into the current buffer in BibTeX format."
  (mapc (lambda (str)
          (insert (format "@String{%s = %s}\n" (car str) (cdr str))))
        (ebib-db-get-all-strings db))
  (insert "\n"))

(defun ebib--get-sortstring (entry-key sortkey-list db)
  "Return the field value on which the entry ENTRY-KEY is to be sorted.
SORTKEY-LIST is a list of fields that are considered in order for
the sort value.  DB is the database that contains the entry
referred to by ENTRY-KEY."
  (let ((sort-string nil))
    (while (and sortkey-list
                (null (setq sort-string (ebib-get-field-value (car sortkey-list) entry-key db 'noerror 'unbraced))))
      (setq sortkey-list (cdr sortkey-list)))
    sort-string))

(defun ebib--format-local-vars (db)
  "Write the local variables of DB into the current buffer."
  (let ((lvars (ebib-db-get-local-vars db)))
    (when lvars
      (insert (concat "@Comment{\n"
                      ebib-local-variable-indentation "Local Variables:\n"
                      (mapconcat (lambda (e) (format "%s%s: %s\n" ebib-local-variable-indentation (car e) (cadr e))) lvars "")
                      ebib-local-variable-indentation "End:\n"
                      "}\n\n")))))

(defun ebib--find-db-for-key (key db)
  "Return the database containing KEY.
First see if KEY is in DB.  If not, check all members of
`ebib--databases'.  If there is no database with KEY, return
nil."
  (if (ebib-db-get-entry key db 'noerror)
      db
    (let ((dbs (reverse ebib--databases)))
      (while (and dbs (not (ebib-db-has-key key (car dbs))))
        (setq dbs (cdr dbs)))
      (car dbs))))

(defun ebib--get-xref-field (target-field target-entry source-entry &optional dialect)
  "Return the field from which TARGET-FIELD inherits.
TARGET-ENTRY is the type of the cross-referencing entry, (the
entry containing the field that should inherit a value).
SOURCE-ENTRY is the entry type of the cross-referenced
entry, (the entry providing the value).  DIALECT is a BibTeX
dialect and defaults to the default value of
`ebib-bibtex-dialect'.  If it is `BibTeX', the return value is
simply TARGET-FIELD; otherwise the inheritances are taken from
the variable `ebib-biblatex-inheritances'.  If TARGET-FIELD cannot
inherit a value, this function returns nil."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (if (eq dialect 'BibTeX)
      target-field
    (let* ((inheritance (append (cl-third (cl-find-if (lambda (e)
                                                        (and (string-match-p (concat "\\b" source-entry "\\b") (cl-first e))
                                                             (string-match-p (concat "\\b" target-entry "\\b") (cl-second e))))
                                                      ebib-biblatex-inheritances))
                                (cl-third (assoc-string "all" ebib-biblatex-inheritances 'case-fold)))))
      (or (car (rassoc (downcase target-field) inheritance))
          target-field))))

;; ebib-set-string is just a front-end for `ebib-db-set-string'.  The only additional
;; functionality is that it wraps the string value in braces if it isn't
;; already.  We need to do this because the brace functions are in this file and
;; `ebib-db.el' cannot depend on `ebib-utils.el' (because `ebib-utils.el' already depends on
;; `ebib-db.el').  Similar considerations apply to `ebib-get-string' below.

(defun ebib-set-string (abbr value db &optional overwrite nobrace)
  "Set the @string definition ABBR to VALUE in database DB.
If ABBR does not exist, create it.  OVERWRITE functions as in
`ebib-db-set-string'.  VALUE is enclosed in braces if it isn't
already.

If NOBRACE is t, the value is stored without braces.  If it is
nil, braces are added if not already present.  NOBRACE may also be
the symbol `as-is', in which case the value is stored as is.

This function basically just calls `ebib-db-set-string' to do the
  real work."
  (ebib-db-set-string abbr (cl-case nobrace
			     (as-is value)
			     ((nil) (ebib-brace value))
			     (t (ebib-unbrace value)))
                      db overwrite))

(defun ebib--expand-string (string db &optional noerror)
  "Recursively expand STRING using abbreviations in DB.

This function respects concatenation with `#', and quoting with
{} and \".  If STRING contains unbalanced (and unescaped) braces
or quote characters, an error is thrown, unless NOERROR is
non-nil, in which case all errors are suppressed and the return
value is simply STRING.

If passed an entirely braced or quoted string (i.e. a string for
which `ebib-unbraced-p' is nil), the return value is the string
without the outermost set of braces/quotes.

If any expansion occurs (i.e. an abbrev string is expanded to its
definition, or any concatenation around `#' characters is done),
the returned string has the face `ebib-abbrev-face'."
  (condition-case err
      (cl-loop with quoted = nil and curr-section = "" and expanded = nil
	       ;; To be able detect its end, make sure that the string
	       ;; ends in '#'. Even if it already does, null strings are
	       ;; removed from the result, so this fact won't change the
	       ;; output.
	       for char across (concat string "#")
	       ;; When we encounter an unquoted concat char...
	       if (and (not quoted) (eq char ?#))
	       ;; ...if the current string is non-empty (this deals with
	       ;; repeated '#' characters)...
	       unless (string-empty-p (string-trim curr-section))
	       ;; ...then add it to the list of strings.
	       collect (let ((curr-string (string-trim curr-section)))
		         (if (ebib-unbraced-p curr-string)
			     ;; Recur if string not quoted
			     (progn (setq expanded t)
				    (ebib-get-string curr-string db nil nil t))
		           (unless (null list) (setq expanded t))
		           (ebib-unbrace curr-string)))
	       into list ;; collect unquoted/expanded strings into var `list'
	       else      ;; String contains only whitespace, or is empty
	       do (error "[Ebib] Too many `#' characters")
	       end ;; end the 'unless'
	       and
	       ;; Either way, then reinitialise the string var as empty.
	       do (setq curr-section "")
	       else
	       ;; Control delimiter/quoting characters
	       ;; The `quoted' variable is a running list of opening quote
	       ;; characters. Each new opening character is added to the
	       ;; end. On encountering a closing character, if the last
	       ;; item in the list balances it, remove the last item. This
	       ;; way, `quoted' is nil whenever we are outside all quotes.
	       do (unless (equal (last (string-to-list curr-section)) '(?\\))
		    (cl-case char
		      (?\" (if (eq (car (last quoted)) ?\")
			       (setq quoted (butlast quoted))
			     (setq quoted (append quoted '(?\")))))
		      (?} (if (eq (car (last quoted)) ?{)
			      (setq quoted (butlast quoted))
			    (unless (member ?\" quoted) ;; account for e.g. " { "
			      (error "[Ebib] Bad quoting, last char of: `%s%s%c'"
				     (apply 'concat list)
				     curr-section
				     char))))
		      (?{ (unless (member ?\" quoted) ;; account for e.g. " { "
			    (setq quoted (append quoted '(?{)))))))
	       and
	       ;; If not at a '#' or a delimiter char, add current char to
	       ;; the string
	       do (setq curr-section (concat curr-section (char-to-string char)))
	       end
	       ;; If a proper expansion has been performed, propertize the result accordingly
	       finally return (if (null quoted)
			          (let ((expansion (apply 'concat list)))
				    (if expanded
				        (propertize expansion 'face 'ebib-abbrev-face)
				      expansion))
			        (error "[Ebib] Bad quoting, unbalanced `\"' characters")))
    (t (if noerror
           string
         (signal (car err) (cdr err))))))

(defun ebib-get-string (abbr db &optional noerror unbraced expand)
  "Return the value of @String definition ABBR in database DB.
NOERROR functions as in `ebib-db-get-string', which this
functions calls to get the actual value.  The braces around the
value are removed if UNBRACED is non-nil.

If EXPAND is non-nil, unbraced elements of a string's
definition (i.e. other, already-defined string abbreviations)
will be expanded recursively."
  (let* ((def (ebib-db-get-string abbr db noerror))
	 (value (if expand (ebib--expand-string def db noerror) def)))
    (if unbraced
        (ebib-unbrace value)
      value)))

;; `ebib-unbraced-p' determines if STRING is enclosed in braces.  Note that we
;; cannot do this by simply checking whether STRING begins with { and ends with
;; } (or begins and ends with "), because something like "{abc} # D # {efg}"
;; would then be incorrectly recognised as braced.  So we need to do the
;; following: take out everything that is between braces or quotes, and see if
;; anything is left.  If there is, the original string was braced, otherwise it
;; was not.

;; So we first check whether the string begins with { or ".  If not, we
;; certainly have an unbraced string.  (`ebib-unbraced-p' recognises this
;; through the default clause of the `cond'.)  If the first character is { or ",
;; we first take out every occurrence of backslash-escaped { and } or ", so that
;; the rest of the function does not get confused over them.

;; Then, if the first character is {, `remove-from-string' takes out every
;; occurrence of the regex "{[^{]*?}", which translates to "the smallest string
;; that starts with { and ends with }, and does not contain another {.  IOW, it
;; takes out the innermost braces and their contents.  Because braces may be
;; embedded, we have to repeat this step until no more balanced braces are found
;; in the string.  (Note that it would be unwise to check for just the occurrence
;; of { or }, because that would throw `ebib-unbraced-p' in an infinite loop if
;; a string contains an unbalanced brace.)

;; For strings beginning with " we do the same, except that it is not
;; necessary to repeat this in a `while' loop, for the simple reason that
;; strings surrounded with double quotes cannot be embedded; i.e.,
;; "ab"cd"ef" is not a valid (BibTeX) string, while {ab{cd}ef} is.

;; Note: because these strings are to be fed to BibTeX and ultimately
;; (La)TeX, it might seem that we don't need to worry about strings
;; containing unbalanced braces, because (La)TeX would choke on them.  But
;; the user may inadvertently enter such a string, and we therefore need to
;; be able to handle it.  (Alternatively, we could perform a check on
;; strings and warn the user.)

(defun ebib-unbraced-p (string)
  "Non-nil if STRING is not enclosed in braces or quotes."
  (save-match-data
   (cl-flet ((remove-from-string (string remove)
                                (apply #'concat (split-string string remove))))
    (when (stringp string)
      (cond
       ((eq (string-to-char string) ?\{)
        ;; First, remove all escaped { and } from the string:
        (setq string (remove-from-string (remove-from-string string "[\\][{]")
                                         "[\\][}]"))
        ;; Then remove the innermost braces with their contents and continue until
        ;; no more braces are left.
        (while (and (member ?\{ (string-to-list string)) (member ?\} (string-to-list string)))
          (setq string (remove-from-string string "{[^{]*?}")))
        ;; If STRING is not empty, the original string contains material not in braces.
        (> (length string) 0))
       ((eq (string-to-char string) ?\")
        ;; Remove escaped ", then remove any occurrences of balanced quotes with
        ;; their contents and check for the length of the remaining string.
        (> (length (remove-from-string (remove-from-string string "[\\][\"]")
                                       "\"[^\"]*?\""))
           0))
       (t t))))))

(defun ebib-unbrace (string)
  "Convert STRING to its unbraced counterpart.
If STRING is already unbraced, do nothing."
  (if (and (stringp string)
           (not (ebib-unbraced-p string)))
      (substring string 1 -1)
    string))

(defun ebib-brace (string)
  "Put braces around STRING.
If STRING is already braced, do nothing."
  (if (ebib-unbraced-p string)
      (concat "{" string "}")
    string))

(defun ebib--real-database-p (db)
  "Return non-nil if DB is a real database.
Return nil if DB is either a filtered or a dependent database."
  (not (or (ebib-db-filtered-p db)
           (ebib-db-dependent-p db))))

(defun ebib--list-dependents (database)
  "Return a list of dependent databases for DATABASE."
  (seq-filter (lambda (db)
                (eq (ebib-db-get-main db) database))
              ebib--databases))

(defun ebib--list-fields (entry-type type &optional dialect)
  "List the fields of ENTRY-TYPE.
TYPE specifies which fields to list.  It is a symbol and can be
one of the following: `required' means to list only required
fields; `optional' means to list optional fields; `extra' means
to list extra fields (i.e., fields defined in `ebib--extra-fields'
and not present in ENTRY-TYPE); finally, `all' means to list all
fields.  DIALECT is the BibTeX dialect; possible values are those
listed in `bibtex-dialect-list' or nil, in which case the value
of `ebib-bibtex-dialect' is used.

If DIALECT is `biblatex' and ENTRY-TYPE is a type alias as
defined by Biblatex, return the fields of the entry type for
which ENTRY-TYPE is an alias."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (if (eq dialect 'biblatex)
      (setq entry-type (or (cdr (assoc-string entry-type ebib--type-aliases 'case-fold))
                           entry-type)))
  (let (required optional extra)
    (when (memq type '(required extra all))
      (setq required (mapcar #'car (append (nth 2 (assoc-string entry-type (bibtex-entry-alist dialect) 'case-fold))
                                           (nth 3 (assoc-string entry-type (bibtex-entry-alist dialect) 'case-fold))))))
    (when (memq type '(optional extra all))
      (setq optional (mapcar #'car (nth 4 (assoc-string entry-type (bibtex-entry-alist dialect) 'case-fold)))))
    (when (and (memq type '(all extra))
	       (not (member-ignore-case entry-type '("xdata" "set"))))
      (setq extra (seq-remove (lambda (elt)
                                (member-ignore-case elt (append required optional)))
                              (cdr (assq dialect ebib-extra-fields)))))
    (cond
     ((eq type 'required) required)
     ((eq type 'optional) optional)
     ((eq type 'extra) extra)
     ((eq type 'all) (append required optional extra)))))

(defun ebib--list-undefined-fields (entry dialect)
  "Return an alist of fields of ENTRY that are not predefined.
ENTRY is an alist representing a BibTeX entry.  The return value
is an alist of (field . value) pairs of those fields that are not
part of the definition of ENTRY's type and also not part of the
extra fields.

DIALECT is the BibTeX dialect; possible values are those listed
in `bibtex-dialect-list' or nil, in which case the value of
`ebib-bibtex-dialect' is used."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (let ((fields (ebib--list-fields (cdr (assoc "=type=" entry)) 'all dialect)))
    (seq-remove (lambda (elt)
                  (or (member-ignore-case (car elt) fields)
                      (null (cdr elt))))
                entry)))

(defun ebib--list-entry-types (&optional dialect include-aliases)
  "Return a list of entry types.
This list depends on the value of DIALECT, which can have the
values in `bibtex-dialect-list' or nil, in which case the value
of `ebib-bibtex-dialect' is used.  If INCLUDE-ALIASES is non-nil,
include entry type aliases as defined by `ebib--type-aliases'."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (append (mapcar #'car (bibtex-entry-alist dialect))
          (if (and include-aliases (eq dialect 'biblatex))
              (mapcar #'car ebib--type-aliases))))

(defvar ebib--unique-field-alist nil
  "Alist of BibTeX dialects and their fields.
This variable is initialized by `ebib--list-field-uniquely'.")

(defun ebib--list-fields-uniquely (dialect)
  "Return a list of all fields of BibTeX DIALECT.
Possible values for DIALECT are those listed in
`bibtex-dialect-list' or nil, in which case the value of
`ebib-bibtex-dialect' is used."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (or (cdr (assq dialect ebib--unique-field-alist))
      (let (fields)
        (mapc (lambda (entry)
                (setq fields (seq-uniq (seq-concatenate 'list fields (ebib--list-fields (car entry) 'all dialect)))))
              (bibtex-entry-alist dialect))
        (push (cons dialect fields) ebib--unique-field-alist)
        fields)))

(defun ebib--get-field-value-for-display (field key db &rest properties)
  "Return the value of FIELD in entry KEY in DB for display.
This function returns a value for FIELD in such a way that it can
be used to display to the user.  If FIELD is found in
`ebib-field-transformation-functions', return the field content
transformed by the associated function.

Otherwise, If FIELD is \"Entry Key\", KEY is returned; if FIELD
is \"Author/Editor\", the contents of the Author field is
returned or, if the Author field is empty, the contents of the
Editor field; if the Editor field is empty as well, return the
string \"(No Author/Editor)\".  If FIELD is \"Year\", the year of
the first date in the Date field is returned (i.e., date ranges
are ignored), or, if no year can be found in the Date field, the
contents of the Year field is returned.  If this field is empty,
the string \"XXXX\" is returned.  These defaults can be
overridden by adding an appropriate transformation function to
`ebib-field-transformation-functions'.

If FIELD is not found in `ebib-field-transformation-functions'
and is not one of the special fields listed above, the field
value is returned.  If a field value is empty, the return value
is the string \"(No <FIELD>)\".

PROPERTIES are text properties that are added to the resulting
string using `propertize'."
  (let ((result (cond
                 ((assoc-string field ebib-field-transformation-functions 'case-fold)
                  (funcall (cdr (assoc field ebib-field-transformation-functions)) field key db))
                 ((cl-equalp field "Entry Key")
                  key)
                 ((cl-equalp field "Author/Editor")
                  (or (ebib-get-field-value "Author" key db 'noerror 'unbraced 'xref)
                      (ebib-get-field-value "Editor" key db "(No Author/Editor)" 'unbraced 'xref)))
                 ((cl-equalp field "Year")
                  (ebib-get-year-for-display key db))
                 (t (ebib-get-field-value field key db (format "(No %s)" (capitalize field)) 'unbraced 'xref)))))
    (if properties
        (apply #'propertize result properties)
      result)))

(defun ebib-get-year-for-display (key db)
  "Return the year for entry KEY in DB.
In BibTeX, this simply returns the value of the Year field, or
the string \"XXXX\" if this is empty.  For BibLaTeX, this first
checks the Date field and returns the year from the first date in
this field (meaning date ranges are ignored).  If no year can be
found in the Date field, the value of the Year field is returned,
or \"XXXX\" if it's empty."
  (save-match-data
    (let ((date (ebib-get-field-value "Date" key db 'noerror 'unbraced 'xref)))
      (or (and date (ebib-biblatex-date-to-year date))
          (ebib-get-field-value "Year" key db "XXXX" 'unbraced 'xref)))))

(defun ebib-biblatex-date-to-year (date)
  "Extract the year from DATE.
DATE is a string conforming to a biblatex date specification.  If
it is a single date, the year is returned.  If DATE is a date
range, the year of the start of the range is returned.  If no
year can be extracted from DATE, return nil."
  (save-match-data
    (if (string-match "^[[:space:]]*\\(?:\\.\\.\\)?/?\\(-?[0-9]\\{4\\}\\)" date)
        (match-string 1 date))))

(defun ebib-clean-TeX-markup-from-entry (field key db)
  "Return the contents of FIELD from KEY in DB without TeX markup."
  (parsebib-clean-TeX-markup (ebib-get-field-value field key db "" 'unbraced 'xref)))

(defun ebib-abbreviate-journal-title (field key db)
  "Abbreviate the content of FIELD from KEY in database DB.
This function is intended to abbreviate journal titles.  Short
function words (specifically \"a, an, at, of, on, and\" and
\"for\") are removed, the initial letters of the remaining words
are returned as a string."
  (let ((str (ebib-get-field-value field key db "" 'unbraced 'xref)))
    (if (string-match-p "\s+" str)
        (apply 'concat
               (mapcar (lambda (str) (substring str 0 1))
                       (seq-difference
                        (mapcar 'capitalize (split-string str "\\Sw+" t))
                        '("A" "An" "At" "Of" "On" "And" "For"))))
      str)))

(defun ebib-display-www-link (field key db)
  "Return the content of FIELD from KEY in DB as a link.
This function is mainly intended for the DOI and URL fields."
  (if-let ((val (ebib-get-field-value field key db 'noerror 'unbraced 'xref))
	   ;; Unless field is doi, assume the string is a full url
	   (str (if (and (string= (downcase field) "doi")
		         (string-match-p "^[0-9]" val))
		    (concat "https://dx.doi.org/" val)
		  val)))
      (propertize "www"
		  'face 'button
		  'font-lock-face 'button
		  'mouse-face 'highlight
		  'help-echo str
		  'button t
		  'follow-link t
		  'category t
		  'button-data str
		  'keymap button-map
		  'action 'ebib--call-browser)
    (propertize "   " 'face '(:height 0.8))))

(defun ebib--sort-keys-list (keys db)
  "Sort KEYS according to the sort info of DB.
First, the keys are sorted themselves, then the list is stably
sorted on the sort info of DB.  Thus if two entries have the same
value for the sort field, their keys determine the order in which
they appear.

Sorting on the sort field is done with the sort function
specified in `ebib-field-sort-functions-alist', with
`string-collate-lessp' as a fall-back."
  ;; First sort the keys themselves.
  (setq keys (sort keys #'string<))
  ;; And then stably sort on the sort field.  Note that we do not check here if
  ;; the field can be sorted on according to `ebib-index-columns'. This is a
  ;; generic sort function.
  (let* ((sortinfo (or (ebib-db-get-sortinfo db)
                       ebib-index-default-sort
                       (cons (caar ebib-index-columns) 'ascend)))
         (field (car sortinfo))
         (direction (cdr sortinfo))
         ;; We use a temp list for sorting, so that the :key argument to
         ;; `cl-stable-sort' can simply be `car' rather than (a much
         ;; heavier) `ebib-get-field-value'. Sorting is much faster
         ;; that way.
         (sort-list (mapcar (lambda (key)
                              (cons (ebib--get-field-value-for-display field key db) key))
                            keys))
	 (predicate (alist-get field ebib-field-sort-functions-alist
			       #'string-collate-lessp
                               nil
			       #'cl-equalp)))
    (setq sort-list (cl-stable-sort sort-list predicate :key #'car))
    (setq keys (mapcar #'cdr sort-list))
    ;; Reverse the list if necessary.
    (if (eq direction 'descend)
        (setq keys (nreverse keys))))
  ;; Now return the list of keys.
  keys)

(provide 'ebib-utils)

;;; ebib-utils.el ends here
