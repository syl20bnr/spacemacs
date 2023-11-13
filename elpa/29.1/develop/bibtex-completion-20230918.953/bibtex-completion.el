;;; bibtex-completion.el --- A BibTeX backend for completion frameworks

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;;         Justin Burkett <justin@burkett.cc>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: https://github.com/tmalsburg/helm-bibtex
;; Version: 1.0.0
;; Package-Requires: ((parsebib "1.0") (s "1.9.0") (dash "2.6.0") (f "0.16.2") (cl-lib "0.5") (biblio "0.2") (emacs "26.1"))

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

;; A BibTeX backend for completion frameworks

;; There are currently two fronends: helm-bibtex and ivy-bibtex.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/helm-bibtex

;;; Code:

(require 'browse-url)
(require 'parsebib)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'biblio)
(require 'filenotify)
(require 'org-capture)

;; Silence byte-compiler
(declare-function reftex-what-macro "reftex-parse")
(declare-function reftex-get-bibfile-list "reftex-cite")
(declare-function outline-show-all "outline")
(declare-function org-narrow-to-subtree "org")
(declare-function org-cycle-hide-drawers "org")
(declare-function org-find-property "org")
(declare-function org-show-entry "org")
(declare-function org-entry-get "org")
(declare-function org-element-parse-buffer "org-element")
(declare-function org-element-map "org-element")
(declare-function org-element-property "org-element")

(defgroup bibtex-completion nil
  "Helm plugin for searching entries in a BibTeX bibliography."
  :group 'completion)

(defcustom bibtex-completion-bibliography nil
  "The BibTeX file or list of BibTeX files.
Org-bibtex users can also specify org mode bibliography files, in
which case it will be assumed that a BibTeX file exists with the
same name and extension bib instead of org.  If the bib file has a
different name, use a cons cell `(\"orgfile.org\" . \"bibfile.bib\")' instead."
  :group 'bibtex-completion
  :type '(choice file (repeat file)))

;;;###autoload (put 'bibtex-completion-bibliography 'safe-local-variable 'stringp)

(defcustom bibtex-completion-library-path nil
  "A directory or list of directories in which PDFs are stored.
Bibtex-completion assumes that the names of these PDFs are
composed of the BibTeX-key plus a \".pdf\" suffix."
  :group 'bibtex-completion
  :type '(choice directory (repeat directory)))

(defcustom bibtex-completion-pdf-open-function 'find-file
  "The function used for opening PDF files.
This can be an arbitrary function that takes one argument: the
path to the PDF file.  The default is `find-file' which opens the
PDF in Emacs (either with docview or, if installed, the much
superior pdf-tools.  When set to
`helm-open-file-with-default-tool', the systems default viewer
for PDFs is used."
  :group 'bibtex-completion
  :type 'function)

(defcustom bibtex-completion-pdf-extension ".pdf"
  "The extension of a BibTeX entry's \"PDF\" file.
This makes it possible to use another file type.  It can also be a
list of file types, which are then tried sequentially until a
file is found.  Beware that adding file types can reduce
performance for large bibliographies.  This variable has no
effect if PDFs are referenced via the file field."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-find-additional-pdfs nil
  "If non-nil, all files whose base name starts with the BibTeX key and ends with `bibtex-completion-pdf-extension' are considered as PDFs, not only \"<key>.<extension>\".
Note that for performance reasons, an entry is only marked as
having a PDF if \"<key>.<extension\" exists."
  :group 'bibtex-completion
  :type 'boolean)

(defcustom bibtex-completion-pdf-symbol "⌘"
  "Symbol used to indicate that a PDF file is available for a publication.
This should be a single character."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-ebib)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (python-mode   . bibtex-completion-format-citation-sphinxcontrib-bibtex)
    (rst-mode      . bibtex-completion-format-citation-sphinxcontrib-bibtex)
    (default       . bibtex-completion-format-citation-default))
  "The functions used for formatting citations.
The publication can be cited, for example, as \cite{key} or
ebib:key depending on the major mode of the current buffer.  Note
that the functions should accept a list of keys as input.  With
multiple marked entries one can insert multiple keys at once,
e.g. \cite{key1,key2}.  See the functions
`bibtex-completion-format-citation-org-cite' and
`bibtex-completion-format-citation-cite' as examples."
  :group 'bibtex-completion
  :type '(alist :key-type symbol :value-type function))

(defcustom bibtex-completion-notes-path nil
  "The place where notes are stored.
This is either a file, in which case all notes are stored in that
file, or a directory, in which case each publication gets its own
notes file in that directory.  In the latter case,
bibtex-completion assumes that the names of the note files are
composed of the BibTeX-key plus a suffix that is specified in
`bibtex-completion-notes-extension'."
  :group 'bibtex-completion
  :type '(choice file directory (const nil)))

(defcustom bibtex-completion-notes-template-multiple-files
  "#+TITLE: Notes on: ${author-or-editor} (${year}): ${title}\n\n"
  "Template used to create a new note when each note is stored in a separate file.
'${field-name}' can be used to insert the value of a BibTeX field
into the template.  Apart from the fields defined in the entry,
one can also use the virtual fields `author-or-editor` -- which
contains the author names if defined and otherwise the names of
the editors -- and `author-abbrev` -- which abbreviates to 'First
author et al.' when there are three or more authors."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-template-one-file
  "\n* ${author-or-editor} (${year}): ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :END:\n\n"
  "Template used to create a new note when all notes are stored in one file.
'${field-name}' can be used to insert the value of a BibTeX field
into the template.  Apart from the fields defined in the entry,
one can also use the virtual field `author-or-editor` -- which
contains the author names if defined and otherwise the names of
the editors -- and `author-abbrev` -- which abbreviates to 'First
author et al.' when there are three or more authors."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-key-pattern
  ":Custom_ID: +%s\\( \\|$\\)"
  "The pattern used to find entries in the notes file.
Only relevant when all notes are stored in one file.  The key can
be inserted into the pattern using the `format` function."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-extension ".org"
  "The extension of the files containing notes.
This is only used when `bibtex-completion-notes-path' is a
directory (not a file)."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-symbol "✎"
  "Symbol used to indicate that a publication has notes.
This should be a single character."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-fallback-options
  '(("CrossRef                                  (biblio.el)"
     . (lambda (search-expression) (biblio-lookup #'biblio-crossref-backend search-expression)))
    ("arXiv                                     (biblio.el)"
     . (lambda (search-expression) (biblio-lookup #'biblio-arxiv-backend search-expression)))
    ("DBLP (computer science bibliography)      (biblio.el)"
     . (lambda (search-expression) (biblio--lookup-1 #'biblio-dblp-backend search-expression)))
    ("HAL (French open archive)                 (biblio.el)"
     . (lambda (search-expression) (biblio--lookup-1 #'biblio-hal-backend search-expression)))
    ("IEEE                                      (biblio.el)"
     . (lambda (search-expression) (biblio--lookup-1 #'biblio-ieee-backend search-expression)))
    ("Google Scholar                            (web)"
     . "https://scholar.google.co.uk/scholar?q=%s")
    ("Pubmed                                    (web)"
     . "https://www.ncbi.nlm.nih.gov/pubmed/?term=%s")
    ("Bodleian Library                          (web)"
     . "http://solo.bodleian.ox.ac.uk/primo_library/libweb/action/search.do?vl(freeText0)=%s&fn=search&tab=all")
    ("Library of Congress                       (web)"
     . "https://www.loc.gov/search/?q=%s&all=true&st=list")
    ("Deutsche Nationalbibliothek               (web)"
     . "https://portal.dnb.de/opac.htm?query=%s")
    ("British National Library                  (web)"
     . "http://explore.bl.uk/primo_library/libweb/action/search.do?&vl(freeText0)=%s&fn=search")
    ("Bibliothèque nationale de France          (web)"
     . "http://catalogue.bnf.fr/servlet/RechercheEquation?host=catalogue?historique1=Recherche+par+mots+de+la+notice&niveau1=1&url1=/jsp/recherchemots_simple.jsp?host=catalogue&maxNiveau=1&categorieRecherche=RechercheMotsSimple&NomPageJSP=/jsp/recherchemots_simple.jsp?host=catalogue&RechercheMotsSimpleAsauvegarder=0&ecranRechercheMot=/jsp/recherchemots_simple.jsp&resultatsParPage=20&x=40&y=22&nbElementsHDJ=6&nbElementsRDJ=7&nbElementsRCL=12&FondsNumerise=M&CollectionHautdejardin=TVXZROM&HDJ_DAV=R&HDJ_D2=V&HDJ_D1=T&HDJ_D3=X&HDJ_D4=Z&HDJ_SRB=O&CollectionRezdejardin=UWY1SPQM&RDJ_DAV=S&RDJ_D2=W&RDJ_D1=U&RDJ_D3=Y&RDJ_D4=1&RDJ_SRB=P&RDJ_RLR=Q&RICHELIEU_AUTRE=ABCDEEGIKLJ&RCL_D1=A&RCL_D2=K&RCL_D3=D&RCL_D4=E&RCL_D5=E&RCL_D6=C&RCL_D7=B&RCL_D8=J&RCL_D9=G&RCL_D10=I&RCL_D11=L&ARSENAL=H&LivrePeriodique=IP&partitions=C&images_fixes=F&son=S&images_animees=N&Disquette_cederoms=E&multimedia=M&cartes_plans=D&manuscrits=BT&monnaies_medailles_objets=JO&salle_spectacle=V&Monographie_TN=M&Periodique_TN=S&Recueil_TN=R&CollectionEditorial_TN=C&Ensemble_TN=E&Spectacle_TN=A&NoticeB=%s")
    ("Gallica Bibliothèque Numérique            (web)"
     . "http://gallica.bnf.fr/Search?q=%s"))
  "Alist of online sources that can be used to search for publications.
The key of each entry is the name of the online source.  The
value is the URL used for retrieving results.  This URL must
contain a %s in the position where the search term should be
inserted.  Alternatively, the value can be a function that will
be called when the entry is selected."
  :group 'bibtex-completion
  :type '(alist :key-type string
                :value-type (choice (string :tag "URL")
                            (function :tag "Function"))))

(defcustom bibtex-completion-browser-function nil
  "The browser that is used to access online resources.
If nil (default), the value of `browse-url-browser-function' is
used.  If that value is nil, Helm uses the first available
browser in `helm-browse-url-default-browser-alist'"
  :group 'bibtex-completion
  :type '(choice
          (const         :tag "Default" :value nil)
          (function-item :tag "Emacs interface to w3m" :value w3m-browse-url)
          (function-item :tag "Emacs W3" :value  browse-url-w3)
          (function-item :tag "W3 in another Emacs via `gnudoit'"
                         :value  browse-url-w3-gnudoit)
          (function-item :tag "Mozilla" :value  browse-url-mozilla)
          (function-item :tag "Firefox" :value browse-url-firefox)
          (function-item :tag "Chromium" :value browse-url-chromium)
          (function-item :tag "Galeon" :value  browse-url-galeon)
          (function-item :tag "Epiphany" :value  browse-url-epiphany)
          (function-item :tag "Netscape" :value  browse-url-netscape)
          (function-item :tag "eww" :value  eww-browse-url)
          (function-item :tag "Mosaic" :value  browse-url-mosaic)
          (function-item :tag "Mosaic using CCI" :value  browse-url-cci)
          (function-item :tag "Text browser in an xterm window"
                         :value browse-url-text-xterm)
          (function-item :tag "Text browser in an Emacs window"
                         :value browse-url-text-emacs)
          (function-item :tag "KDE" :value browse-url-kde)
          (function-item :tag "Elinks" :value browse-url-elinks)
          (function-item :tag "Specified by `Browse Url Generic Program'"
                         :value browse-url-generic)
          (function-item :tag "Default Windows browser"
                         :value browse-url-default-windows-browser)
          (function-item :tag "Default Mac OS X browser"
                         :value browse-url-default-macosx-browser)
          (function-item :tag "GNOME invoking Mozilla"
                         :value browse-url-gnome-moz)
          (function-item :tag "Default browser"
                         :value browse-url-default-browser)
          (function      :tag "Your own function")
          (alist         :tag "Regexp/function association list"
                         :key-type regexp :value-type function)))

(defcustom bibtex-completion-additional-search-fields nil
  "The fields that are used for searching in addition to author, editor, title, year, BibTeX key, and entry type."
  :group 'bibtex-completion
  :type '(repeat string))

(defcustom bibtex-completion-no-export-fields nil
  "A list of fields that should be ignored when exporting BibTeX entries."
  :group 'bibtex-completion
  :type '(repeat string))

(defcustom bibtex-completion-cite-commands '("cite" "Cite" "parencite"
"Parencite" "footcite" "footcitetext" "textcite" "Textcite"
"smartcite" "Smartcite" "cite*" "parencite*" "supercite" "autocite"
"Autocite" "autocite*" "Autocite*" "citeauthor" "Citeauthor"
"citeauthor*" "Citeauthor*" "citetitle" "citetitle*" "citeyear"
"citeyear*" "citedate" "citedate*" "citeurl" "nocite" "fullcite"
"footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite"
"fnotecite")
  "The list of LaTeX cite commands.
When creating LaTeX citations, these can be accessed as future
entries in the minibuffer history, i.e. by pressing the arrow
down key.  The default entries are taken from biblatex.  There is
currently no special support for multicite commands and volcite
et al.  These commands can be used but bibtex-completion does not
prompt for their extra arguments."
  :group 'bibtex-completion
  :type '(choice string (repeat string)))

(defcustom bibtex-completion-cite-default-command "cite"
  "The LaTeX cite command that is used if the user doesn't enter anything when prompted for such a command."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-cite-prompt-for-optional-arguments t
  "If t, bibtex-completion prompts for pre- and postnotes for LaTeX cite commands.
Choose `nil' for no prompts."
  :group 'bibtex-completion
  :type 'boolean)

(defcustom bibtex-completion-cite-default-as-initial-input nil
  "This variable controls how the default command defined in `bibtex-completion-cite-default-command' is used.
If t, it is inserted into the minibuffer before reading input
from the user.  If nil, it is used as the default if the user
doesn't enter anything."
  :group 'bibtex-completion
  :type 'boolean)

(defcustom bibtex-completion-pdf-field nil
  "The name of the BibTeX field in which the path to PDF files is stored or `nil' if no such field should be used.
If an entry has no value for this field, or if the specified file
does not exist, or if this variable is nil, bibtex-completion
will look up the PDF in the directories listed in
`bibtex-completion-library-path'."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-display-formats
  '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}"))
  "Alist of format strings for displaying entries in the results list.
The key of each element of this list is either a BibTeX entry
type (in which case the format string applies to entries of this
type only) or t (in which case the format string applies to all
other entry types).  The value is the format string.

In the format string, expressions like \"${author:36}\",
\"${title:*}\", etc, are expanded to the value of the
corresponding field.  An expression like \"${author:N}\" is
truncated to a width of N characters, whereas an expression like
\"${title:*}\" is truncated to the remaining width in the results
window.  Three special fields are available: \"=type=\" holds the
BibTeX entry type, \"=has-pdf=\" holds
`bibtex-completion-pdf-symbol' if the entry has a PDF file, and
\"=has-notes=\" holds `bibtex-completion-notes-symbol' if the
entry has a notes file.  The \"author\" field is expanded to
either the author names or, if the entry has no author field, the
editor names."
  :group 'bibtex-completion
  :type '(alist :key-type symbol :value-type string))

(defvar bibtex-completion-cross-referenced-entry-types
  '("proceedings" "mvproceedings" "book" "mvbook" "collection" "mvcollection")
  "The list of potentially cross-referenced entry types (in lowercase).
Only entries of these types are checked in order to resolve
cross-references.  The default list is usually sufficient; adding
more types can slow down resolution for large biblioraphies.")

(defvar bibtex-completion-display-formats-internal nil
  "Stores `bibtex-completion-display-formats' together with the \"used width\" of each format string.
This is set internally.")

(defvar bibtex-completion-cache nil
  "A cache storing the hash of the bibliography content and the corresponding list of entries, for each bibliography file, obtained when the bibliography was last parsed.
When the current bibliography hash is identical to the cached
hash, the cached list of candidates is reused, otherwise the
bibliography file is reparsed.")

(defvar bibtex-completion-string-cache nil
  "A cache storing bibtex strings, for each bibliography file, obtained when the bibliography was last parsed.")

(defvar bibtex-completion-string-hash-table nil
  "A hash table used for string replacements.")

(defun bibtex-completion-normalize-bibliography (&optional type)
  "Return a list of bibliography file(s) in `bibtex-completion-bibliography'.
If there are org mode bibliography-files, their corresponding
bibtex files are listed as well, unless TYPE is 'main.  If TYPE is
'bibtex, org mode bibliography-files are instead replaced with
their associated bibtex files."
  (delete-dups
   (cl-loop
    for bib-file in (-flatten (list bibtex-completion-bibliography))
    for main-file = (if (consp bib-file)
                        (car bib-file)
                      bib-file)
    for bibtex-file = (if (consp bib-file)
                          (cdr bib-file)
                        (cond
                         ((string= (file-name-extension main-file) "bib") main-file)
                         ((string= (file-name-extension main-file) "org")
                          (concat (file-name-sans-extension main-file) "bib"))
                         ((and (string= (file-name-extension main-file) "gpg")
                               (string= (file-name-extension
                                         (file-name-sans-extension main-file)) "bib")) main-file)
                         ((and (string= (file-name-extension main-file) "gpg")
                               (string= (file-name-extension
                                         (file-name-sans-extension main-file)) "org"))
                          (concat (file-name-sans-extension
                                   (file-name-sans-extension main-file)) ".bib.gpg"))))
    unless (equal type 'bibtex)
    collect main-file
    unless (equal type 'main)
    collect bibtex-file)))

(defvar bibtex-completion-file-watch-descriptors nil
  "List of file watches monitoring bibliography files for changes.")

(defun bibtex-completion-init ()
  "Check that the files and directories specified by the user actually exist.
Also sets `bibtex-completion-display-formats-internal'."

  ;; Remove current watch-descriptors for bibliography files:
  (mapc (lambda (watch-descriptor)
          (file-notify-rm-watch watch-descriptor))
        bibtex-completion-file-watch-descriptors)
  (setq bibtex-completion-file-watch-descriptors nil)

  ;; Check that all specified bibliography files exist and add file
  ;; watches for automatic reloading of the bibliography when a file
  ;; is changed:
  (mapc (lambda (file)
          (if (f-file? file)
              (let ((watch-descriptor
                     (file-notify-add-watch file
                                            '(change)
                                            (lambda (event) (bibtex-completion-candidates)))))
                (setq bibtex-completion-file-watch-descriptors
                      (cons watch-descriptor bibtex-completion-file-watch-descriptors)))
            (user-error "Bibliography file %s could not be found" file)))
            (bibtex-completion-normalize-bibliography))

  ;; Pre-calculate minimal widths needed by the format strings for
  ;; various entry types:
  (setq bibtex-completion-display-formats-internal
        (mapcar (lambda (format)
                  (let* ((format-string (cdr format))
                         (fields-width 0)
                         (string-width
                          (string-width
                           (s-format format-string
                                     (lambda (field)
                                       (setq fields-width
                                             (+ fields-width
                                                (string-to-number
                                                 (or (cadr (split-string field ":"))
                                                     ""))))
                                       "")))))
                    (-cons* (car format) format-string (+ fields-width string-width))))
                bibtex-completion-display-formats)))

(defun bibtex-completion-clear-cache (&optional files)
  "Clears FILES from cache.
If FILES is omitted, all files in `bibtex-completion-biblography'
are cleared."
  (setq bibtex-completion-cache
        (cl-remove-if
         (lambda (x)
           (member (car x)
                   (or files
                       (bibtex-completion-normalize-bibliography 'bibtex))))
         bibtex-completion-cache)))

(defun bibtex-completion-clear-string-cache (&optional files)
  "Clears FILES from cache.
If FILES is omitted, all files in
`bibtex-completion-bibliography' are cleared."
  (setq bibtex-completion-string-cache
        (cl-remove-if
         (lambda (x)
           (member (car x)
                   (or files
                       (-flatten (list bibtex-completion-bibliography)))))
         bibtex-completion-string-cache)))

(defun bibtex-completion-parse-strings (&optional ht-strings)
  "Parse the BibTeX strings listed in the current buffer and return a list of entries in the order in which they appeared in the BibTeX file.
If HT-STRINGS is provided it is assumed to be a hash table used
for string replacement."
  (goto-char (point-min))
  (let  ((strings (cl-loop
                   with ht = (if ht-strings ht-strings (make-hash-table :test #'equal))
                   for entry-type = (parsebib-find-next-item)
                   while entry-type
                   if (string= (downcase entry-type) "string")
                   collect (let ((entry (parsebib-read-string (point) ht)))
                             (puthash (car entry) (cdr entry) ht)
                             entry))))
    (-filter (lambda (x) x) strings)))

(defun bibtex-completion-update-strings-ht (ht strings)
  (cl-loop
   for entry in strings
   do (puthash (car entry) (cdr entry) ht)))

(defvar bibtex-completion-cached-notes-keys nil
  "A cache storing notes keys obtained when the bibliography was last parsed.")

(defun bibtex-completion-candidates ()
  "Read the BibTeX files and return a list of conses, one for each entry.
The first element of these conses is a string containing authors,
editors, title, year, type, and key of the entry.  This string
is used for matching.  The second element is the entry (only the
fields listed above) as an alist."
  (let ((files (bibtex-completion-normalize-bibliography 'bibtex))
        (ht-strings (make-hash-table :test #'equal))
        reparsed-files)

    ;; Open each bibliography file in a temporary buffer,
    ;; check hash of bibliography and mark for reparsing if necessary:

    (cl-loop
     for file in files
     do
     (with-temp-buffer
       (insert-file-contents file)
       (let ((bibliography-hash (secure-hash 'sha256 (current-buffer))))
         (unless (string= (cadr (assoc file bibtex-completion-cache))
                          bibliography-hash)
           ;; Mark file as reparsed.
           ;; This will be useful to resolve cross-references:
           (push file reparsed-files)))))

    ;; TODO This code doesn't belong here.  It's specific to just one
    ;; way of doing notes and should be a module.  Could be run via a
    ;; hook, a defadvice, or perhaps via inotify when the notes file
    ;; changes.  The benefit of the last option is that it needs no
    ;; new interface in core bibtex-completion and runs independently.
    ;; The downside is that we get in trouble if the user changes the
    ;; `bibtex-completion-notes-path'.  We're then tracking an
    ;; incorrect file.

    ;; TODO This code does not respect
    ;; `bibtex-completion-notes-key-pattern'.

    ;; TODO There should be a checksum for the notes file and the keys
    ;; should only be collected if this checksum has changed.

    ;; TODO This code should only be run if we actually reload BibTeX
    ;; files.  No need to do it otherwise.
    (when (and bibtex-completion-notes-path
               (f-file? bibtex-completion-notes-path))
      (require 'org-element)
      (with-temp-buffer
	(org-mode)     ;;  need this to avoid error in emacs 25.3.1
        (insert-file-contents bibtex-completion-notes-path)
        (setq bibtex-completion-cached-notes-keys
              (let ((tree (org-element-parse-buffer 'headline)))
                (org-element-map tree 'headline
                  (lambda (key) (org-element-property :CUSTOM_ID key)))))))

    ;; reparse if necessary

    (when reparsed-files
      (cl-loop
       for file in files
       do
       (with-temp-buffer
         (insert-file-contents file)
         (let ((bibliography-hash (secure-hash 'sha256 (current-buffer))))
           (if (not (member file reparsed-files))
               (bibtex-completion-update-strings-ht ht-strings
                                                    (cddr (assoc file bibtex-completion-string-cache)))
             (progn
               (message "Parsing bibliography file %s ..." file)
               (bibtex-completion-clear-string-cache (list file))
               (push (-cons* file
                             bibliography-hash
                             (bibtex-completion-parse-strings ht-strings))
                     bibtex-completion-string-cache)

               (bibtex-completion-clear-cache (list file))
               (push (-cons* file
                             bibliography-hash
                             (bibtex-completion-parse-bibliography ht-strings))
                     bibtex-completion-cache))))))
      (setf bibtex-completion-string-hash-table ht-strings))

    ;; If some files were reparsed, resolve cross-references:
    (when reparsed-files
      (message "Resolving cross-references ...")
      (bibtex-completion-resolve-crossrefs files reparsed-files))

    ;; Finally return the list of candidates:
    (message "Done (re)loading bibliography.")
    (cl-loop
     for file in files
     append (reverse (cddr (assoc file bibtex-completion-cache))))))

(defun bibtex-completion-resolve-crossrefs (files reparsed-files)
  "Expand all entries with fields from cross-referenced entries in FILES, assuming that only those files in REPARSED-FILES were reparsed whereas the other files in FILES were up-to-date."
  (cl-loop
   with entry-hash = (bibtex-completion-make-entry-hash files reparsed-files)
   for file in files
   for entries = (cddr (assoc file bibtex-completion-cache))
   if (member file reparsed-files)
   ;; The file was reparsed.
   ;; Resolve crossrefs then make candidates for all entries:
   do (setf
       (cddr (assoc file bibtex-completion-cache))
       (cl-loop
        for entry in entries
        ;; Entries are alists of \(FIELD . VALUE\) pairs.
        for crossref = (bibtex-completion-get-value "crossref" entry)
        collect (bibtex-completion-make-candidate
                 (if crossref
                     (bibtex-completion-remove-duplicated-fields
                      ;; Insert an empty field so we can discard the crossref info if needed:
                      (append entry
                              (cl-acons "" ""
                                     (gethash (downcase crossref) entry-hash))))
                   entry))))
   else
   ;; The file was not reparsed.
   ;; Resolve crossrefs then make candidates for the entries with a crossref field:
   do (setf
       (cddr (assoc file bibtex-completion-cache))
       (cl-loop
        for entry in entries
        ;; Entries are \(STRING . ALIST\) conses.
        for entry-alist = (cdr entry)
        for crossref = (bibtex-completion-get-value "crossref" entry-alist)
        collect (if crossref
                    (bibtex-completion-make-candidate
                     (bibtex-completion-remove-duplicated-fields
                      ;; Discard crossref info and resolve crossref again:
                      (append (--take-while (> (length (car it)) 0) entry-alist)
                              (cl-acons "" ""
                                     (gethash (downcase crossref) entry-hash)))))
                  entry)))))

(defun bibtex-completion-make-entry-hash (files reparsed-files)
  "Return a hash table of all potentially cross-referenced bibliography entries in FILES, assuming that only those files in REPARSED-FILES were reparsed whereas the other files in FILES were up-to-date.
Only entries whose type belongs to
`bibtex-completion-cross-referenced-entry-types' are included in
the hash table."
  (cl-loop
   with entries =
     (cl-loop
      for file in files
      for entries = (cddr (assoc file bibtex-completion-cache))
      if (member file reparsed-files)
      ;; Entries are alists of \(FIELD . VALUE\) pairs.
      append entries
      ;; Entries are \(STRING . ALIST\) conses.
      else
      append (mapcar 'cdr entries))
   with ht = (make-hash-table :test #'equal :size (length entries))
   for entry in entries
   for key = (bibtex-completion-get-value "=key=" entry)
   if (member (downcase (bibtex-completion-get-value "=type=" entry))
              bibtex-completion-cross-referenced-entry-types)
   do (puthash (downcase key) entry ht)
   finally return ht))

(defun bibtex-completion-make-candidate (entry)
  "Return a candidate for ENTRY."
  (let* ((candidate (bibtex-completion-clean-string
                     (s-join " " (-map #'cdr entry))))
         (candidate (concat candidate " " (car (assoc "=has-pdf=" entry))))
         (candidate (concat candidate " " (car (assoc "=has-note=" entry)))))
    (cons candidate entry)))

(defun bibtex-completion-parse-bibliography (&optional ht-strings)
  "Parse the BibTeX entries listed in the current buffer and return a list of entries in the order in which they appeared in the BibTeX file.
Also do some preprocessing of the entries.

If HT-STRINGS is provided it is assumed to be a hash table."
  (goto-char (point-min))
  (cl-loop
   with fields = (append '("title" "crossref")
                         (-map (lambda (it) (if (symbolp it) (symbol-name it) it))
                               bibtex-completion-additional-search-fields))
   for entry-type = (parsebib-find-next-item)
   while entry-type
   unless (member-ignore-case entry-type '("preamble" "string" "comment"))
   collect (let* ((entry (parsebib-read-entry entry-type (point) ht-strings))
                  (fields (append
                           (list (if (assoc-string "author" entry 'case-fold)
                                     "author"
                                   "editor")
                                 (if (assoc-string "date" entry 'case-fold)
                                     "date"
                                   "year"))
                           fields)))
             (-map (lambda (it)
                     (cons (downcase (car it)) (cdr it)))
                   (bibtex-completion-prepare-entry entry fields)))))

(defun bibtex-completion-get-entry (entry-key)
  "Given a BibTeX key this function scans all bibliographies listed in `bibtex-completion-bibliography' and returns an alist of the record with that key.
Fields from crossreferenced entries are appended to the requested entry."
  (let* ((entry (bibtex-completion-get-entry1 entry-key))
         (crossref (bibtex-completion-get-value "crossref" entry))
         (crossref (when crossref (bibtex-completion-get-entry1 crossref))))
    (bibtex-completion-remove-duplicated-fields (append entry crossref))))

(defun bibtex-completion-get-entry1 (entry-key &optional do-not-find-pdf)
  (let ((bib (bibtex-completion-normalize-bibliography 'bibtex)))
    (with-temp-buffer
      (mapc #'insert-file-contents bib)
      (goto-char (point-min))
      (if (re-search-forward (concat "^[ \t]*@\\(" parsebib--bibtex-identifier
                                     "\\)[[:space:]]*[\(\{][[:space:]]*"
                                     (regexp-quote entry-key) "[[:space:]]*,")
                             nil t)
          (let ((entry-type (match-string 1)))
            (reverse (bibtex-completion-prepare-entry
                      (parsebib-read-entry entry-type (point) bibtex-completion-string-hash-table) nil do-not-find-pdf)))
        (progn
          (display-warning :warning (concat "Bibtex-completion couldn't find entry with key \"" entry-key "\"."))
          nil)))))

(defun bibtex-completion-find-pdf-in-field (key-or-entry)
  "Return the path of the PDF specified in the field `bibtex-completion-pdf-field' if that file exists.
Returns nil if no file is specified, or if the specified file
does not exist, or if `bibtex-completion-pdf-field' is nil."
  (when bibtex-completion-pdf-field
    (let* ((entry (if (stringp key-or-entry)
                      (bibtex-completion-get-entry1 key-or-entry t)
                    key-or-entry))
           (value (bibtex-completion-get-value bibtex-completion-pdf-field entry)))
      (cond
       ((not value) nil)         ; Field not defined.
       ((f-file? value) (list value))   ; A bare full path was found.
       ((-any 'f-file? (--map (f-join it (f-filename value)) (-flatten bibtex-completion-library-path))) (-filter 'f-file? (--map (f-join it (f-filename value)) (-flatten bibtex-completion-library-path))))
       (t                               ; Zotero/Mendeley/JabRef/Calibre format:
        (let ((value (replace-regexp-in-string "\\([^\\]\\)[;,]" "\\1\^^" value)))
          (cl-loop  ; Looping over the files:
           for record in (s-split "\^^" value)
                                        ; Replace unescaped colons by field separator:
           for record = (replace-regexp-in-string "\\([^\\]\\|^\\):" "\\1\^_" record)
                                        ; Unescape stuff:
           for record = (replace-regexp-in-string "\\\\\\(.\\)" "\\1" record)
                                        ; Now we can safely split:
           for record = (s-split "\^_" record)
           for file-name = (nth 0 record)
           for path = (or (nth 1 record) "")
           for paths = (if (s-match "^[A-Z]:" path)
                           (list path)                 ; Absolute Windows path
                                        ; Something else:
                         (append
                          (list
                           path
                           file-name
                           (f-join (f-root) path) ; Mendeley #105
                           (f-join (f-root) path file-name)) ; Mendeley #105
                          (--map (f-join it path)
                                 (-flatten bibtex-completion-library-path)) ; Jabref #100
                          (--map (f-join it path file-name)
                                 (-flatten bibtex-completion-library-path)))) ; Jabref #100
           for result = (-first (lambda (path)
                                  (if (and (not (s-blank-str? path))
                                           (f-exists? path))
                                      path nil)) paths)
           if result collect result)))))))


(defun bibtex-completion-find-pdf-in-library (key-or-entry &optional find-additional)
  "Searches the directories in `bibtex-completion-library-path' for a PDF whose name is composed of the BibTeX key plus `bibtex-completion-pdf-extension'.
The path of the first matching PDF is returned.

If FIND-ADDITIONAL is non-nil, the paths of all PDFs whose name
starts with the BibTeX key and ends with
`bibtex-completion-pdf-extension' are returned instead."
  (let* ((key (if (stringp key-or-entry)
                  key-or-entry
                (bibtex-completion-get-value "=key=" key-or-entry)))
         (main-pdf (cl-loop
                    for dir in (-flatten bibtex-completion-library-path)
                    append (cl-loop
                            for ext in (-flatten bibtex-completion-pdf-extension)
                            collect (f-join dir (s-concat key ext))))))
    (if find-additional
        (sort ; move main pdf on top of the list if needed
         (cl-loop
          for dir in (-flatten bibtex-completion-library-path)
          append (directory-files dir t
                                  (s-concat "^" (regexp-quote key)
                                            ".*\\("
                                            (mapconcat 'regexp-quote
                                                       (-flatten bibtex-completion-pdf-extension)
                                                       "\\|")
                                            "\\)$")))
         (lambda (x y)
           (and (member x main-pdf)
                (not (member y main-pdf)))))
      (-flatten (-first 'f-file? main-pdf)))))

(defun bibtex-completion-find-pdf (key-or-entry &optional find-additional)
  "Return the path of the PDF associated with the specified entry KEY-OR-ENTRY.
This is either the path(s) specified in the field
`bibtex-completion-pdf-field' or, if that does not exist, the
first PDF in any of the directories in
`bibtex-completion-library-path' whose name is composed of the
the BibTeX key plus `bibtex-completion-pdf-extension' (or if
FIND-ADDITIONAL is non-nil, all PDFs in
`bibtex-completion-library-path' whose name starts with the
BibTeX key and ends with `bibtex-completion-pdf-extension').
Returns nil if no PDF is found."
  (or (bibtex-completion-find-pdf-in-field key-or-entry)
      (bibtex-completion-find-pdf-in-library key-or-entry find-additional)))

(defun bibtex-completion-find-note-multiple-files (entry-key)
  "Find note file associated with entry ENTRY-KEY in the default directory.
The default directory is `bibtex-completion-notes-path'.  If the
note file doesn’t exist, return nil."
  (and bibtex-completion-notes-path
       (f-directory? bibtex-completion-notes-path)
       (f-file? (f-join bibtex-completion-notes-path
                        (s-concat entry-key
                                  bibtex-completion-notes-extension)))))

(defun bibtex-completion-find-note-one-file (entry-key)
  "Find notes associated with entry ENTRY-KEY in the single notes file.
The single notes file is the one specified in
`bibtex-completion-notes-path'.  If no note exists, return nil."
  (and bibtex-completion-notes-path
       (f-file? bibtex-completion-notes-path)
       (member entry-key bibtex-completion-cached-notes-keys)))

;; This defvar allows other packages like org-roam-bibtex to customize
;; the back-end for storing notes.
(defvar bibtex-completion-find-note-functions
  (list #'bibtex-completion-find-note-multiple-files
        #'bibtex-completion-find-note-one-file)
  "List of functions to use to find note files.
The functions should accept one argument: the key of the BibTeX
entry and return non-nil if notes exist for that entry.")

(defun bibtex-completion-prepare-entry (entry &optional fields do-not-find-pdf)
  "Prepare ENTRY for display.
ENTRY is an alist representing an entry as returned by
`parsebib-read-entry'.  All the fields not in FIELDS are removed
from ENTRY, with the exception of the \"=type=\" and \"=key=\"
fields.  If FIELDS is empty, all fields are kept.  Also add a
=has-pdf= and/or =has-note= field, if they exist for ENTRY.  If
DO-NOT-FIND-PDF is non-nil, this function does not attempt to
find a PDF file."
  (when entry ; entry may be nil, in which case just return nil
    (let* ((fields (when fields (append fields (list "=type=" "=key=" "=has-pdf=" "=has-note="))))
           ; Check for PDF:
           (entry (if (and (not do-not-find-pdf) (bibtex-completion-find-pdf entry))
                      (cons (cons "=has-pdf=" bibtex-completion-pdf-symbol) entry)
                    entry))
           (entry-key (cdr (assoc "=key=" entry)))
           ; Check for notes:
           (entry (if (cl-some #'identity
                               (mapcar (lambda (fn)
                                         (funcall fn entry-key))
                                       bibtex-completion-find-note-functions))
                      (cons (cons "=has-note=" bibtex-completion-notes-symbol) entry)
                    entry))
           ; Remove unwanted fields:
           (entry (if fields
                       (--filter (member-ignore-case (car it) fields) entry)
                    entry)))
      ;; Normalize case of entry type:
      (setcdr (assoc "=type=" entry) (downcase (cdr (assoc "=type=" entry))))
      ;; Remove duplicated fields:
      (bibtex-completion-remove-duplicated-fields entry))))

(defun bibtex-completion-remove-duplicated-fields (entry)
  "Remove duplicated fields from ENTRY."
  (cl-remove-duplicates entry
                        :test (lambda (x y) (string= (s-downcase x) (s-downcase y)))
                        :key 'car :from-end t))


(defun bibtex-completion-format-entry (entry width)
  "Formats a BibTeX ENTRY for display in results list.
WIDTH is the width of the results list.  The display format is
governed by the variable `bibtex-completion-display-formats'."
  (let* ((format
          (or (assoc-string (bibtex-completion-get-value "=type=" entry)
                            bibtex-completion-display-formats-internal
                            'case-fold)
              (assoc t bibtex-completion-display-formats-internal)))
         (format-string (cadr format)))
    (s-format
     format-string
     (lambda (field)
       (let* ((field (split-string field ":"))
              (field-name (car field))
              (field-width (cadr field))
              (field-value (bibtex-completion-get-value field-name entry)))
         (when (and (string= field-name "author")
                    (not field-value))
           (setq field-value (bibtex-completion-get-value "editor" entry)))
         (when (and (string= field-name "year")
                    (not field-value))
           (setq field-value (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
         (setq field-value (bibtex-completion-clean-string (or field-value " ")))
         (when (member field-name '("author" "editor"))
           (setq field-value (bibtex-completion-shorten-authors field-value)))
         (if (not field-width)
             field-value
           (setq field-width (string-to-number field-width))
           (truncate-string-to-width
            field-value
            (if (> field-width 0)
                field-width
              (- width (cddr format)))
            0 ?\s)))))))


(defun bibtex-completion-clean-string (s)
  "Remove quoting and superfluous white space from BibTeX field value in S."
  (if s (replace-regexp-in-string "[\n\t ]+" " "
         (replace-regexp-in-string "[\"{}]+" "" s))
    nil))

(defun bibtex-completion-shorten-authors (authors)
  "Return a comma-separated list of the surnames in AUTHORS."
  (if authors
      (cl-loop for a in (s-split " and " authors)
               for p = (s-split "," a t)
               for sep = "" then ", "
               concat sep
               if (eq 1 (length p))
               concat (-last-item (s-split " +" (car p) t))
               else
               concat (car p))
    nil))


(defun bibtex-completion-open-pdf (keys &optional fallback-action)
  "Open the PDFs associated with the marked entries using the function specified in `bibtex-completion-pdf-open-function'.
If multiple PDFs are found for an entry, ask for the one to open
using `completion-read'.  If FALLBACK-ACTION is non-nil, it is
called in case no PDF is found."
  (dolist (key keys)
    (let ((pdf (bibtex-completion-find-pdf key bibtex-completion-find-additional-pdfs)))
      (cond
       ((> (length pdf) 1)
        (let* ((pdf (f-uniquify-alist pdf))
               (choice (completing-read "File to open: " (mapcar 'cdr pdf) nil t))
               (file (car (rassoc choice pdf))))
          (funcall bibtex-completion-pdf-open-function file)))
       (pdf
        (funcall bibtex-completion-pdf-open-function (car pdf)))
       (fallback-action
        (funcall fallback-action (list key)))
       (t
        (message "No PDF(s) found for this entry: %s"
                 key))))))

(defun bibtex-completion-open-url-or-doi (keys)
  "Open the URL or DOI associated with entries in KEYS in a browser."
  (dolist (key keys)
    (let* ((entry (bibtex-completion-get-entry key))
           (url (bibtex-completion-get-value "url" entry))
           (doi (bibtex-completion-get-value "doi" entry))
           (browse-url-browser-function
            (or bibtex-completion-browser-function
                browse-url-browser-function)))
      (if url
          (browse-url url)
        (if doi (browse-url
                 (s-concat "http://dx.doi.org/" doi))
          (message "No URL or DOI found for this entry: %s"
                   key))))))

(defun bibtex-completion-open-any (keys)
  "Open the PDFs associated with the marked entries using the function specified in `bibtex-completion-pdf-open-function'.
If multiple PDFs are found for an entry, ask for the one to open
using `completion-read'.  If no PDF is found, try to open a URL
or DOI in the browser instead."
  (bibtex-completion-open-pdf keys 'bibtex-completion-open-url-or-doi))

(defun bibtex-completion-format-citation-default (keys)
  "Default formatter for keys, separates multiple keys in KEYS with commas."
  (s-join ", " keys))

(defvar bibtex-completion-cite-command-history nil
  "History list for LaTeX citation commands.")

(defun bibtex-completion-format-citation-cite (keys)
  "Formatter for LaTeX citation commands.
Prompts for the command and for arguments if the commands can
take any.  If point is inside or just after a citation command,
only adds KEYS to it."
  (let (macro)
    (cond
     ((and (require 'reftex-parse nil t)
           (setq macro (reftex-what-macro 1))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are inside a cite macro.  Insert key at point, with appropriate delimiters.
      (delete-horizontal-space)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              (if (member (following-char) '(?\} ?,))
		     ""
                ", ")))
     ((and (equal (preceding-char) ?\})
           (require 'reftex-parse nil t)
           (save-excursion
             (forward-char -1)
             (setq macro (reftex-what-macro 1)))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are right after a cite macro.  Append key and leave point at the end.
      (delete-char -1)
      (delete-horizontal-space t)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              "}"))
     (t
      ;; We are not inside or right after a cite macro.  Insert a full citation.
      (let* ((initial (when bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default (unless bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default-info (if default (format " (default \"%s\")" default) ""))
             (cite-command (completing-read
                            (format "Cite command%s: " default-info)
                            bibtex-completion-cite-commands nil nil initial
                            'bibtex-completion-cite-command-history default nil)))
        (if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
            (format "\\%s{%s}" cite-command (s-join ", " keys))
          (let ((prenote (if bibtex-completion-cite-prompt-for-optional-arguments
                             (read-from-minibuffer "Prenote: ")
                           ""))
                (postnote (if bibtex-completion-cite-prompt-for-optional-arguments
                              (read-from-minibuffer "Postnote: ")
                            "")))
            (cond ((not (string= "" prenote))
                   (format "\\%s[%s][%s]{%s}" cite-command prenote postnote (s-join ", " keys)))
                  ((not (string= "" postnote))
                   (format "\\%s[%s]{%s}" cite-command postnote (s-join ", " keys)))
                  (t
                   (format "\\%s{%s}" cite-command (s-join ", " keys)))))))))))

(defun bibtex-completion-format-citation-pandoc-citeproc (keys)
  "Format pandoc-citeproc citations for the entries in KEYS."
  (let* ((prenote  (if bibtex-completion-cite-prompt-for-optional-arguments (read-from-minibuffer "Prenote: ") ""))
         (postnote (if bibtex-completion-cite-prompt-for-optional-arguments (read-from-minibuffer "Postnote: ") ""))
         (prenote  (if (string= "" prenote)  "" (concat prenote  " ")))
         (postnote (if (string= "" postnote) "" (concat ", " postnote))))
    (format "[%s%s%s]" prenote (s-join "; " (--map (concat "@" it) keys)) postnote)))

(defun bibtex-completion-format-citation-ebib (keys)
  "Format ebib references for keys in KEYS."
  (s-join ", "
          (--map (format "ebib:%s" it) keys)))

(defun bibtex-completion-format-citation-sphinxcontrib-bibtex (keys)
  "Format sphinxcontrib-bibtex references for keys in KEYS."
  (format ":cite:`%s`" (s-join "," keys)))

(defun bibtex-completion-format-citation-org-link-to-PDF (keys)
  "Format org-links to PDFs associated with entries in KEYS.
Uses first matching PDF if several are available.  Entries for
which no PDF is available are omitted."
  (s-join ", " (cl-loop
                for key in keys
                for pdfs = (bibtex-completion-find-pdf key bibtex-completion-find-additional-pdfs)
                append (with-no-warnings (--map (org-make-link-string it key) pdfs)))))

(defun bibtex-completion-format-citation-org-cite (keys)
  "Format org-links using Org mode's own cite syntax."
  (format "[cite:%s]"
    (s-join ";"
            (--map (format "@%s" it) keys))))

(defun bibtex-completion-format-citation-org-apa-link-to-PDF (keys)
  "Format org-links to PDF for entries in KEYS.
Link text loosely follows APA format.  Uses first matching PDF if
several are available."
  (s-join ", " (cl-loop
                for key in keys
                for entry = (bibtex-completion-get-entry key)
                for author = (bibtex-completion-shorten-authors
                              (or (bibtex-completion-get-value "author" entry)
                                  (bibtex-completion-get-value "editor" entry)))
                for year = (or (bibtex-completion-get-value "year" entry)
                               (car (split-string (bibtex-completion-get-value "date" entry "") "-")))
                for pdf = (car (bibtex-completion-find-pdf key))
                if pdf
                  collect (with-no-warnings (org-make-link-string pdf (format "%s (%s)" author year)))
                else
                  collect (format "%s (%s)" author year))))

;; When you want to create a todo list about reading, I think using
;; PDF's title is more intuitive.
(defun bibtex-completion-format-citation-org-title-link-to-PDF (keys)
  "Formatter org-links to PDFs associated with entries in KEYS.
Link text follows file title format.  Uses first matching PDF if
several are available."
  (s-join ", " (cl-loop
                for key in keys
                for entry = (bibtex-completion-get-entry key)
                for title = (bibtex-completion-apa-get-value "title" entry)
                for pdf = (or (car (bibtex-completion-find-pdf key))
                              (bibtex-completion-get-value "url" entry))
                if pdf
                  collect (with-no-warnings (org-make-link-string pdf title))
                else
                collect (format "%s" title))))

(defun bibtex-completion-insert-citation (keys)
  "Insert citations for entries in KEYS at point.
The format depends on
`bibtex-completion-format-citation-functions'."
  (let ((format-function
         (cdr (or (assoc major-mode bibtex-completion-format-citation-functions)
                  (assoc 'default   bibtex-completion-format-citation-functions)))))
    (insert
     (funcall format-function keys))))

(defun bibtex-completion-insert-reference (keys)
  "Insert references for entries in KEYS."
  (let* ((refs (--map
                (s-word-wrap fill-column
                             (concat "\n- " (bibtex-completion-apa-format-reference it)))
                keys)))
    (insert "\n" (s-join "\n" refs) "\n")))

(defun bibtex-completion-apa-format-reference (key)
  "Return a plain text reference in APA format for the publication specified by KEY."
  (let*
   ((entry (bibtex-completion-get-entry key))
    (ref (pcase (downcase (bibtex-completion-get-value "=type=" entry))
           ("article"
            (s-format
             "${author} (${year}). ${title}. ${journal}, ${volume}(${number}), ${pages}.${doi}"
             'bibtex-completion-apa-get-value entry))
           ("inproceedings"
            (s-format
             "${author} (${year}). ${title}. In ${editor}, ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("book"
            (s-format
             "${author} (${year}). ${title}. ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("phdthesis"
            (s-format
             "${author} (${year}). ${title} (Doctoral dissertation). ${school}, ${address}."
             'bibtex-completion-apa-get-value entry))
           ("inbook"
            (s-format
             "${author} (${year}). ${chapter}. In ${editor} (Eds.), ${title} (pp. ${pages}). ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("incollection"
            (s-format
             "${author} (${year}). ${title}. In ${editor} (Eds.), ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("proceedings"
            (s-format
             "${editor} (Eds.). (${year}). ${booktitle}. ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("unpublished"
            (s-format
             "${author} (${year}). ${title}. Unpublished manuscript."
             'bibtex-completion-apa-get-value entry))
           (_
            (s-format
             "${author} (${year}). ${title}."
             'bibtex-completion-apa-get-value entry)))))
   (replace-regexp-in-string "\\([.?!]\\)\\." "\\1" ref))) ; Avoid sequences of punctuation marks.

(defun bibtex-completion-apa-get-value (field entry &optional default)
  "Return FIELD or ENTRY formatted following the APA guidelines.
Return DEFAULT if FIELD is not present in ENTRY.  Return empty
string if FIELD is not present in ENTRY and DEFAULT is nil."
  (or
   (pcase field
     ;; Virtual fields:
     ("author-or-editor"
      ;; Avoid if-let and when-let because they're not working reliably
      ;; in all versions of Emacs that we currently support:
      (if-let ((value (bibtex-completion-get-value "author" entry)))
          (bibtex-completion-apa-format-authors value)
        (when-let ((value (bibtex-completion-get-value "editor" entry)))
          (bibtex-completion-apa-format-editors value))))
     ("author-or-editor-abbrev"
      (if-let ((value (bibtex-completion-get-value "author" entry)))
          (bibtex-completion-apa-format-authors-abbrev value)
        (when-let ((value (bibtex-completion-get-value "editor" entry)))
          (bibtex-completion-apa-format-editors-abbrev value))))
     ("author-abbrev"
      (when-let ((value (bibtex-completion-get-value "author" entry)))
        (bibtex-completion-apa-format-authors-abbrev value)))
     ("editor-abbrev"
      (when-let ((value (bibtex-completion-get-value "editor" entry)))
        (bibtex-completion-apa-format-editors-abbrev value)))
     ((or "journal" "journaltitle")
      (or (bibtex-completion-get-value "journal" entry)
          (bibtex-completion-get-value "journaltitle" entry)))
     (_
      ;; Real fields:
      (let ((value (bibtex-completion-get-value field entry)))
        (if value
            (pcase field
              ;; https://owl.english.purdue.edu/owl/resource/560/06/
              ("author" (bibtex-completion-apa-format-authors value))
              ("editor" (bibtex-completion-apa-format-editors value))
              ;; When referring to books, chapters, articles, or Web pages,
              ;; capitalize only the first letter of the first word of a
              ;; title and subtitle, the first word after a colon or a dash
              ;; in the title, and proper nouns.  Do not capitalize the first
              ;; letter of the second word in a hyphenated compound word.
              ("title" (replace-regexp-in-string ; remove braces
                        "[{}]"
                        ""
                        (replace-regexp-in-string ; remove macros
                         "\\\\[[:alpha:]]+{"
                         ""
                         (replace-regexp-in-string ; upcase initial letter
                          "^[[:alpha:]]"
                          'upcase
                          (replace-regexp-in-string ; preserve stuff in braces from being downcased
                           "\\(^[^{]*{\\)\\|\\(}[^{]*{\\)\\|\\(}.*$\\)\\|\\(^[^{}]*$\\)"
                           (lambda (x) (downcase (s-replace "\\" "\\\\" x)))
                           value)))))
              ("booktitle" value)
              ;; Maintain the punctuation and capitalization that is used by
              ;; the journal in its title.
              ("pages" (s-join "–" (s-split "[^0-9]+" value t)))
              ("doi" (s-concat " http://dx.doi.org/" value))
              ("year" value)
              (_ value))
          (pcase field
            ("year" (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
          ))))
   default ""))

(defun bibtex-completion-apa-format-authors (value &optional abbrev)
  "Format author list in VALUE in APA style.
When ABBREV is non-nil, format in abbreviated APA style instead."
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "{" a)
           collect
           (replace-regexp-in-string "[{}]" "" a)
           into authors
           else if (s-index-of "," a)
           collect
           (let ((p (s-split " *, *" a t)))
             (concat
              (car p) ", "
              (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                (s-split " " (cadr p))))))
           into authors
           else
           collect
           (let ((p (s-split " " a t)))
             (concat
              (-last-item p) ", "
              (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                (-butlast p)))))
           into authors
           finally return
           (let ((l (length authors)))
             (cond
              ((= l 1) (car authors))
              ((and abbrev (= l 2))
               (concat (s-join " & " authors)))
              (abbrev
               (format "%s et al." (car authors)))
              ((< l 8) (concat (s-join ", " (-butlast authors))
                               ", & " (-last-item authors)))
              (t (concat (s-join ", " (-slice authors 0 7)) ", …"))))))

(defun bibtex-completion-apa-format-authors-abbrev (value)
  "Format author list in VALUE in abbreviated APA style."
  (bibtex-completion-apa-format-authors value t))

(defun bibtex-completion-apa-format-editors (value &optional abbrev)
  "Format editors in VALUE in APA style.
When ABBREV is non-nil, format in abbreviated APA style instead."
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "," a)
           collect
           (let ((p (s-split " *, *" a t)))
             (concat
              (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                (s-split " " (cadr p))))
              " " (car p)))
           into editors
           else
           collect
           (let ((p (s-split " " a t)))
             (concat
              (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                (-butlast p)))
              " " (-last-item p)))
           into editors
           finally return
           (let ((l (length editors)))
             (cond
              ((= l 1) (car editors))
              ((and abbrev (= l 2))
               (concat (s-join " & " editors)))
              (abbrev
               (format "%s et al." (car editors)))
              ((< l 8) (concat (s-join ", " (-butlast editors))
                               ", & " (-last-item editors)))
              (t (concat (s-join ", " (-slice editors 0 7)) ", …"))))))

(defun bibtex-completion-apa-format-editors-abbrev (value)
  "Format editor list in VALUE in abbreviated APA style."
  (bibtex-completion-apa-format-editors value t))

(defun bibtex-completion-get-value (field entry &optional default)
  "Return the value for FIELD in ENTRY or DEFAULT if the value is not defined.
Surrounding curly braces are stripped."
  (let ((value (cdr (assoc-string field entry 'case-fold))))
    (if value
        (replace-regexp-in-string
         "\\(^[[:space:]]*[\"{][[:space:]]*\\)\\|\\([[:space:]]*[\"}][[:space:]]*$\\)"
         ""
         ;; Collapse whitespaces when the content is not a path:
         (if (equal bibtex-completion-pdf-field field)
             value
           (s-collapse-whitespace value)))
      default)))

(defun bibtex-completion-insert-key (keys)
  "Insert BibTeX KEYS at point."
  (insert
   (funcall 'bibtex-completion-format-citation-default keys)))

(defun bibtex-completion-insert-bibtex (keys)
  "Insert BibTeX entries for entries in KEYS at point."
  (insert (s-join "\n" (--map (bibtex-completion-make-bibtex it) keys))))

(defun bibtex-completion-make-bibtex (key)
  "Create a self-contained BibTeX entry KEY.
Self-contained means that cross-referenced entries are merged."
  (let* ((entry (bibtex-completion-get-entry key))
         (entry-type (bibtex-completion-get-value "=type=" entry)))
    (format "@%s{%s,\n%s}\n"
            entry-type key
            (cl-loop
             for field in entry
             for name = (car field)
             for value = (cdr field)
             unless (member name
                            (append (-map (lambda (it) (if (symbolp it) (symbol-name it) it))
                                          bibtex-completion-no-export-fields)
                             '("=type=" "=key=" "=has-pdf=" "=has-note=" "crossref")))
             concat
             (format "  %s = {%s},\n" name value)))))

(defun bibtex-completion-add-PDF-attachment (keys)
  "Attach the PDFs of the entries with the given KEYS where available."
  (dolist (key keys)
    (let ((pdf (bibtex-completion-find-pdf key bibtex-completion-find-additional-pdfs)))
      (if pdf
          (mapc 'mml-attach-file pdf)
        (message "No PDF(s) found for this entry: %s"
                 key)))))

(define-minor-mode bibtex-completion-notes-mode
  "Minor mode for managing notes."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'bibtex-completion-exit-notes-buffer)
            (define-key map (kbd "C-c C-w") 'org-refile)
            map)
  (setq-local
   header-line-format
   (substitute-command-keys
    " Finish \\[bibtex-completion-exit-notes-buffer], refile \\[org-refile]")))

;; Define global minor mode.  This is needed to the toggle minor mode.
;;;###autoload
(define-globalized-minor-mode bibtex-completion-notes-global-mode bibtex-completion-notes-mode bibtex-completion-notes-mode)

(defun bibtex-completion-exit-notes-buffer ()
  "Exit notes buffer and delete its window.
This will also disable `bibtex-completion-notes-mode' and remove
the header line."
  (interactive)
  (widen)
  (bibtex-completion-notes-global-mode -1)
  (setq-local
   header-line-format nil)
  (save-buffer)
  (let ((window (get-buffer-window (get-file-buffer bibtex-completion-notes-path))))
    (if (and window (not (one-window-p window)))
        (delete-window window)
      (switch-to-buffer (other-buffer)))))

(defun bibtex-completion-fill-template (entry template)
  "Fill TEMPLATE according to info from ENTRY.

First, the BibTeX fields are expanded (e.g. ${field-name}).
Then, the `org-capture' %-escapes are replaced with their values
according to `org-capture-templates'."
  (let ((bibtex-exp (s-format template
                              'bibtex-completion-apa-get-value
                              entry)))
    ;; Delete trailing newline inserted by `org-capture-fill-template'
    (substring
     (->> bibtex-exp
          ;; Escape newlines to prevent `org-capture-fill-template' from
          ;; gobbling them
          (replace-regexp-in-string "\n" "\\\\n")
          (org-capture-fill-template)
          ;; Restore newlines
          (replace-regexp-in-string "\\\\n" "\n"))
     0 -1)))

;; The purpose of this defvar is to allow other packages like
;; org-roam-bibtex to customize the back-end used for notes.
(defvar bibtex-completion-edit-notes-function
  #'bibtex-completion-edit-notes-default
  "Function used to edit notes.
The function should accept one argument, a list of BibTeX keys.")

;; TODO Split this function into two, one for one file per note and
;; the other for one file for all notes.
(defun bibtex-completion-edit-notes-default (keys)
  "Open the notes associated with the entries in KEYS.
Creates new notes where none exist yet."
  (dolist (key keys)
    (let* ((entry (bibtex-completion-get-entry key))
           (year (or (bibtex-completion-get-value "year" entry)
                     (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
           (entry (push (cons "year" year) entry)))
      (if (and bibtex-completion-notes-path
               (f-directory? bibtex-completion-notes-path))
                                        ; One notes file per publication:
          (let* ((path (f-join bibtex-completion-notes-path
                               (s-concat key bibtex-completion-notes-extension))))
            (find-file path)
            (unless (f-exists? path)
              ;; First expand BibTeX variables, then org-capture template vars:
              (insert (bibtex-completion-fill-template
                       entry
                       bibtex-completion-notes-template-multiple-files))))
                                        ; One file for all notes:
        (unless (and buffer-file-name
                     (f-same? bibtex-completion-notes-path buffer-file-name))
          (find-file-other-window bibtex-completion-notes-path))
        (widen)
        (outline-show-all)
        (goto-char (point-min))
        (if (re-search-forward (format bibtex-completion-notes-key-pattern (regexp-quote key)) nil t)
                                        ; Existing entry found:
            (when (eq major-mode 'org-mode)
              (org-narrow-to-subtree)
              (re-search-backward "^\*+ " nil t)
              (org-cycle-hide-drawers nil)
              (bibtex-completion-notes-mode 1))
                                        ; Create a new entry:
          (goto-char (point-max))
          (save-excursion (insert (bibtex-completion-fill-template
                                   entry
                                   bibtex-completion-notes-template-one-file)))
          (re-search-forward "^*+ " nil t))
        (when (eq major-mode 'org-mode)
          (org-narrow-to-subtree)
          (re-search-backward "^\*+ " nil t)
          (org-cycle-hide-drawers nil)
          (goto-char (point-max))
          (bibtex-completion-notes-mode 1))
        ;; Move point to ‘%?’ if it’s included in the pattern
        (when (save-excursion
                (progn (goto-char (point-min))
                       (re-search-forward "%\\?" nil t)))
          (let ((beginning (match-beginning 0))
                (end (match-end 0)))
            (delete-region beginning end)
            (goto-char beginning)))))))

(defun bibtex-completion-edit-notes (keys)
  "Open the notes associated with KEYS using `bibtex-completion-edit-notes-function'."
  (funcall bibtex-completion-edit-notes-function keys))

(defun bibtex-completion-show-entry (keys)
  "Show the first entry in KEYS in the relevant BibTeX file."
  (catch 'break
    (dolist (bib-file (bibtex-completion-normalize-bibliography 'main))
      (let ((key (car keys))
            (buf (or (get-file-buffer bib-file)
                     (find-buffer-visiting bib-file))))
        (find-file bib-file)
        (widen)
        (if (eq major-mode 'org-mode)
            (let* ((prop (if (boundp 'org-bibtex-key-property)
                             org-bibtex-key-property
                           "CUSTOM_ID"))
                   (match (org-find-property prop key)))
              (when match
                (goto-char match)
                (org-show-entry)
                (throw 'break t)))
          (goto-char (point-min))
          (when (re-search-forward
                 (concat "^@\\(" parsebib--bibtex-identifier
                         "\\)[[:space:]]*[\(\{][[:space:]]*"
                         (regexp-quote key) "[[:space:]]*,") nil t)
            (throw 'break t)))
        (unless buf
          (kill-buffer))))))

(defun bibtex-completion-add-pdf-to-library (keys)
  "Add a PDF to the library for the first entry in KEYS.
The PDF can be added either from an open buffer, a file, or a
URL."
  (let* ((key (car keys))
         (source (char-to-string
                  (read-char-choice "Add pdf from [b]uffer, [f]ile, or [u]rl? " '(?b ?f ?u))))
         (buffer (when (string= source "b")
                   (read-buffer-to-switch "Add pdf buffer: ")))
         (file (when (string= source "f")
                 (expand-file-name (read-file-name "Add pdf file: " nil nil t))))
         (url (when (string= source "u")
                (read-string "Add pdf URL: ")))
         (path (-flatten (list bibtex-completion-library-path)))
         (path (if (cdr path)
                   (completing-read "Add pdf to: " path nil t)
                 (car path)))
         (pdf (expand-file-name (completing-read "Rename pdf to: "
                                                 (--map (s-concat key it)
                                                        (-flatten bibtex-completion-pdf-extension))
                                                 nil nil key)
                                path)))
    (cond
     (buffer
      (with-current-buffer buffer
        (write-file pdf t)))
     (file
      (copy-file file pdf 1))
     (url
      (url-copy-file url pdf 1)))))

(defun bibtex-completion-fallback-action (url-or-function search-expression)
  "Execute fallback option.
Could consist of opening an URL or executing a function,
depending of type of URL-OR-FUNCTION (`stringp' or `function').
If string, SEARCH-EXPRESSION will be inserted at %s in string.
If function, it will be called with SEARCH-EXPRESSION as
argument."
  (let ((browse-url-browser-function
          (or bibtex-completion-browser-function
              browse-url-browser-function)))
    (cond
      ((stringp url-or-function)
        (browse-url (format url-or-function (url-hexify-string search-expression))))
      ((functionp url-or-function)
        (funcall url-or-function search-expression))
      (t (error "Don't know how to interpret this: %s" url-or-function)))))

(defun bibtex-completion-fallback-candidates ()
  "Compile list of fallback options.
These consist of the online resources defined in
`bibtex-completion-fallback-options' plus one entry for each
bibliography file that will open that file for editing."
  (let ((bib-files (bibtex-completion-normalize-bibliography 'main)))
    (-concat
      (--map (cons (s-concat "Create new entry in " (f-filename it))
                   `(lambda (_search-expression) (find-file ,it) (goto-char (point-max)) (newline)))
             bib-files)
      bibtex-completion-fallback-options)))

(defun bibtex-completion-find-local-bibliography ()
  "Return a list of BibTeX files associated with the current file.

If the current file is a BibTeX file, return this file.  In LaTeX
documents, use `reftex' to find associated BibTeX files.  In org
files return the local or global org bibliography (see oc.el).
If all fails, return nil."
  (or (and (buffer-file-name)
           (string= (or (f-ext (buffer-file-name)) "") "bib")
           (list (buffer-file-name)))
      ;; LaTeX:
      (and (buffer-file-name)
           (string= (or (f-ext (buffer-file-name)) "") "tex")
           (require 'reftex-cite nil t)
           (ignore-errors (reftex-get-bibfile-list)))
      ;; Org (with oc.el):
      (and (buffer-file-name)
           (string= (or (f-ext (buffer-file-name)) "") "org")
           (fboundp 'org-cite-list-bibliography-files)
           (org-cite-list-bibliography-files))))

(defun bibtex-completion-get-key-bibtex ()
  "Return the key of the BibTeX entry at point, nil otherwise.
This function can be used by `bibtex-completion-key-at-point' to
find the key of the BibTeX entry at point in a BibTeX-mode
buffer."
  (when (eq major-mode 'bibtex-mode)
    (save-excursion
      (bibtex-beginning-of-entry)
      (and (looking-at bibtex-entry-maybe-empty-head)
           (bibtex-key-in-head)))))

(defun bibtex-completion-get-key-latex ()
  "Return the key of the BibTeX entry at point, nil otherwise.
This function can be used by `bibtex-completion-key-at-point' to
find the key of the BibTeX entry at point in a LaTeX buffer."
  (when (and (derived-mode-p 'latex-mode)
             (require 'reftex-parse nil t))
    (save-excursion
      (skip-chars-backward "[:space:],;}")
      (let ((macro (reftex-what-macro 1)))
        (and (stringp (car macro))
             (string-match "\\`\\\\cite\\|cite\\'" (car macro))
             ;; allow '_' in citekeys
             (let ((temp-syn-table (make-syntax-table)))
               (modify-syntax-entry ?_ "_" temp-syn-table)
               (with-syntax-table temp-syn-table
                 (thing-at-point 'symbol))))))))

(defun bibtex-completion-get-key-org-bibtex ()
  "Return the key of the BibTeX entry at point, nil otherwise.
This function can be used by `bibtex-completion-key-at-point' to
find the key of the BibTeX entry at point in an Org-mode buffer."
  (when (eq major-mode 'org-mode)
    (let (key)
      (and (setq key (org-entry-get nil
                                    (if (boundp 'org-bibtex-key-property)
                                        org-bibtex-key-property
                                      "CUSTOM_ID")
                                    t))
           ;; KEY may be the empty string the the property is
           ;; present but has no value
           (> (length key) 0)
           key))))

(defun bibtex-completion-get-key-org-cite ()
  "Return the org-cite key at point, nil otherwise.
This function can be used by `bibtex-completion-key-at-point' to
find the org-cite key at point in an Org-mode buffer."
  (when (eq major-mode 'org-mode)
    (let ((el (org-element-context)))
      (when (eq (car el) 'citation-reference)
        (plist-get (cadr el) :key)))))

(defvar bibtex-completion-key-at-point-functions
  (list #'bibtex-completion-get-key-bibtex
        #'bibtex-completion-get-key-latex
        #'bibtex-completion-get-key-org-bibtex
        #'bibtex-completion-get-key-org-cite)
  "List of functions to use to find the BibTeX key.
The functions should take no argument and return the BibTeX
key.  Stops as soon as a function returns something.
See `bibtex-completion-key-at-point' for details.")

(defun bibtex-completion-key-at-point ()
  "Return the key of the BibTeX entry at point.
The functions used to match the keys are defined in
`bibtex-completion-key-at-point-functions'."
  (cl-some #'identity
           (mapcar #'funcall bibtex-completion-key-at-point-functions)))

(provide 'bibtex-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bibtex-completion.el ends here
