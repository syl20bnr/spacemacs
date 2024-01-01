;;; rst-directives.el --- Build Sphinx projects.

;; Copyright (C) 2012-2024 Wei-Wei Guo.

;; Author: Wei-Wei Guo <wwguocn at gmail dot com>
;; Version: 0.1
;;
;; This file is published under the GNU Gerneral Public License,
;; see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file provides functionalities for Emacs rst-mode.


(defgroup rst-directive nil
  "Functions supporting of directives with \\[rst-directives]."
  :group 'rst
  :version "24.5")


(defun rst-directive-direct-format (type &optional argument)
  "Insert the first line of directive"
  (insert (concat ".. " type ":: " argument))
  ;; (newline-and-indent)
  (newline))

(defun rst-directive-option-format (option &optional value)
  "Insert an option line of directive"
  (insert (concat ":" option ": " value)))

(defun rst-directive-option-insert ()
  "Insert a directive option."
  (interactive)
  (let (option value)
    (save-excursion
      (re-search-backward "\\.\\. \\(\\w+\\)\\|\\(\\w+-\\w+\\)::")
      (setq option (read-string "Providing directive option: "))
      (setq value (read-string "Providing option value: "))
      (end-of-line)
      (newline)
      (insert "   ")
      ;; (newline-and-indent)
      (rst-directive-option-format option value))))

(defun rst-directive-option (optalist)
  "Insert directive options in directive inserting function."
  (let (optlist option type value)
    (setq optlist (mapcar 'car optalist))
    (setq option (completing-read "Providing option: " optlist))
    (setq type (car (cdr (assoc option optalist))))
    (setq value
          (cond
           ((equal type "flag") nil)
           ((equal type "option") (completing-read
                                   "Providing optional value: "
                                   (cadr (cdr (assoc option optalist)))))
           ((equal type "number") (number-to-string
                                   (read-number "Providing numeric value: ")))
           ((equal type "string") (read-string "Providing value: "))))
    (rst-directive-option-format option value)
    (newline)
    (insert "   ")
    ))

(defun rst-directive-insert ()
  "Meta-function of all directives."
  (interactive)
  (let (type optlist content optnum)
    (setq type (completing-read "Providing directive type: " rst-directive-types))
    (funcall (cdr (assoc type rst-directive-type-alist)))
    (setq optlist (eval (car (cdr (assoc type rst-directive-option-list)))))
    (setq content (eval (cadr (cdr (assoc type rst-directive-option-list)))))
    (if (or optlist content)
        (progn
        (insert "   ")
        (if optlist
            (while (y-or-n-p "Set directive option(s)? ")
              (rst-directive-option optlist)
              (setq optnum 't)))
        (newline)
        ;; (newline-and-indent)
        (if (and (not optnum) content)
            (insert "   "))
        (if (and optnum (not content))
              (newline)))
            ;; (progn
            ;;   (newline-and-indent)
            ;;   (delete-indentation))))
      (newline))))

;;======= Functions for adding directives by user =======

(defun rst-directive-add-type (type directfunc optalist content)
  "Adding new directive to directive alist and completion list.

Use the following way to add directive type.

  (rst-directive-add-type \"definition\"
                          'rst-directive-insert-definition
                          'rst-directive-options
                          'content-presence-boolean)
"
  (add-to-list 'rst-directive-types type)
  (add-to-list 'rst-directive-type-alist (cons type directfunc))
  (add-to-list 'rst-directive-option-list (list type optalist content)))

(defun rst-directive-append (directlist)
  "Meta function of add directives.

Elements of directives should arranged as

   (type funciton option-list content-boolean).
"
  (dolist (direct directlist)
    (eval (cons 'rst-directive-add-type direct))))


;;;;====================================
;;;;====== Directives in Docutils ======
;;;;====================================

(defvar rst-directive-types
  '("definition" "field" "admonition" "image" "figure" "topic"
    "sidebar" "line-block" "parsed-literal" "math" "rubric"
    "epigraph" "highlights" "pull-quote" "compound" "container"
    "table" "csv-table" "list-table" "contents" "sectnum"
    "replace" "unicode" "date" "include" "index" "raw")
  "List of directive types")

(defvar rst-directive-type-alist
  '(("definition" . rst-directive-insert-definition)
    ("field" . rst-directive-insert-field)
    ("admonition" . rst-directive-insert-admonition)
    ("image" . rst-directive-insert-image)
    ("figure" . rst-directive-insert-figure)
    ("topic" . rst-directive-insert-topic)
    ("sidebar" . rst-directive-insert-sidebar)
    ("line-block" . rst-directive-insert-line-block)
    ("parsed-literal" . rst-directive-insert-parsed-literal)
    ("math" . rst-directive-insert-math)
    ("rubric" . rst-directive-insert-rubric)
    ("epigraph" . rst-directive-insert-epigraph)
    ("highlights" . rst-directive-insert-highlights)
    ("pull-quote" . rst-directive-insert-pull-quote)
    ("compound" . rst-directive-insert-compound)
    ("container" . rst-directive-insert-container)
    ("table" . rst-directive-insert-table)
    ("csv-table" . rst-directive-insert-csv-table)
    ("list-table" . rst-directive-insert-list-table)
    ("contents" . rst-directive-insert-contents)
    ("sectnum" . rst-directive-insert-sectnum)
    ("replace" . rst-directive-insert-replace)
    ("unicode" . rst-directive-insert-unicode)
    ("date" . rst-directive-insert-date)
    ("include" . rst-directive-insert-include)
    ("index" . rst-directive-insert-index)
    ("raw" . rst-directive-insert-raw))
  "List of directive inserting functions of directive types.")

(defvar rst-directive-option-list
  '(("definition" rst-directive-option-definition t)
    ("field" rst-directive-option-field t)
    ("admonition" rst-directive-option-admonition nil)
    ("image" rst-directive-option-image nil)
    ("figure" rst-directive-option-figure t)
    ("topic" nil t)
    ("sidebar" rst-directive-option-sidebar t)
    ("line-block" nil t)
    ("parsed-literal" nil t)
    ("math" rst-directive-option-math t)
    ("rubric" nil nil)
    ("epigraph" nil t)
    ("highlights" nil t)
    ("pull-quote" nil t)
    ("compound" nil t)
    ("container" nil t)
    ("table" nil t)
    ("csv-table" rst-directive-option-csv-table t)
    ("list-table" rst-directive-option-list-table t)
    ("contents" rst-contents-option nil)
    ("sectnum" rst-sectnum-option nil)
    ("replace" nil nil)
    ("unicode" rst-directive-option-unicode nil)
    ("date" nil nil)
    ("include" rst-include-option nil)
    ("index" rst-directive-option-index nil)
    ("raw" rst-directive-option-raw t))
  "List of option functions of directive types.")

;;====== Directive Definitions ======

(defun rst-directive-insert-admonition ()
  "Insert a admonition."
  (interactive)
  (let (admon argu)
    (setq admon (read-string "Providing admonition type: "))
    (setq argu (read-string "Providing admonition description: "))
    (rst-directive-direct-format admon argu)))

(defun rst-directive-insert-image ()
  "Insert a image."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing image and its path: "))
    (rst-directive-direct-format "image" arg)))

(defvar rst-directive-option-image
  '(("align" "option" ("top" "middle" "bottom" "left" "center" "right"))
    ("width" "string" "300")
    ("height" "string" "300")
    ("scale" "number" 80)
    ("alt" "string" "")
    ("target" "string" "")))

(defun rst-directive-insert-figure ()
  "Insert a image."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing figure and its path: "))
    (rst-directive-direct-format "figure" arg)))

(defvar rst-directive-option-figure
  '(("align" "option" ("left" "center" "right"))
    ("width" "string" "300")
    ("height" "string" "300")
    ("scale" "number" 80)
    ("figwidth" "string" "350")
    ("alt" "string" "")
    ("target" "string" "")))

(defun rst-directive-insert-topic ()
  "Insert a topic."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing topic title: "))
    (rst-directive-direct-format "topic" arg)))

(defun rst-directive-insert-sidebar ()
  "Insert a sidebar."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing sidebar title: "))
    (rst-directive-direct-format "sidebar" arg)))

(defvar rst-directive-option-sidebar
'(("subtitle" "string" "")))

(defun rst-directive-insert-line-block ()
  "Insert a line block."
  (interactive)
  (rst-directive-direct-format "line-block"))

(defun rst-directive-insert-parsed-literal ()
  "Insert a parsed literal."
  (interactive)
  (rst-directive-direct-format "parsed-literal"))

(defun rst-directive-insert-math ()
  "Insert a math."
  (interactive)
  (rst-directive-direct-format "math"))

(defvar rst-directive-option-math
  '(("label" "flag" nil)
    ("nowrap" "flag" nil)))

(defun rst-directive-insert-rubric ()
  "Insert a rubric title."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing rubric title: "))
    (rst-directive-direct-format "rubric" arg)))

(defun rst-directive-insert-epigraph ()
  "Insert a epigraph."
  (interactive)
  (rst-directive-direct-format "epigraph"))

(defun rst-directive-insert-highlights ()
  "Insert a highlights."
  (interactive)
  (rst-directive-direct-format "highlights"))

(defun rst-directive-insert-pull-quote ()
  "Insert a pull quote."
  (interactive)
  (rst-directive-direct-format "pull-quote"))

(defun rst-directive-insert-compound ()
  "Insert a compound."
  (interactive)
  (rst-directive-direct-format "compound"))

(defun rst-directive-insert-container ()
  "Insert a container."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing container title: "))
    (rst-directive-direct-format "container" arg)))

(defun rst-directive-insert-table ()
  "Insert a table."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing table title: "))
    (rst-directive-direct-format "table" arg)))

(defun rst-directive-insert-csv-table ()
  "Insert a table."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing table title: "))
    (rst-directive-direct-format "csv-table" arg)))

(defvar rst-directive-option-csv-table
'(("widths" "string" "")
  ("header-rows" "number" 0)
  ("stub-columns" "number" 0)
  ("header" "string" "")
  ("file" "string" "")
  ("url" "string" "")
  ("encoding" "string" "")
  ("delim" "string" "")
  ("quote" "string" "")
  ("keepspace" "flag" nil)
  ("escape" "string" "")))

(defun rst-directive-insert-list-table ()
  "Insert a table."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing table title: "))
    (rst-directive-direct-format "list-table" arg)))

(defvar rst-directive-option-list-table
'(("widths" "string" "")
  ("header-rows" "number" 0)
  ("stub-columns" "number" 0)))

(defun rst-directive-insert-contents ()
  "Insert a contents."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing contents title: "))
    (rst-directive-direct-format "contents" arg)))

(defvar rst-directive-option-contents
'(("backlinks" "option" ("entry" "top" "none"))
  ("depth" "number" 2)
  ("local" "flag" nil)))

(defun rst-directive-insert-sectnum ()
  "Set section-autonumbering."
  (interactive)
  (rst-directive-direct-format "sectnum"))

(defvar rst-directive-option-sectnum
'(("prefix" "string" "")
  ("suffix" "string" "")
  ("depth" "number" 2)
  ("start" "number" 1)))

(defun rst-directive-insert-replace ()
  "Insert the head of word replace."
  (interactive)
  (let (tag)
    (setq tag (read-string "Providing replaced word: "))
    (rst-directive-direct-format (concat "|" tag "| replace"))))

(defun rst-directive-insert-unicode ()
  "Insert unicode replacement."
  (interactive)
  (let (tag)
    (setq tag (read-string "Providing unicode string: "))
    (rst-directive-direct-format (concat "|" tag "| unicode"))))

(defvar rst-directive-option-unicode
  '(("ltrim" "flag" nil)
    ("rtrim" "flag" nil)
    ("trim" "flag" nil)))

(defun rst-directive-insert-date ()
  "Insert date or time."
  (interactive)
  (let (type value)
    (setq type (completing-read "Insert date or time? " '("date" "time")))
    (cond
     ((equal type "date")
      (setq value (read-string "Providing date format: " nil nil "%Y-%m-%d")))
     ((equal type "time")
      (setq value (read-string "Providing time format: " nil nil "%H:%M"))))
    (rst-directive-direct-format (concat "|" type "| date") value)))

(defun rst-directive-insert-include ()
  "Insert an external file."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing file path: "))
    (rst-directive-direct-format "include" arg)))

(defvar rst-directive-option-include
  '(("start-after" "string" "")
    ("end-before" "string" "")
    ("encoding" "string" "")
    ("literal" "flag" nil)))

(defun rst-directive-insert-index ()
  "Insert an index entry."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing index entry: "))
    (rst-directive-direct-format "index" arg)))

(defvar rst-directive-option-index
  '(("single" "string" "")
    ("pair" "string" "")
    ("triple" "string" "")
    ("see" "string" "")
    ("seealso" "string" "")))

(defun rst-directive-insert-raw ()
  "Insert raw data."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing raw data type: "))
    (rst-directive-direct-format "raw" arg)))

(defvar rst-directive-option-raw
  '(("file" "string" "")
    ("url" "string" "")
    ("encoding" "string" "")))

;;======= Directive-like definitions listed in Docutils =======

(defun rst-directive-insert-definition ()
  "Insert a definition list"
  (interactive)
  (let (term classifiers classel)
    (setq term (read-string "Providing the definition's term: "))
    (setq classifiers (read-string "Providing classifier(s) (if many, seperated by ', '): "))
    (if (equal classifiers "")
        (insert term "\n    ")
      (progn
        (setq classifiers (split-string classifiers ", "))
        (dolist (tmpclass classifiers)
          (setq classel (concat classel " : " tmpclass)))
        (insert term classel "\n    ")))))

(defun rst-directive-insert-field ()
  "Insert a field list."
  (interactive)
  (let (field value)
    (setq field (read-string "Providing field: "))
    (save-excursion
      (beginning-of-line)
      (insert (concat ":" field ": ")))))


;;;;==================================
;;;;====== Directives in Sphinx ======
;;;;==================================

(rst-directive-append
 '(("note" 'rst-directive-insert-note 'nil 't)
   ("glossary" 'rst-directive-insert-glossary 'nil 't)
   ("centered" 'rst-directive-insert-centered 'nil 'nil)
   ("hlist" 'rst-directive-insert-hlist 'rst-directive-option-hlist 't)
   ("tabularcolumns" 'rst-directive-insert-tabularcolumns 'nil 't)
   ;; showing code
   ("highlight" 'rst-directive-insert-highlight 'rst-directive-option-highlight 'nil)
   ("code-block" 'rst-directive-insert-code-block 'rst-directive-option-code-block 't)
   ;; sphinx.ext.doctest
   ("testcode" 'rst-directive-insert-testcode 'rst-directive-option-testcode 't)
   ("testoutput" 'rst-directive-insert-testoutput 'rst-directive-option-testoutput 't)
   ;; sphinx.ext.graphviz
   ("graph" 'rst-directive-insert-graph 'nil 't)
   ("graphviz" 'rst-directive-insert-graphviz 'nil 't)
   ("digraph" 'rst-directive-insert-digraph 'nil 't)
   ;; sphinx.ext.todo
   ("todo" 'rst-directive-insert-todo 'nil 't)
   ("todolist" 'rst-directive-insert-todolist 'nil 'nil)
   ))

(defun rst-directive-insert-note ()
  "Insert a note block."
  (interactive)
  (rst-directive-direct-format "note"))

(defun rst-directive-insert-glossary ()
  "Insert a glossary block."
  (interactive)
  (rst-directive-direct-format "glossary"))

(defun rst-directive-insert-centered ()
  "Insert a centered title."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing centered title: "))
    (rst-directive-direct-format "centered" arg)))

(defun rst-directive-insert-hlist ()
  "Insert a compact list."
  (interactive)
  (rst-directive-direct-format "hlist"))

(defvar rst-directive-option-hlist
  '(("columns" "number" 2)))

(defun rst-directive-insert-tabularcolumns ()
  "Insert a table that can specify columns like in LaTeX."
  (interactive)
  (rst-directive-direct-format "tabularcolumns"))

(defun rst-directive-insert-highlight ()
  "Insert a image."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing lexer type: "))
    (rst-directive-direct-format "highlight" arg)))

(defvar rst-directive-option-highlight
  '(("linenothreshold" "number" 5)))

(defun rst-directive-insert-code-block ()
  "Insert a code block."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing lexer type: "))
    (rst-directive-direct-format "code-block" arg)))

(defvar rst-directive-option-code-block
  '(("linenos" "flag" nil)))

(defun rst-directive-insert-testcode ()
  "Insert a testcode."
  (interactive)
  (rst-directive-direct-format "testcode"))

(defvar rst-directive-option-testcode
  '(("hide" "flag" nil)))

(defun rst-directive-insert-testoutput ()
  "Insert a testoutput."
  (interactive)
  (rst-directive-direct-format "testoutput"))

(defvar rst-directive-option-testoutput
  '(("hide" "flag" nil)
    ("options" "string" "")))

(defun rst-directive-insert-graphviz ()
  "Insert a graphviz block."
  (interactive)
  (rst-directive-direct-format "graphviz"))

(defun rst-directive-insert-graph ()
  "Insert a graphviz single graph."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing graph title: "))
    (rst-directive-direct-format "graph" arg)))

(defun rst-directive-insert-digraph ()
  "Insert a digraph block."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing digraph title: "))
    (rst-directive-direct-format "digraph" arg)))

(defun rst-directive-insert-todo ()
  "Insert a todo block."
  (interactive)
  (rst-directive-direct-format "todo"))

(defun rst-directive-insert-todolist ()
  "Insert a todolist block."
  (interactive)
  (rst-directive-direct-format "todolist"))


;;;;=======================================
;;;;====== Directives by Wei-Wei Guo ======
;;;;=======================================

(rst-directive-append
 '(("blog" 'rst-directive-insert-blog 'rst-directive-option-blog 'nil)
   ("lilypond" 'rst-directive-insert-lilypond 'rst-directive-option-lilypond 't)
   ("metapost" 'rst-directive-insert-metapost 'rst-directive-option-metapost 't)
   ("tikz" 'rst-directive-insert-tikz 'rst-directive-option-tikz 't)
   ("tex-table" 'rst-directive-insert-tex-table 'rst-directive-option-tex-table 't)
   ("algorithm" 'rst-directive-insert-algorithm 'rst-directive-option-algorithm 't)
   ))

(defun rst-directive-insert-blog ()
  "Insert a blog block."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing blog title: "))
    (rst-directive-direct-format "blog" arg)))

(defvar rst-directive-option-blog
  '(("date" "string" nil)
    ("source" "string" "")))

(defun rst-directive-insert-lilypond ()
  "Insert a lilypond block."
  (interactive)
  (rst-directive-direct-format "lilypond"))

(defvar rst-directive-option-lilypond
  '(("align" "option" ("top" "middle" "bottom" "left" "center" "right"))
    ("width" "string" "300")
    ("height" "string" "300")
    ("scale" "number" 80)
    ("nowrap" "flag" nil)))

(defun rst-directive-insert-metapost ()
  "Insert a metapost block."
  (interactive)
  (rst-directive-direct-format "metapost"))

(defvar rst-directive-option-metapost
  '(("align" "option" ("top" "middle" "bottom" "left" "center" "right"))
    ("width" "string" "300")
    ("height" "string" "300")
    ("scale" "number" 80)
    ("quality" "number" 300)
    ("nowrap" "flag" nil)))

(defun rst-directive-insert-tikz ()
  "Insert a metapost block."
  (interactive)
  (rst-directive-direct-format "displaytikz"))

(defvar rst-directive-option-tikz
  '(("align" "option" ("top" "middle" "bottom" "left" "center" "right"))
    ("width" "string" "300")
    ("height" "string" "300")
    ("scale" "number" 80)
    ("quality" "number" 300)
    ("options" "string" "")
    ("nowrap" "flag" nil)))

(defun rst-directive-insert-tex-table ()
  "Insert a LaTeX table."
  (interactive)
  (rst-directive-direct-format "tex-table"))

(defvar rst-directive-option-tex-table
  '(("align" "option" ("top" "middle" "bottom" "left" "center" "right"))
    ("width" "string" "300")
    ("height" "string" "300")
    ("scale" "number" 80)
    ("quality" "number" 300)
    ("place" "string" "\centering")
    ("talign" "string" "")
    ("nowrap" "flag" nil)))

(defun rst-directive-insert-algorithm ()
  "Insert algorithm codes in LaTeX ."
  (interactive)
  (let (arg)
    (setq arg (read-string "Providing caption: "))
    (rst-directive-direct-format "algorithm" arg)))

(defvar rst-directive-option-algorithm
  '(("align" "option" ("top" "middle" "bottom" "left" "center" "right"))
    ("width" "string" "300")
    ("height" "string" "300")
    ("scale" "number" 80)
    ("quality" "number" 300)))


(provide 'rst-directives)
