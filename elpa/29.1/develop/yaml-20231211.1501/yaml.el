;;; yaml.el --- YAML parser for Elisp -*- lexical-binding: t -*-

;; Copyright © 2021 Zachary Romero <zkry@posteo.org>

;; Author: Zachary Romero <zkry@posteo.org>
;; Version: 0.5.5
;; Homepage: https://github.com/zkry/yaml.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools

;; yaml.el requires at least GNU Emacs 25.1

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; yaml.el contains the code for parsing YAML natively in Elisp with
;; no dependencies.  The main function to parse YAML provided is
;; `yaml-parse-string'.  `yaml-encode' is also provided to encode a
;; Lisp object to YAML.  The following are some examples of its usage:
;;
;; (yaml-parse-string "key1: value1\nkey2: value2")
;; (yaml-parse-string "key1: value1\nkey2: value2" :object-type 'alist)
;; (yaml-parse-string "numbers: [1, 2, 3]" :sequence-type 'list)
;;
;; (yaml-encode '((count . 3) (value . 10) (items ("ruby" "diamond"))))

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)

(defconst yaml-parser-version "0.5.1")

(defvar yaml--parse-debug nil
  "Turn on debugging messages when parsing YAML when non-nil.

This flag is intended for development purposes.")

(defconst yaml--tracing-ignore '("s-space"
                                 "s-tab"
                                 "s-white"
                                 "l-comment"
                                 "b-break"
                                 "b-line-feed"
                                 "b-carriage-return"
                                 "s-b-comment"
                                 "b-comment"
                                 "l-comment"
                                 "ns-char"
                                 "nb-char"
                                 "b-char"
                                 "c-printable"
                                 "b-as-space"))

(defvar yaml--parsing-input ""
  "The string content of the current item being processed.")
(defvar yaml--parsing-position 0
  "The position that the parser is currently looking at.")
(defvar yaml--states nil
  "Stack of parsing states.")

(defvar yaml--parsing-object-type nil)
(defvar yaml--parsing-object-key-type nil)
(defvar yaml--parsing-sequence-type nil)
(defvar yaml--parsing-null-object nil)
(defvar yaml--parsing-false-object nil)
(defvar yaml--parsing-store-position nil)
(defvar yaml--string-values nil)

(cl-defstruct (yaml--state (:constructor yaml--state-create)
                           (:copier nil))
  doc tt m name lvl beg end)

(defmacro yaml--parse (data &rest forms)
  "Parse DATA according to FORMS."
  (declare (indent defun))
  `(progn (setq yaml--parsing-input ,data)
          (setq yaml--parsing-position 0)
          (yaml--initialize-state)
          ,@forms))

(defun yaml--state-curr ()
  "Return the current state."
  (or (car yaml--states)
      (yaml--state-create
       :name nil :doc nil :lvl 0 :beg 0 :end 0 :m nil :tt nil)))

(defun yaml--state-set-m (val)
  "Set the current value of t to VAL."
  (let* ((states yaml--states))
    (while states
      (let* ((top-state (car states))
             (new-state (yaml--state-create :doc (yaml--state-doc top-state)
                                            :tt (yaml--state-tt top-state)
                                            :m val
                                            :name (yaml--state-name top-state)
                                            :lvl (yaml--state-lvl top-state)
                                            :beg (yaml--state-beg top-state)
                                            :end (yaml--state-end top-state))))
        (setcar states new-state))
      (setq states (cdr states)))))

(defun yaml--state-set-t (val)
  "Set the current value of t to VAL."
  (let* ((states yaml--states))
    (while states
      (let* ((top-state (car states))
             (new-state (yaml--state-create :doc (yaml--state-doc top-state)
                                            :tt val
                                            :m (yaml--state-m top-state)
                                            :name (yaml--state-name top-state)
                                            :lvl (yaml--state-lvl top-state)
                                            :beg (yaml--state-beg top-state)
                                            :end (yaml--state-end top-state))))
        (setcar states new-state))
      (setq states (cdr states)))))

(defun yaml--state-curr-doc ()
  "Return the doc property of current state."
  (yaml--state-doc (yaml--state-curr)))

(defun yaml--state-curr-t ()
  "Return the doc property of current state."
  (yaml--state-tt (yaml--state-curr)))

(defun yaml--state-curr-m ()
  "Return the doc property of current state."
  (or (yaml--state-m (yaml--state-curr)) 1))

(defun yaml--state-curr-end ()
  "Return the doc property of current state."
  (yaml--state-end (yaml--state-curr)))

(defun yaml--push-state (name)
  "Add a new state frame with NAME."
  (let* ((curr-state (yaml--state-curr))
         (new-state (yaml--state-create
                     :doc (yaml--state-curr-doc)
                     :tt (yaml--state-curr-t)
                     :m (yaml--state-curr-m)
                     :name name
                     :lvl (1+ (yaml--state-lvl curr-state))
                     :beg yaml--parsing-position
                     :end nil)))
    (push new-state yaml--states)))

(defun yaml--pop-state ()
  "Pop the current state."
  (let ((popped-state (car yaml--states)))
    (setq yaml--states (cdr yaml--states))
    (let ((top-state (car yaml--states)))
      (when top-state
        (setcar yaml--states
                (yaml--state-create :doc (yaml--state-doc top-state)
                                    :tt (yaml--state-tt top-state)
                                    :m (yaml--state-m top-state)
                                    :name (yaml--state-name top-state)
                                    :lvl (yaml--state-lvl top-state)
                                    :beg (yaml--state-beg popped-state)
                                    :end yaml--parsing-position))))))

(defun yaml--initialize-state ()
  "Initialize the yaml state for parsing."
  (setq yaml--states
        (list (yaml--state-create :doc nil
                                  :tt nil
                                  :m nil
                                  :name nil
                                  :lvl 0
                                  :beg nil
                                  :end nil))))

(defconst yaml--grammar-resolution-rules
  '(("ns-plain" . literal))
  "Alist determining how to resolve grammar rule.")

;;; Receiver Functions

(defvar yaml--document-start-version nil)
(defvar yaml--document-start-explicit nil)
(defvar yaml--document-end-explicit nil)
(defvar yaml--tag-map nil)
(defvar yaml--tag-handle nil)
(defvar yaml--document-end nil)

(defvar yaml--cache nil
  "Stack of data for temporary calculations.")
(defvar yaml--object-stack nil
  "Stack of objects currently being build.")
(defvar yaml--state-stack nil
  "The state that the YAML parser is with regards to incoming events.")
(defvar yaml--root nil)

(defvar yaml--anchor-mappings nil
  "Hashmap containing the anchor mappings of the current parsing run.")
(defvar yaml--resolve-aliases nil
  "Flag determining if the event processing should attempt to resolve aliases.")

(defun yaml--parse-block-header (header)
  "Parse the HEADER string returning chomping style and indent count."
  (let* ((pos 0)
         (chomp-indicator :clip)
         (indentation-indicator nil)
         (char (and (< pos (length header)) (aref header pos)))
         (process-char (lambda (char)
                         (when char
                           (cond
                            ((< ?0 char ?9)
                             (progn (setq indentation-indicator (- char ?0))))
                            ((equal char ?\-) (setq chomp-indicator :strip))
                            ((equal char ?\+) (setq chomp-indicator :keep)))
                           (setq pos (1+ pos))))))
    (when (or (eq char ?\|) (eq char ?\>))
      (setq pos (1+ pos))
      (setq char (and (< pos (length header)) (aref header pos))))
    (funcall process-char char)
    (let ((char (and (< pos (length header)) (aref header pos)))) ;
      (funcall process-char char)
      (list chomp-indicator indentation-indicator))))

(defun yaml--chomp-text (text-body chomp)
  "Change the ending newline of TEXT-BODY based on CHOMP."
  (cond ((eq :clip chomp)
         (concat (replace-regexp-in-string "\n*\\'" "" text-body) "\n"))
        ((eq :strip chomp)
         (replace-regexp-in-string "\n*\\'" "" text-body))
        ((eq :keep chomp)
         text-body)))

(defun yaml--process-folded-text (text)
  "Remove the header line for a folded match and return TEXT body formatted."
  (let* ((text (yaml--process-literal-text text))
         (done))
    (while (not done)
      (let ((replaced (replace-regexp-in-string "\\([^\n]\\)\n\\([^\n ]\\)"
                                                "\\1 \\2"
                                                text)))
        (when (equal replaced text)
          (setq done t))
        (setq text replaced)))
    (replace-regexp-in-string
     "\\(\\(?:^\\|\n\\)[^ \n][^\n]*\\)\n\\(\n+\\)\\([^\n ]\\)" "\\1\\2\\3"
     text)))

(defun yaml--process-literal-text (text)
  "Remove the header line for a folded match and return TEXT body formatted."
  (let ((n (get-text-property 0 'yaml-n text)))
    (remove-text-properties 0 (length text) '(yaml-n nil) text)
    (let* ((header-line (substring text 0 (string-match "\n" text)))
           (text-body (substring text (1+ (string-match "\n" text))))
           (parsed-header (yaml--parse-block-header header-line))
           (chomp (car parsed-header))
           (starting-spaces-ct
            (or (and (cadr parsed-header) (+ (or n 0) (cadr parsed-header)))
                (let ((_ (string-match "^\n*\\( *\\)" text-body)))
                  (length (match-string 1 text-body)))))
           (lines (split-string text-body "\n"))
           (striped-lines
            (seq-map (lambda (l)
                       (replace-regexp-in-string
                        (format "\\` \\{0,%d\\}" starting-spaces-ct) "" l))
                     lines))
           (text-body (string-join striped-lines "\n")))
      (yaml--chomp-text text-body chomp))))

;; TODO: Process tags and use them in this function.
(defun yaml--resolve-scalar-tag (scalar)
  "Convert a SCALAR string to it's corresponding object."
  (cond
   (yaml--string-values
    scalar)
   ;; tag:yaml.org,2002:null
   ((or (equal "null" scalar)
        (equal "Null" scalar)
        (equal "NULL" scalar)
        (equal "~" scalar))
    yaml--parsing-null-object)
   ;; tag:yaml.org,2002:bool
   ((or (equal "true" scalar)
        (equal "True" scalar)
        (equal "TRUE" scalar)) t)
   ((or (equal "false" scalar)
        (equal "False" scalar)
        (equal "FALSE" scalar))
    yaml--parsing-false-object)
   ;; tag:yaml.org,2002:int
   ((string-match "^0$\\|^-?[1-9][0-9]*$" scalar)
    (string-to-number scalar))
   ((string-match "^[-+]?[0-9]+$" scalar)
    (string-to-number scalar))
   ((string-match "^0o[0-7]+$" scalar)
    (string-to-number (substring scalar 2) 8))
   ((string-match "^0x[0-9a-fA-F]+$" scalar)
    (string-to-number (substring scalar 2) 16))
   ;; tag:yaml.org,2002:float
   ((string-match
     "^[-+]?\\(\\.[0-9]+\\|[0-9]+\\(\\.[0-9]*\\)?\\)\\([eE][-+]?[0-9]+\\)?$"
     scalar)
    (string-to-number scalar 10))
   ((string-match "^[-+]?\\(\\.inf\\|\\.Inf\\|\\.INF\\)$" scalar)
    1.0e+INF)
   ((string-match "^[-+]?\\(\\.nan\\|\\.NaN\\|\\.NAN\\)$" scalar)
    1.0e+INF)
   ((string-match "^0$\\|^-?[1-9]\\(\\.[0-9]*\\)?\\(e[-+][1-9][0-9]*\\)?$"
                  scalar)
    (string-to-number scalar))
   (t scalar)))

(defun yaml--hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to a alist."
  (let ((alist nil))
    (maphash
     (lambda (k v)
       (setq alist (cons (cons k v) alist)))
     hash-table)
    alist))

(defun yaml--hash-table-to-plist (hash-table)
  "Convert HASH-TABLE to a plist."
  (let ((plist nil))
    (maphash
     (lambda (k v)
       (setq plist (cons k (cons v plist))))
     hash-table)
    plist))

(defun yaml--format-object (hash-table)
  "Convert HASH-TABLE to alist of plist if specified."
  (cond
   ((equal yaml--parsing-object-type 'hash-table)
    hash-table)
   ((equal yaml--parsing-object-type 'alist)
    (yaml--hash-table-to-alist hash-table))
   ((equal yaml--parsing-object-type 'plist)
    (yaml--hash-table-to-plist hash-table))
   (t hash-table)))

(defun yaml--format-list (l)
  "Convert L to array if specified."
  (cond
   ((equal yaml--parsing-sequence-type 'list)
    l)
   ((equal yaml--parsing-sequence-type 'array)
    (apply #'vector l))
   (t l)))

(defun yaml--stream-start-event ()
  "Create the data for a stream-start event."
  '(:stream-start))

(defun yaml--stream-end-event ()
  "Create the data for a stream-end event."
  '(:stream-end))

(defun yaml--mapping-start-event (_)
  "Process event indicating start of mapping."
  ;; NOTE: currently don't have a use for FLOW
  (push :mapping yaml--state-stack)
  (push (make-hash-table :test 'equal) yaml--object-stack))

(defun yaml--mapping-end-event ()
  "Process event indicating end of mapping."
  (pop yaml--state-stack)
  (let ((obj (pop yaml--object-stack)))
    (yaml--scalar-event nil obj))
  '(:mapping-end))

(defun yaml--sequence-start-event (_)
  "Process event indicating start of sequence according to FLOW."
  ;; NOTE: currently don't have a use for FLOW
  (push :sequence yaml--state-stack)
  (push nil yaml--object-stack)
  '(:sequence-start))

(defun yaml--sequence-end-event ()
  "Process event indicating end of sequence."
  (pop yaml--state-stack)
  (let ((obj (pop yaml--object-stack)))
    (yaml--scalar-event nil obj))
  '(:sequence-end))

(defun yaml--anchor-event (name)
  "Process event indicating an anchor has been defined with NAME."
  (push :anchor yaml--state-stack)
  (push `(:anchor ,name) yaml--object-stack))

(defun yaml--scalar-event (style value)
  "Process the completion of a scalar VALUE.

 Note that VALUE may be a complex object here.  STYLE is
 currently unused."
  (let ((top-state (car yaml--state-stack))
        (value* (cond
                 ((stringp value) (yaml--resolve-scalar-tag value))
                 ((listp value) (yaml--format-list value))
                 ((hash-table-p value) (yaml--format-object value))
                 ((vectorp value) value)
                 ((not value) nil))))
    (cond
     ((not top-state)
      (setq yaml--root value*))
     ((equal top-state :anchor)
      (let* ((anchor (pop yaml--object-stack))
             (name (nth 1 anchor)))
        (puthash name value yaml--anchor-mappings)
        (pop yaml--state-stack)
        (yaml--scalar-event nil value)))
     ((equal top-state :sequence)
      (let ((l (car yaml--object-stack)))
        (setcar yaml--object-stack (append l (list value*)))))
     ((equal top-state :mapping)
      (progn
        (push :mapping-value yaml--state-stack)
        (push value* yaml--cache)))
     ((equal top-state :mapping-value)
      (progn
        (let ((key (pop yaml--cache))
              (table (car yaml--object-stack)))
          (when (stringp key)
            (cond
             ((eql 'symbol yaml--parsing-object-key-type)
              (setq key (intern key)))
             ((eql 'keyword yaml--parsing-object-key-type)
              (setq key (intern (format ":%s" key))))))
          (puthash key value* table))
        (pop yaml--state-stack)))
     ((equal top-state :trail-comments)
      (pop yaml--state-stack)
      (let ((comment-text (pop yaml--object-stack)))
        (unless (stringp value*)
          (error "Trail-comments can't be nested under non-string"))
        (yaml--scalar-event
         style
         (replace-regexp-in-string (concat (regexp-quote comment-text) "\n*\\'")
                                   ""
                                   value*))))
     ((equal top-state nil))))
  '(:scalar))

(defun yaml--alias-event (name)
  "Process a node has been defined via alias NAME."
  (if yaml--resolve-aliases
      (let ((resolved (gethash name yaml--anchor-mappings)))
        (unless resolved (error "Undefined alias '%s'" name))
        (yaml--scalar-event nil resolved))
    (yaml--scalar-event nil (vector :alias name)))
  '(:alias))

(defun yaml--trail-comments-event (text)
  "Process trailing comments of TEXT which should be trimmed from parent."
  (push :trail-comments yaml--state-stack)
  (push text yaml--object-stack)
  '(:trail-comments))



(defun yaml--check-document-end ()
  "Return non-nil if at end of document."
  ;; NOTE: currently no need for this.  May be needed in the future.
  t)

(defun yaml--reverse-at-list ()
  "Reverse the list at the top of the object stack.

This is needed to get the correct order as lists are processed in
reverse order."
  (setcar yaml--object-stack (reverse (car yaml--object-stack))))

(defconst yaml--grammar-events-in
  '(("l-yaml-stream" . (lambda ()
                         (yaml--stream-start-event)
                         (setq yaml--document-start-version nil)
                         (setq yaml--document-start-explicit nil)
                         (setq yaml--tag-map (make-hash-table))))
    ("c-flow-mapping" . (lambda ()
                          (yaml--mapping-start-event t)))
    ("c-flow-sequence" . (lambda ()
                           (yaml--sequence-start-event nil)))
    ("l+block-mapping" . (lambda ()
                           (yaml--mapping-start-event nil)))
    ("l+block-sequence" . (lambda ()
                            (yaml--sequence-start-event nil)))
    ("ns-l-compact-mapping" . (lambda ()
                                (yaml--mapping-start-event nil)))
    ("ns-l-compact-sequence" . (lambda ()
                                 (yaml--sequence-start-event nil)))
    ("ns-flow-pair" . (lambda ()
                        (yaml--mapping-start-event t)))
    ("ns-l-block-map-implicit-entry" . (lambda ()))
    ("ns-l-compact-mapping" . (lambda ()))
    ("c-l-block-seq-entry" . (lambda ())))
  "List of functions for matched rules that run on the entering of a rule.")

(defconst yaml--grammar-events-out
  '(("c-b-block-header" .
     (lambda (text)
       nil))
    ("l-yaml-stream" .
     (lambda (text)
       (yaml--check-document-end)
       (yaml--stream-end-event)))
    ("ns-yaml-version" .
     (lambda (text)
       (when yaml--document-start-version
         (throw 'error "Multiple %YAML directives not allowed."))
       (setq yaml--document-start-version text)))
    ("c-tag-handle" .
     (lambda (text)
       (setq yaml--tag-handle text)))
    ("ns-tag-prefix" .
     (lambda (text)
       (puthash yaml--tag-handle text yaml--tag-map)))
    ("c-directives-end" .
     (lambda (text)
       (yaml--check-document-end)
       (setq yaml--document-start-explicit t)))
    ("c-document-end" .
     (lambda (text)
       (when (not yaml--document-end)
         (setq yaml--document-end-explicit t))
       (yaml--check-document-end)))
    ("c-flow-mapping" .
     (lambda (text)
       (yaml--mapping-end-event)))
    ("c-flow-sequence" .
     (lambda (text)
       (yaml--sequence-end-event )))
    ("l+block-mapping" .
     (lambda (text)
       (yaml--mapping-end-event)))
    ("l+block-sequence" .
     (lambda (text)
       (yaml--reverse-at-list)
       (yaml--sequence-end-event)))
    ("ns-l-compact-mapping" .
     (lambda (text)
       (yaml--mapping-end-event)))
    ("ns-l-compact-sequence" .
     (lambda (text)
       (yaml--sequence-end-event)))
    ("ns-flow-pair" .
     (lambda (text)
       (yaml--mapping-end-event)))
    ("ns-plain" .
     (lambda (text)
       (let* ((replaced (if (and (zerop (length yaml--state-stack))
                                 (string-match "\\(^\\|\n\\)\\.\\.\\.\\'" text))
                            ;; Hack to not send the document parse end.
                            ;; Will only occur with bare ns-plain at top level.
                            (replace-regexp-in-string "\\(^\\|\n\\)\\.\\.\\.\\'"
                                                      ""
                                                      text)
                          text))
              (replaced (replace-regexp-in-string
                         "\\(?:[ \t]*\r?\n[ \t]*\\)"
                         "\n"
                         replaced))
              (replaced (replace-regexp-in-string
                         "\\(\n\\)\\(\n*\\)"
                         (lambda (x)
                           (if (> (length x) 1)
                               (substring x 1)
                             " "))
                         replaced)))
         (yaml--scalar-event "plain" replaced))))
    ("c-single-quoted" .
     (lambda (text)
       (let* ((replaced (replace-regexp-in-string
                         "\\(?:[ \t]*\r?\n[ \t]*\\)"
                         "\n"
                         text))
              (replaced (replace-regexp-in-string
                         "\\(\n\\)\\(\n*\\)"
                         (lambda (x)
                           (if (> (length x) 1)
                               (substring x 1)
                             " "))
                         replaced))
              (replaced (if (not (equal "''" replaced))
                            (replace-regexp-in-string
                             "''"
                             (lambda (x)
                               (if (> (length x) 1)
                                   (substring x 1)
                                 "'"))
                             replaced)
                          replaced)))
         (yaml--scalar-event "single"
                             (substring replaced 1 (1- (length replaced)))))))
    ("c-double-quoted" .
     (lambda (text)
       (let* ((replaced (replace-regexp-in-string
                         "\\(?:[ \t]*\r?\n[ \t]*\\)"
                         "\n"
                         text))
              (replaced (replace-regexp-in-string
                         "\\(\n\\)\\(\n*\\)"
                         (lambda (x)
                           (if (> (length x) 1)
                               (substring x 1)
                             " "))
                         replaced))
              (replaced (replace-regexp-in-string "\\\\\\([\"\\/]\\)"
                                                  "\\1"
                                                  replaced))
              (replaced (replace-regexp-in-string "\\\\ " " " replaced))
              (replaced (replace-regexp-in-string "\\\\ " " " replaced))
              (replaced (replace-regexp-in-string "\\\\b" "\b" replaced))
              (replaced (replace-regexp-in-string "\\\\t" "\t" replaced))
              (replaced (replace-regexp-in-string "\\\\n" "\n" replaced))
              (replaced (replace-regexp-in-string "\\\\r" "\r" replaced))
              (replaced (replace-regexp-in-string "\\\\r" "\r" replaced))
              (replaced (replace-regexp-in-string
                         "\\\\x\\([0-9a-fA-F]\\{2\\}\\)"
                         (lambda (x)
                           (let ((char-pt (substring 2 x)))
                             (string (string-to-number char-pt 16))))
                         replaced))
              (replaced (replace-regexp-in-string
                         "\\\\x\\([0-9a-fA-F]\\{2\\}\\)"
                         (lambda (x)
                           (let ((char-pt (substring x 2)))
                             (string (string-to-number char-pt 16))))
                         replaced))
              (replaced (replace-regexp-in-string
                         "\\\\x\\([0-9a-fA-F]\\{4\\}\\)"
                         (lambda (x)
                           (let ((char-pt (substring x 2)))
                             (string (string-to-number char-pt 16))))
                         replaced))
              (replaced (replace-regexp-in-string
                         "\\\\x\\([0-9a-fA-F]\\{8\\}\\)"
                         (lambda (x)
                           (let ((char-pt (substring x 2)))
                             (string (string-to-number char-pt 16))))
                         replaced))
              (replaced (replace-regexp-in-string
                         "\\\\\\\\"
                         "\\"
                         replaced))
              (replaced (substring replaced 1 (1- (length replaced)))))
         (yaml--scalar-event "double" replaced))))
    ("c-l+literal" .
     (lambda (text)
       (when (equal (car yaml--state-stack) :trail-comments)
         (pop yaml--state-stack)
         (let ((comment-text (pop yaml--object-stack)))
           (setq text (replace-regexp-in-string
                       (concat (regexp-quote comment-text) "\n*\\'") "" text))))
       (let* ((processed-text (yaml--process-literal-text text)))
         (yaml--scalar-event "folded" processed-text))))
    ("c-l+folded" .
     (lambda (text)
       (when (equal (car yaml--state-stack) :trail-comments)
         (pop yaml--state-stack)
         (let ((comment-text (pop yaml--object-stack)))
           (setq text (replace-regexp-in-string
                       (concat (regexp-quote comment-text) "\n*\\'") "" text))))
       (let* ((processed-text (yaml--process-folded-text text)))
         (yaml--scalar-event "folded" processed-text))))
    ("e-scalar" .
     (lambda (text)
       (yaml--scalar-event "plain" "null")))
    ("c-ns-anchor-property" .
     (lambda (text)
       (yaml--anchor-event (substring text 1))))
    ("c-ns-tag-property" .
     (lambda (text)
       ;; TODO: Implement tags
       ))
    ("l-trail-comments" .
     (lambda (text)
       (yaml--trail-comments-event text)))
    ("c-ns-alias-node" .
     (lambda (text)
       (yaml--alias-event (substring text 1)))))
  "List of functions for matched rules that run on the exiting of a rule.")

(defconst yaml--terminal-rules
  '( "l-nb-literal-text"
     "l-nb-diff-lines"
     "ns-plain"
     "c-single-quoted"
     "c-double-quoted")
  "List of rules that indicate at which the parse tree should stop.

This addition is a hack to prevent the parse tree from going too deep and thus
risk hitting the stack depth limit.  Each of these rules are recursive and
repeat for each character in a text.")

(defun yaml--walk-events (tree)
  "Event walker iterates over the parse TREE and signals events from the rules."
  (when (consp tree)
    (if (stringp (car tree))
        (let ((grammar-rule (car tree))
              (text (cadr tree))
              (children (cl-caddr tree)))
          (let ((in-fn (cdr (assoc grammar-rule yaml--grammar-events-in)))
                (out-fn (cdr (assoc grammar-rule yaml--grammar-events-out))))
            (when in-fn
              (funcall in-fn))
            (yaml--walk-events children)
            (when out-fn
              (funcall out-fn text))))
      (yaml--walk-events (car tree))
      (yaml--walk-events (cdr tree)))))

(defmacro yaml--frame (name rule)
  "Add a new state frame of NAME for RULE."
  (declare (indent defun))
  (let ((res-symbol (make-symbol "res")))
    `(let ((beg yaml--parsing-position)
           (_ (when (and yaml--parse-debug
                         (not (member ,name yaml--tracing-ignore)))
                (message "|%s>%s %40s args=%s '%s'"
                         (make-string (length yaml--states) ?-)
                         (make-string (- 70 (length yaml--states)) ?\s)
                         ,name
                         args
                         (replace-regexp-in-string
                          "\n"
                          "↓"
                          (yaml--slice yaml--parsing-position)))))
           (_ (yaml--push-state ,name))
           (,res-symbol ,rule))
       (when (and yaml--parse-debug
                  ,res-symbol
                  (not (member ,name yaml--tracing-ignore)))
         (message "<%s|%s %40s = '%s'"
                  (make-string (length yaml--states) ?-)
                  (make-string (- 70 (length yaml--states)) ?\s)
                  ,name
                  (replace-regexp-in-string
                   "\n"
                   "↓"
                   (substring yaml--parsing-input beg yaml--parsing-position))))
       (yaml--pop-state)
       (if (not ,res-symbol)
           nil
         (let ((res-type (cdr (assoc ,name yaml--grammar-resolution-rules)))
               (,res-symbol (if (member ,name yaml--terminal-rules)
                                ;; Ignore children if at-rule is
                                ;; indicated to be terminal.
                                t
                              ,res-symbol)))
           (cond
            ((or (assoc ,name yaml--grammar-events-in)
                 (assoc ,name yaml--grammar-events-out))
             (let ((str (substring yaml--parsing-input beg yaml--parsing-position)))
               (when yaml--parsing-store-position
                 (setq str (propertize str 'yaml-position
                                       (cons (1+ beg)
                                             (1+ yaml--parsing-position)))))
               (when (member ,name '("c-l+folded" "c-l+literal"))
                 (setq str (propertize str 'yaml-n (max 0 n))))
               (list ,name
                     (if yaml--parsing-store-position
                         (propertize str 'yaml-position (cons (1+ beg)
                                                              (1+ yaml--parsing-position)))
                       str)
                     ,res-symbol)))
            ((equal res-type 'list) (list ,name ,res-symbol))
            ((equal res-type 'literal)
             (substring yaml--parsing-input beg yaml--parsing-position))
            (t ,res-symbol)))))))

(defun yaml--end-of-stream ()
  "Return non-nil if the current position is after the end of the document."
  (>= yaml--parsing-position (length yaml--parsing-input)))

(defun yaml--char-at-pos (pos)
  "Return the character at POS."
  (aref yaml--parsing-input pos))

(defun yaml--slice (pos)
  "Return the character at POS."
  (substring yaml--parsing-input pos))

(defun yaml--at-char ()
  "Return the current character."
  (yaml--char-at-pos yaml--parsing-position))

(defun yaml--char-match (at &rest chars)
  "Return non-nil if AT match any of CHARS."
  (if (not chars)
      nil
    (or (equal at (car chars))
        (apply #'yaml--char-match (cons at (cdr chars))))))

(defun yaml--chr (c)
  "Try to match the character C."
  (if (or (yaml--end-of-stream) (not (equal (yaml--at-char) c)))
      nil
    (setq yaml--parsing-position (1+ yaml--parsing-position))
    t))

(defun yaml--chr-range (min max)
  "Return non-nil if the current character is between MIN and MAX."
  (if (or (yaml--end-of-stream) (not (<= min (yaml--at-char) max)))
      nil
    (setq yaml--parsing-position (1+ yaml--parsing-position))
    t))

(defun yaml--run-all (&rest funcs)
  "Return list of all evaluated FUNCS if all of FUNCS pass."
  (let* ((start-pos yaml--parsing-position)
         (ress '())
         (break))
    (while (and (not break) funcs)
      (let ((res (funcall (car funcs))))
        (when (not res)
          (setq break t))
        (setq ress (append ress (list res)))
        (setq funcs (cdr funcs))))
    (when break
      (setq yaml--parsing-position start-pos))
    (if break nil ress)))

(defmacro yaml--all (&rest forms)
  "Pass and return all forms if all of FORMS pass."
  `(yaml--run-all
    ,@(mapcar (lambda (form)
                `(lambda () ,form))
              forms)))

(defmacro yaml--any (&rest forms)
  "Pass if any of FORMS pass."
  (if (= 1 (length forms))
      (car forms)
    (let ((start-pos-sym (make-symbol "start"))
          (rules-sym (make-symbol "rules"))
          (res-sym (make-symbol "res")))
      `(let ((,start-pos-sym yaml--parsing-position)
             (,rules-sym ,(cons 'list
                                (seq-map (lambda (form) `(lambda () ,form))
                                         forms)))
             (,res-sym))
         (while (and (not ,res-sym) ,rules-sym)
           (setq ,res-sym (funcall (car ,rules-sym)))
           (unless ,res-sym
             (setq yaml--parsing-position ,start-pos-sym))
           (setq ,rules-sym (cdr ,rules-sym)))
         ,res-sym))))

(defmacro yaml--exclude (_)
  "Set the excluded characters according to RULE.

This is currently unimplemented."
  ;; NOTE: This is currently not implemented.
  't)

(defmacro yaml--max (_)
  "Automatically pass."
  t)

(defun yaml--empty ()
  "Return non-nil indicating that empty rule needs nothing to pass."
  't)

(defun yaml--sub (a b)
  "Return A minus B."
  (- a b))

(defun yaml--match ()
  "Return the content of the previous sibling completed."
  (let* ((states yaml--states)
         (res nil))
    (while (and states (not res))
      (let ((top-state (car states)))
        (if (yaml--state-end top-state)
            (let ((beg (yaml--state-beg top-state))
                  (end (yaml--state-end top-state)))
              (setq res (substring yaml--parsing-input beg end)))
          (setq states (cdr states)))))
    res))

(defun yaml--auto-detect (n)
  "Detect the indentation given N."
  (let* ((slice (yaml--slice yaml--parsing-position))
         (match (string-match
                 "^.*\n\\(\\(?: *\n\\)*\\)\\( *\\)"
                 slice)))
    (if (not match)
        1
      (let ((pre (match-string 1 slice))
            (m (- (length (match-string 2 slice)) n)))
        (if (< m 1)
            1
          (when (string-match (format "^.\\{%d\\}." m) pre)
            (error "Spaces found after indent in auto-detect (5LLU)"))
          m)))))

(defun yaml--auto-detect-indent (n)
  "Detect the indentation given N."
  (let* ((pos yaml--parsing-position)
         (in-seq (and
                  (> pos 0)
                  (yaml--char-match (yaml--char-at-pos (1- pos)) ?\- ?\? ?\:)))
         (slice (yaml--slice pos))
         (_ (string-match
             "^\\(\\(?: *\\(?:#.*\\)?\n\\)*\\)\\( *\\)"
             slice))
         (pre (match-string 1 slice))
         (m (length (match-string 2 slice))))
    (if (and in-seq (= (length pre) 0))
        (when (= n -1)
          (setq m (1+ m)))
      (setq m (- m n)))
    (when (< m 0)
      (setq m 0))
    m))

(defun yaml--the-end ()
  "Return non-nil if at the end of input (?)."
  (or (>= yaml--parsing-position (length yaml--parsing-input))
      (and (yaml--state-curr-doc)
           (yaml--start-of-line)
           (string-match
            "\\^g\\(?:---|\\.\\.\\.\\)\\([[:blank:]]\\|$\\)"
            (substring yaml--parsing-input yaml--parsing-position)))))

(defun yaml--ord (f)
  "Convert an ASCII number returned by F to a number."
  (let ((res (funcall f)))
    (- (aref res 0) 48)))

(defun yaml--but (&rest fs)
  "Match the first FS but none of the others."
  (if (yaml--the-end)
      nil
    (let ((pos1 yaml--parsing-position))
      (if (not (funcall (car fs)))
          nil
        (let ((pos2 yaml--parsing-position))
          (setq yaml--parsing-position pos1)
          (if (equal ':error (catch 'break
                               (dolist (f (cdr fs))
                                 (if (funcall f)
                                     (progn
                                       (setq yaml--parsing-position pos1)
                                       (throw 'break ':error))))))
              nil
            (setq yaml--parsing-position pos2)
            t))))))

(defmacro yaml--rep (min max func)
  "Repeat FUNC between MIN and MAX times."
  (declare (indent 2))
  `(yaml--rep2 ,min ,max ,func))

(defun yaml--rep2 (min max func)
  "Repeat FUNC between MIN and MAX times."
  (declare (indent 2))
  (if (and max (< max 0))
      nil
    (let* ((res-list '())
           (count 0)
           (pos yaml--parsing-position)
           (pos-start pos)
           (break nil))
      (while (and (not break) (or (not max) (< count max)))
        (let ((res (funcall func)))
          (if (or (not res) (= yaml--parsing-position pos))
              (setq break t)
            (setq res-list (cons res res-list))
            (setq count (1+ count))
            (setq pos yaml--parsing-position))))
      (if (and (>= count min)
               (or (not max) (<= count max)))
          (progn
            (setq yaml--parsing-position pos)
            (if (zerop count)
                t
              res-list))
        (setq yaml--parsing-position pos-start)
        nil))))

(defun yaml--start-of-line ()
  "Return non-nil if start of line."
  (or (= yaml--parsing-position 0)
      (>= yaml--parsing-position (length yaml--parsing-input))
      (equal (yaml--char-at-pos (1- yaml--parsing-position)) ?\n)))

(defun yaml--top ()
  "Perform top level YAML parsing rule."
  (yaml--parse-from-grammar 'l-yaml-stream))

(defmacro yaml--set (variable value)
  "Set the current state of VARIABLE to VALUE."
  (let ((res-sym (make-symbol "res")))
    `(let ((,res-sym ,value))
       (when ,res-sym
         (,(cond ((equal "m" (symbol-name variable)) 'yaml--state-set-m)
                 ((equal "t" (symbol-name variable)) 'yaml--state-set-t))
          ,res-sym)
         ,res-sym))))

(defmacro yaml--chk (type expr)
  "Check if EXPR is non-nil at the parsing position.

If TYPE is \"<=\" then check at the previous position.  If TYPE
is \"!\" ensure that EXPR is nil.  Otherwise, if TYPE is \"=\"
then check EXPR at the current position."
  (let ((start-symbol (make-symbol "start"))
        (ok-symbol (make-symbol "ok")))
    `(let ((,start-symbol yaml--parsing-position)
           (_ (when (equal ,type "<=")
                (setq yaml--parsing-position (1- yaml--parsing-position))))
           (,ok-symbol (and (>= yaml--parsing-position 0) ,expr)))
       (setq yaml--parsing-position ,start-symbol)
       (if (equal ,type "!")
           (not ,ok-symbol)
         ,ok-symbol))))

(cl-defun yaml--initialize-parsing-state
    (&key (null-object :null)
          (false-object :false)
          object-type
          object-key-type
          sequence-type
          string-values)
  "Initialize state required for parsing according to plist ARGS."
  (setq yaml--cache nil)
  (setq yaml--object-stack nil)
  (setq yaml--state-stack nil)
  (setq yaml--root nil)
  (setq yaml--anchor-mappings (make-hash-table :test 'equal))
  (setq yaml--resolve-aliases nil)
  (setq yaml--parsing-null-object null-object)
  (setq yaml--parsing-false-object false-object)
  (cond
   ((or (not object-type)
        (equal object-type 'hash-table))
    (setq yaml--parsing-object-type 'hash-table))
   ((equal 'alist object-type)
    (setq yaml--parsing-object-type 'alist))
   ((equal 'plist object-type)
    (setq yaml--parsing-object-type 'plist))
   (t (error "Invalid object-type.  Must be hash-table, alist, or plist")))
  (cond
   ((or (not object-key-type)
        (equal 'symbol object-key-type))
    (if (equal 'plist yaml--parsing-object-type)
        (setq yaml--parsing-object-key-type 'keyword)
      (setq yaml--parsing-object-key-type 'symbol)))
   ((equal 'string object-key-type)
    (setq yaml--parsing-object-key-type 'string))
   ((equal 'keyword object-key-type)
    (setq yaml--parsing-object-key-type 'keyword))
   (t (error "Invalid object-key-type.  Must be string, keyword, or symbol")))
  (cond
   ((or (not sequence-type)
        (equal sequence-type 'array))
    (setq yaml--parsing-sequence-type 'array))
   ((equal 'list sequence-type)
    (setq yaml--parsing-sequence-type 'list))
   (t (error "Invalid sequence-type.  sequence-type must be list or array")))
  (if string-values
      (setq yaml--string-values t)
    (setq yaml--string-values nil)))

(cl-defun yaml-parse-string (string
                             &key
                             (null-object :null)
                             (false-object :false)
                             object-type
                             object-key-type
                             sequence-type
                             string-values)
  "Parse the YAML value in STRING.

OBJECT-TYPE specifies the Lisp object to use for representing
key-value YAML mappings.  Possible values for OBJECT-TYPE are
the symbols `hash-table' (default), `alist', and `plist'.

OBJECT-KEY-TYPE specifies the Lisp type to use for keys in
key-value YAML mappings.  Possible values are the symbols
`string', `symbol', and `keyword'.  By default, this is `symbol';
if OBJECT-TYPE is `plist', the default is `keyword' (and `symbol'
becomes synonym for `keyword').

SEQUENCE-TYPE specifies the Lisp object to use for representing
YAML sequences.  Possible values for SEQUENCE-TYPE are the symbols
`list', and `array' (default).

NULL-OBJECT contains the object used to represent the null value.
It defaults to the symbol `:null'.

FALSE-OBJECT contains the object used to represent the false
value.  It defaults to the symbol `:false'."
  (yaml--initialize-parsing-state
   :null-object null-object
   :false-object false-object
   :object-type object-type
   :object-key-type object-key-type
   :sequence-type sequence-type
   :string-values string-values)
  (let ((res (yaml--parse string
               (yaml--top))))
    (when (< yaml--parsing-position (length yaml--parsing-input))
      (error
       "Unable to parse YAML.  Parser finished before end of input %s/%s"
       yaml--parsing-position
       (length yaml--parsing-input)))
    (when yaml--parse-debug (message "Parsed data: %s" (pp-to-string res)))
    (yaml--walk-events res)
    (if (hash-table-empty-p yaml--anchor-mappings)
        yaml--root
      ;; Run event processing twice to resolve aliases.
      (let ((yaml--root nil)
            (yaml--resolve-aliases t))
        (yaml--walk-events res)
        yaml--root))))

(defun yaml-parse-tree (string)
  "Parse the YAML value in STRING and return its parse tree."
  (yaml--initialize-parsing-state)
  (let* ((yaml--parsing-store-position t)
         (res (yaml--parse string
                (yaml--top))))
    (when (< yaml--parsing-position (length yaml--parsing-input))
      (error
       "Unable to parse YAML.  Parser finished before end of input %s/%s"
       yaml--parsing-position
       (length yaml--parsing-input)))
    res))

(defun yaml-parse-string-with-pos (string)
  "Parse the YAML value in STRING, storing positions as text properties.

NOTE: This is an experimental feature and may experience API
changes in the future."
  (let ((yaml--parsing-store-position t))
    (yaml-parse-string string
                       :object-type 'alist
                       :object-key-type 'string
                       :string-values t)))

(defun yaml--parse-from-grammar (state &rest args)
  "Parse YAML grammar for given STATE and ARGS.

Rules for this function are defined by the yaml-spec JSON file."
  (pcase state
    ('c-flow-sequence
     (let ((n (nth 0 args))
           (c (nth 1 args)))
       (yaml--frame "c-flow-sequence"
         (yaml--all
          (yaml--chr ?\[)
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 's-separate n c)))
          (yaml--rep 0 1
            (lambda ()
              (yaml--parse-from-grammar
               'ns-s-flow-seq-entries n
               (yaml--parse-from-grammar 'in-flow c))))
          (yaml--chr ?\])))))

    ('c-indentation-indicator
     (let ((m (nth 0 args)))
       (yaml--frame "c-indentation-indicator"
         (yaml--any (when (yaml--parse-from-grammar 'ns-dec-digit)
                      (yaml--set m (yaml--ord (lambda () (yaml--match)))) t)
                    (when (yaml--empty)
                      (let ((new-m (yaml--auto-detect m)))
                        (yaml--set m new-m))
                      t)))))

    ('ns-reserved-directive
     (yaml--frame "ns-reserved-directive"
       (yaml--all (yaml--parse-from-grammar 'ns-directive-name)
                  (yaml--rep2 0 nil
                    (lambda ()
                      (yaml--all
                       (yaml--parse-from-grammar 's-separate-in-line)
                       (yaml--parse-from-grammar 'ns-directive-parameter)))))))

    ('ns-flow-map-implicit-entry
     (let ((n (nth 0 args))
           (c (nth 1 args)))
       (yaml--frame "ns-flow-map-implicit-entry"
         ;; NOTE: I ran into a bug with the order of these rules. It seems
         ;; sometimes ns-flow-map-yaml-key-entry succeeds with an empty
         ;; when the correct answer should be
         ;; c-ns-flow-map-json-key-entry.  Changing the order seemed to
         ;; have fix this but this seems like a bandage fix.
         (yaml--any
          (yaml--parse-from-grammar 'c-ns-flow-map-json-key-entry n c)
          (yaml--parse-from-grammar 'ns-flow-map-yaml-key-entry n c)
          (yaml--parse-from-grammar 'c-ns-flow-map-empty-key-entry n c)))))

    ('ns-esc-double-quote
     (yaml--frame "ns-esc-double-quote"
       (yaml--chr ?\")))

    ('c-mapping-start
     (yaml--frame "c-mapping-start" (yaml--chr ?\{)))

    ('ns-flow-seq-entry
     (let ((n (nth 0 args))
           (c (nth 1 args)))
       (yaml--frame "ns-flow-seq-entry"
         (yaml--any (yaml--parse-from-grammar 'ns-flow-pair n c)
                    (yaml--parse-from-grammar 'ns-flow-node n c)))))

    ('l-empty
     (let ((n (nth 0 args))
           (c (nth 1 args)))
       (yaml--frame "l-empty"
         (yaml--all (yaml--any (yaml--parse-from-grammar 's-line-prefix n c)
                               (yaml--parse-from-grammar 's-indent-lt n))
                    (yaml--parse-from-grammar 'b-as-line-feed)))))

    ('c-primary-tag-handle
     (yaml--frame "c-primary-tag-handle" (yaml--chr ?\!)))

    ('ns-plain-safe-out
     (yaml--frame "ns-plain-safe-out"
       (yaml--parse-from-grammar 'ns-char)))

    ('c-ns-shorthand-tag
     (yaml--frame "c-ns-shorthand-tag"
       (yaml--all
        (yaml--parse-from-grammar 'c-tag-handle)
        (yaml--rep 1 nil (lambda () (yaml--parse-from-grammar 'ns-tag-char))))))

    ('nb-ns-single-in-line
     (yaml--frame "nb-ns-single-in-line"
       (yaml--rep2 0 nil
         (lambda ()
           (yaml--all (yaml--rep2 0 nil
                        (lambda () (yaml--parse-from-grammar 's-white)))
                      (yaml--parse-from-grammar 'ns-single-char))))))

    ('l-strip-empty
     (let ((n (nth 0 args)))
       (yaml--frame "l-strip-empty"
         (yaml--all
          (yaml--rep2 0 nil
            (lambda () (yaml--all
                        (yaml--parse-from-grammar 's-indent-le n)
                        (yaml--parse-from-grammar 'b-non-content))))
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 'l-trail-comments n)))))))

    ('c-indicator
     (yaml--frame "c-indicator"
       (yaml--any (yaml--chr ?\-)
                  (yaml--chr ?\?)
                  (yaml--chr ?\:)
                  (yaml--chr ?\,)
                  (yaml--chr ?\[)
                  (yaml--chr ?\])
                  (yaml--chr ?\{)
                  (yaml--chr ?\})
                  (yaml--chr ?\#)
                  (yaml--chr ?\&)
                  (yaml--chr ?\*)
                  (yaml--chr ?\!)
                  (yaml--chr ?\|)
                  (yaml--chr ?\>)
                  (yaml--chr ?\')
                  (yaml--chr ?\")
                  (yaml--chr ?\%)
                  (yaml--chr ?\@)
                  (yaml--chr ?\`))))

    ('c-l+literal
     (let ((n (nth 0 args)))
       (yaml--frame "c-l+literal"
         (progn
           (yaml--all
            (yaml--chr ?\|)
            (yaml--parse-from-grammar 'c-b-block-header n (yaml--state-curr-t))
            (yaml--parse-from-grammar 'l-literal-content
                                      (max (+ n (yaml--state-curr-m)) 1)
                                      (yaml--state-curr-t)))))))

    ('c-single-quoted
     (let ((n (nth 0 args))
           (c (nth 1 args)))
       (yaml--frame "c-single-quoted"
         (yaml--all (yaml--chr ?\')
                    (yaml--parse-from-grammar 'nb-single-text n c)
                    (yaml--chr ?\')))))

    ('c-forbidden
     (yaml--frame "c-forbidden"
       (yaml--all (yaml--start-of-line)
                  (yaml--any
                   (yaml--parse-from-grammar 'c-directives-end)
                   (yaml--parse-from-grammar 'c-document-end))
                  (yaml--any
                   (yaml--parse-from-grammar 'b-char)
                   (yaml--parse-from-grammar 's-white)
                   (yaml--end-of-stream)))))

    ('c-ns-alias-node
     (yaml--frame "c-ns-alias-node"
       (yaml--all (yaml--chr ?\*)
                  (yaml--parse-from-grammar 'ns-anchor-name))))

    ('c-secondary-tag-handle
     (yaml--frame "c-secondary-tag-handle"
       (yaml--all (yaml--chr ?\!) (yaml--chr ?\!))))

    ('ns-esc-next-line
     (yaml--frame "ns-esc-next-line" (yaml--chr ?N)))

    ('l-nb-same-lines
     (let ((n (nth 0 args)))
       (yaml--frame "l-nb-same-lines"
         (yaml--all
          (yaml--rep2 0 nil
            (lambda () (yaml--parse-from-grammar 'l-empty n "block-in")))
          (yaml--any (yaml--parse-from-grammar 'l-nb-folded-lines n)
                     (yaml--parse-from-grammar 'l-nb-spaced-lines n))))))

    ('c-alias
     (yaml--frame "c-alias" (yaml--chr ?\*)))

    ('ns-single-char
     (yaml--frame "ns-single-char"
       (yaml--but (lambda () (yaml--parse-from-grammar 'nb-single-char))
                  (lambda () (yaml--parse-from-grammar 's-white)))))

    ('c-l-block-map-implicit-value
     (let ((n (nth 0 args)))
       (yaml--frame "c-l-block-map-implicit-value"
         (yaml--all (yaml--chr ?\:)
                    (yaml--any
                     (yaml--parse-from-grammar 's-l+block-node n "block-out")
                     (yaml--all (yaml--parse-from-grammar 'e-node)
                                (yaml--parse-from-grammar 's-l-comments)))))))

    ('ns-uri-char
     (yaml--frame "ns-uri-char"
       (yaml--any (yaml--all (yaml--chr ?\%)
                             (yaml--parse-from-grammar 'ns-hex-digit)
                             (yaml--parse-from-grammar 'ns-hex-digit))
                  (yaml--parse-from-grammar 'ns-word-char)
                  (yaml--chr ?\#)
                  (yaml--chr ?\;)
                  (yaml--chr ?\/)
                  (yaml--chr ?\?)
                  (yaml--chr ?\:)
                  (yaml--chr ?\@)
                  (yaml--chr ?\&)
                  (yaml--chr ?\=)
                  (yaml--chr ?\+)
                  (yaml--chr ?\$)
                  (yaml--chr ?\,)
                  (yaml--chr ?\_)
                  (yaml--chr ?\.)
                  (yaml--chr ?\!)
                  (yaml--chr ?\~)
                  (yaml--chr ?\*)
                  (yaml--chr ?\')
                  (yaml--chr ?\()
                  (yaml--chr ?\))
                  (yaml--chr ?\[)
                  (yaml--chr ?\]))))

    ('ns-esc-16-bit
     (yaml--frame "ns-esc-16-bit"
       (yaml--all (yaml--chr ?u)
                  (yaml--rep 4 4
                    (lambda () (yaml--parse-from-grammar 'ns-hex-digit))))))

    ('l-nb-spaced-lines
     (let ((n (nth 0 args)))
       (yaml--frame "l-nb-spaced-lines"
         (yaml--all
          (yaml--parse-from-grammar 's-nb-spaced-text n)
          (yaml--rep2 0 nil
            (lambda () (yaml--all
                        (yaml--parse-from-grammar 'b-l-spaced n)
                        (yaml--parse-from-grammar 's-nb-spaced-text n))))))))

    ('ns-plain
     (let ((n (nth 0 args))
           (c (nth 1 args)))
       (yaml--frame "ns-plain"
         (pcase c
           ("block-key" (yaml--parse-from-grammar 'ns-plain-one-line c))
           ("flow-in" (yaml--parse-from-grammar 'ns-plain-multi-line n c))
           ("flow-key" (yaml--parse-from-grammar 'ns-plain-one-line c))
           ("flow-out" (yaml--parse-from-grammar 'ns-plain-multi-line n c))))))

    ('c-printable
     (yaml--frame "c-printable"
       (yaml--any (yaml--chr ?\x09)
                  (yaml--chr ?\x0A)
                  (yaml--chr ?\x0D)
                  (yaml--chr-range ?\x20 ?\x7E)
                  (yaml--chr ?\x85)
                  (yaml--chr-range ?\xA0 ?\xD7FF)
                  (yaml--chr-range ?\xE000 ?\xFFFD)
                  (yaml--chr-range ?\x010000 ?\x10FFFF))))

    ('c-mapping-value
     (yaml--frame "c-mapping-value" (yaml--chr ?\:)))

    ('l-nb-literal-text
     (let ((n (nth 0 args)))
       (yaml--frame "l-nb-literal-text"
         (yaml--all
          (yaml--rep2 0 nil
            (lambda () (yaml--parse-from-grammar 'l-empty n "block-in")))
          (yaml--parse-from-grammar 's-indent n)
          (yaml--rep 1 nil
            (lambda () (yaml--parse-from-grammar 'nb-char)))))))

    ('ns-plain-char
     (let ((c (nth 0 args)))
       (yaml--frame "ns-plain-char"
         (yaml--any
          (yaml--but
           (lambda () (yaml--parse-from-grammar 'ns-plain-safe c))
           (lambda () (yaml--chr ?\:)) (lambda () (yaml--chr ?\#)))
          (yaml--all
           (yaml--chk "<=" (yaml--parse-from-grammar 'ns-char))
           (yaml--chr ?\#))
          (yaml--all
           (yaml--chr ?\:)
           (yaml--chk "=" (yaml--parse-from-grammar 'ns-plain-safe c)))))))

    ('ns-anchor-char
     (yaml--frame "ns-anchor-char"
       (yaml--but (lambda () (yaml--parse-from-grammar 'ns-char))
                  (lambda () (yaml--parse-from-grammar 'c-flow-indicator)))))

    ('s-l+block-scalar
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "s-l+block-scalar"
         (yaml--all (yaml--parse-from-grammar 's-separate (+ n 1) c)
                    (yaml--rep 0 1
                      (lambda ()
                        (yaml--all
                         (yaml--parse-from-grammar 'c-ns-properties (+ n 1) c)
                         (yaml--parse-from-grammar 's-separate (+ n 1) c))))
                    (yaml--any (yaml--parse-from-grammar 'c-l+literal n)
                               (yaml--parse-from-grammar 'c-l+folded n))))))

    ('ns-plain-safe-in
     (yaml--frame "ns-plain-safe-in"
       (yaml--but (lambda () (yaml--parse-from-grammar 'ns-char))
                  (lambda () (yaml--parse-from-grammar 'c-flow-indicator)))))

    ('nb-single-text
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "nb-single-text"
         (pcase c
           ("block-key" (yaml--parse-from-grammar 'nb-single-one-line))
           ("flow-in" (yaml--parse-from-grammar 'nb-single-multi-line n))
           ("flow-key" (yaml--parse-from-grammar 'nb-single-one-line))
           ("flow-out" (yaml--parse-from-grammar 'nb-single-multi-line n))))))

    ('s-indent-le
     (let ((n (nth 0 args)))
       (yaml--frame "s-indent-le"
         (yaml--all (yaml--rep2 0 nil
                      (lambda () (yaml--parse-from-grammar 's-space)))
                    (<= (length (yaml--match)) n)))))

    ('ns-esc-carriage-return
     (yaml--frame "ns-esc-carriage-return" (yaml--chr ?r)))

    ('l-chomped-empty
     (let ((n (nth 0 args))
           (tt (nth 1 args)))
       (yaml--frame "l-chomped-empty"
         (pcase tt
           ("clip" (yaml--parse-from-grammar 'l-strip-empty n))
           ("keep" (yaml--parse-from-grammar 'l-keep-empty n))
           ("strip" (yaml--parse-from-grammar 'l-strip-empty n))))))

    ('c-s-implicit-json-key
     (let ((c (nth 0 args)))
       (yaml--frame "c-s-implicit-json-key"
         (yaml--all
          (yaml--max 1024)
          (yaml--parse-from-grammar 'c-flow-json-node nil c)
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 's-separate-in-line)))))))

    ('b-as-space
     (yaml--frame "b-as-space"
       (yaml--parse-from-grammar 'b-break)))

    ('ns-s-flow-seq-entries
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-s-flow-seq-entries"
         (yaml--all
          (yaml--parse-from-grammar 'ns-flow-seq-entry n c)
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 's-separate n c)))
          (yaml--rep 0 1
            (lambda ()
              (yaml--all
               (yaml--chr ?\,)
               (yaml--rep 0 1
                 (lambda () (yaml--parse-from-grammar 's-separate n c)))
               (yaml--rep 0 1
                 (lambda ()
                   (yaml--parse-from-grammar 'ns-s-flow-seq-entries
                                             n
                                             c))))))))))

    ('l-block-map-explicit-value
     (let ((n (nth 0 args)))
       (yaml--frame "l-block-map-explicit-value"
         (yaml--all
          (yaml--parse-from-grammar 's-indent n)
          (yaml--chr ?\:)
          (yaml--parse-from-grammar 's-l+block-indented n "block-out")))))

    ('c-ns-flow-map-json-key-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-ns-flow-map-json-key-entry"
         (yaml--all
          (yaml--parse-from-grammar 'c-flow-json-node n c)
          (yaml--any
           (yaml--all
            (yaml--rep 0 1
              (lambda () (yaml--parse-from-grammar 's-separate n c)))
            (yaml--parse-from-grammar 'c-ns-flow-map-adjacent-value n c))
           (yaml--parse-from-grammar 'e-node))))))

    ('c-sequence-entry
     (yaml--frame "c-sequence-entry" (yaml--chr ?\-)))

    ('l-bare-document
     (yaml--frame "l-bare-document"
       (yaml--all (yaml--exclude "c-forbidden")
                  (yaml--parse-from-grammar 's-l+block-node -1 "block-in"))))

    ;; TODO: don't use the symbol t as a variable.
    ('b-chomped-last
     (let ((tt (nth 0 args)))
       (yaml--frame "b-chomped-last"
         (pcase tt
           ("clip"
            ;; TODO: Fix this
            (yaml--any (yaml--parse-from-grammar 'b-as-line-feed)
                       (yaml--end-of-stream)))
           ("keep"
            (yaml--any (yaml--parse-from-grammar 'b-as-line-feed)
                       (yaml--end-of-stream)))
           ("strip"
            (yaml--any (yaml--parse-from-grammar 'b-non-content)
                       (yaml--end-of-stream)))))))

    ('l-trail-comments
     (let ((n (nth 0 args)))
       (yaml--frame "l-trail-comments"
         (yaml--all (yaml--parse-from-grammar 's-indent-lt n)
                    (yaml--parse-from-grammar 'c-nb-comment-text)
                    (yaml--parse-from-grammar 'b-comment)
                    (yaml--rep2 0 nil
                      (lambda () (yaml--parse-from-grammar 'l-comment)))))))

    ('ns-flow-map-yaml-key-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-map-yaml-key-entry"
         (yaml--all
          (yaml--parse-from-grammar 'ns-flow-yaml-node n c)
          (yaml--any
           (yaml--all
            (yaml--rep 0 1
              (lambda () (yaml--parse-from-grammar 's-separate n c)))
            (yaml--parse-from-grammar 'c-ns-flow-map-separate-value n c))
           (yaml--parse-from-grammar 'e-node))))))

    ('s-indent
     (let ((n (nth 0 args)))
       (yaml--frame "s-indent"
         (yaml--rep n n (lambda () (yaml--parse-from-grammar 's-space))))))

    ('ns-esc-line-separator
     (yaml--frame "ns-esc-line-separator" (yaml--chr ?L)))

    ('ns-flow-yaml-node
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-yaml-node"
         (yaml--any
          (yaml--parse-from-grammar 'c-ns-alias-node)
          (yaml--parse-from-grammar 'ns-flow-yaml-content n c)
          (yaml--all
           (yaml--parse-from-grammar 'c-ns-properties n c)
           (yaml--any
            (yaml--all
             (yaml--parse-from-grammar 's-separate n c)
             (yaml--parse-from-grammar 'ns-flow-yaml-content n c))
            (yaml--parse-from-grammar 'e-scalar)))))))

    ('ns-yaml-version
     (yaml--frame "ns-yaml-version"
       (yaml--all (yaml--rep 1 nil
                    (lambda () (yaml--parse-from-grammar 'ns-dec-digit)))
                  (yaml--chr ?\.)
                  (yaml--rep 1 nil
                    (lambda () (yaml--parse-from-grammar 'ns-dec-digit))))))

    ('c-folded
     (yaml--frame "c-folded" (yaml--chr ?\>)))

    ('c-directives-end
     (yaml--frame "c-directives-end"
       (yaml--all (yaml--chr ?\-) (yaml--chr ?\-) (yaml--chr ?\-))))

    ('s-double-break
     (let ((n (nth 0 args)))
       (yaml--frame "s-double-break"
         (yaml--any (yaml--parse-from-grammar 's-double-escaped n)
                    (yaml--parse-from-grammar 's-flow-folded n)))))

    ('s-nb-spaced-text
     (let ((n (nth 0 args)))
       (yaml--frame "s-nb-spaced-text"
         (yaml--all (yaml--parse-from-grammar 's-indent n)
                    (yaml--parse-from-grammar 's-white)
                    (yaml--rep2 0 nil
                      (lambda () (yaml--parse-from-grammar 'nb-char)))))))

    ('l-folded-content
     (let ((n (nth 0 args))
           (tt (nth 1 args)))
       (yaml--frame "l-folded-content"
         (yaml--all
          (yaml--rep 0 1
            (lambda ()
              (yaml--all (yaml--parse-from-grammar 'l-nb-diff-lines n)
                         (yaml--parse-from-grammar 'b-chomped-last tt))))
          (yaml--parse-from-grammar 'l-chomped-empty n tt)))))

    ('nb-ns-plain-in-line
     (let ((c (nth 0 args)))
       (yaml--frame "nb-ns-plain-in-line"
         (yaml--rep2 0 nil
           (lambda () (yaml--all
                       (yaml--rep2 0 nil
                         (lambda () (yaml--parse-from-grammar 's-white)))
                       (yaml--parse-from-grammar 'ns-plain-char c)))))))

    ('nb-single-multi-line
     (let ((n (nth 0 args)))
       (yaml--frame "nb-single-multi-line"
         (yaml--all
          (yaml--parse-from-grammar 'nb-ns-single-in-line)
          (yaml--any
           (yaml--parse-from-grammar 's-single-next-line n)
           (yaml--rep2 0 nil
             (lambda () (yaml--parse-from-grammar 's-white))))))))

    ('l-document-suffix
     (yaml--frame "l-document-suffix"
       (yaml--all (yaml--parse-from-grammar 'c-document-end)
                  (yaml--parse-from-grammar 's-l-comments))))

    ('c-sequence-start
     (yaml--frame "c-sequence-start"
       (yaml--chr ?\[)))

    ('ns-l-block-map-entry
     (yaml--frame "ns-l-block-map-entry"
       (yaml--any
        (yaml--parse-from-grammar 'c-l-block-map-explicit-entry
                                  (nth 0 args))
        (yaml--parse-from-grammar 'ns-l-block-map-implicit-entry
                                  (nth 0 args)))))

    ('ns-l-compact-mapping
     (yaml--frame "ns-l-compact-mapping"
       (yaml--all
        (yaml--parse-from-grammar 'ns-l-block-map-entry (nth 0 args))
        (yaml--rep2 0 nil
          (lambda ()
            (yaml--all
             (yaml--parse-from-grammar 's-indent (nth 0 args))
             (yaml--parse-from-grammar 'ns-l-block-map-entry (nth 0 args))))))))

    ('ns-esc-space
     (yaml--frame "ns-esc-space" (yaml--chr ?\x20)))
    ('ns-esc-vertical-tab
     (yaml--frame "ns-esc-vertical-tab" (yaml--chr ?v)))

    ('ns-s-implicit-yaml-key
     (let ((c (nth 0 args)))
       (yaml--frame "ns-s-implicit-yaml-key"
         (yaml--all
          (yaml--max 1024)
          (yaml--parse-from-grammar 'ns-flow-yaml-node nil c)
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 's-separate-in-line)))))))

    ('b-l-folded
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "b-l-folded"
         (yaml--any (yaml--parse-from-grammar 'b-l-trimmed n c)
                    (yaml--parse-from-grammar 'b-as-space)))))

    ('s-l+block-collection
     (yaml--frame "s-l+block-collection"
       (yaml--all
        (yaml--rep 0 1
          (lambda ()
            (yaml--all
             (yaml--parse-from-grammar
              's-separate
              (+ (nth 0 args) 1)
              (nth 1 args))
             (yaml--parse-from-grammar
              'c-ns-properties
              (+ (nth 0 args) 1)
              (nth 1 args)))))
        (yaml--parse-from-grammar 's-l-comments)
        (yaml--any
         (yaml--parse-from-grammar
          'l+block-sequence
          (yaml--parse-from-grammar 'seq-spaces (nth 0 args) (nth 1 args)))
         (yaml--parse-from-grammar 'l+block-mapping (nth 0 args))))))

    ('c-quoted-quote
     (yaml--frame "c-quoted-quote" (yaml--all (yaml--chr ?\') (yaml--chr ?\'))))

    ('l+block-sequence
     (yaml--frame "l+block-sequence"
       ;; NOTE: deviated from the spec example here by making new-m at least 1.
       ;; The wording and examples lead me to believe this is how it's done.
       ;; ie /* For some fixed auto-detected m > 0 */
       (let ((new-m (max (yaml--auto-detect-indent (nth 0 args)) 1)))
         (yaml--all
          (yaml--set m new-m)
          (yaml--rep 1 nil
            (lambda ()
              (yaml--all
               (yaml--parse-from-grammar
                's-indent
                (+ (nth 0 args) new-m))
               (yaml--parse-from-grammar
                'c-l-block-seq-entry
                (+ (nth 0 args) new-m)))))))))

    ('c-double-quote
     (yaml--frame "c-double-quote"
       (yaml--chr ?\")))

    ('ns-esc-backspace
     (yaml--frame "ns-esc-backspace"
       (yaml--chr ?b)))

    ('c-flow-json-content
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-flow-json-content"
         (yaml--any (yaml--parse-from-grammar 'c-flow-sequence n c)
                    (yaml--parse-from-grammar 'c-flow-mapping n c)
                    (yaml--parse-from-grammar 'c-single-quoted n c)
                    (yaml--parse-from-grammar 'c-double-quoted n c)))))

    ('c-mapping-end
     (yaml--frame "c-mapping-end" (yaml--chr ?\})))

    ('nb-single-char
     (yaml--frame "nb-single-char"
       (yaml--any (yaml--parse-from-grammar 'c-quoted-quote)
                  (yaml--but (lambda () (yaml--parse-from-grammar 'nb-json))
                             (lambda () (yaml--chr ?\'))))))

    ('ns-flow-node
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-node"
         (yaml--any
          (yaml--parse-from-grammar 'c-ns-alias-node)
          (yaml--parse-from-grammar 'ns-flow-content n c)
          (yaml--all
           (yaml--parse-from-grammar 'c-ns-properties n c)
           (yaml--any
            (yaml--all (yaml--parse-from-grammar 's-separate n c)
                       (yaml--parse-from-grammar 'ns-flow-content n c))
            (yaml--parse-from-grammar 'e-scalar)))))))

    ('c-non-specific-tag
     (yaml--frame "c-non-specific-tag" (yaml--chr ?\!)))

    ('l-directive-document
     (yaml--frame "l-directive-document"
       (yaml--all (yaml--rep 1 nil
                    (lambda () (yaml--parse-from-grammar 'l-directive)))
                  (yaml--parse-from-grammar 'l-explicit-document))))

    ('c-l-block-map-explicit-entry
     (let ((n (nth 0 args)))
       (yaml--frame "c-l-block-map-explicit-entry"
         (yaml--all
          (yaml--parse-from-grammar 'c-l-block-map-explicit-key n)
          (yaml--any (yaml--parse-from-grammar 'l-block-map-explicit-value n)
                     (yaml--parse-from-grammar 'e-node))))))

    ('e-node
     (yaml--frame "e-node"
       (yaml--parse-from-grammar 'e-scalar)))

    ('seq-spaces
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "seq-spaces"
         (pcase c
           ("block-in" n)
           ("block-out" (yaml--sub n 1))))))

    ('l-yaml-stream
     (yaml--frame "l-yaml-stream"
       (yaml--all
        (yaml--rep2 0 nil
          (lambda () (yaml--parse-from-grammar 'l-document-prefix)))
        (yaml--rep 0 1
          (lambda () (yaml--parse-from-grammar 'l-any-document)))
        (yaml--rep2 0 nil
          (lambda ()
            (yaml--any
             (yaml--all
              (yaml--rep 1 nil
                (lambda () (yaml--parse-from-grammar 'l-document-suffix)))
              (yaml--rep2 0 nil
                (lambda () (yaml--parse-from-grammar 'l-document-prefix)))
              (yaml--rep 0 1
                (lambda () (yaml--parse-from-grammar 'l-any-document))))
             (yaml--all
              (yaml--rep2 0 nil
                (lambda () (yaml--parse-from-grammar 'l-document-prefix)))
              (yaml--rep 0 1
                (lambda ()
                  (yaml--parse-from-grammar 'l-explicit-document))))))))))

    ('nb-double-one-line
     (yaml--frame "nb-double-one-line"
       (yaml--rep2 0 nil
         (lambda () (yaml--parse-from-grammar 'nb-double-char)))))

    ('s-l-comments
     (yaml--frame "s-l-comments"
       (yaml--all (yaml--any
                   (yaml--parse-from-grammar 's-b-comment)
                   (yaml--start-of-line))
                  (yaml--rep2 0 nil
                    (lambda () (yaml--parse-from-grammar 'l-comment))))))

    ('nb-char
     (yaml--frame "nb-char"
       (yaml--but (lambda () (yaml--parse-from-grammar 'c-printable))
                  (lambda () (yaml--parse-from-grammar 'b-char))
                  (lambda () (yaml--parse-from-grammar 'c-byte-order-mark)))))

    ('ns-plain-first
     (let ((c (nth 0 args)))
       (yaml--frame "ns-plain-first"
         (yaml--any
          (yaml--but (lambda () (yaml--parse-from-grammar 'ns-char))
                     (lambda () (yaml--parse-from-grammar 'c-indicator)))
          (yaml--all
           (yaml--any (yaml--chr ?\?)
                      (yaml--chr ?\:)
                      (yaml--chr ?\-))
           (yaml--chk "=" (yaml--parse-from-grammar 'ns-plain-safe c)))))))

    ('c-ns-esc-char
     (yaml--frame "c-ns-esc-char"
       (yaml--all
        (yaml--chr ?\\)
        (yaml--any (yaml--parse-from-grammar 'ns-esc-null)
                   (yaml--parse-from-grammar 'ns-esc-bell)
                   (yaml--parse-from-grammar 'ns-esc-backspace)
                   (yaml--parse-from-grammar 'ns-esc-horizontal-tab)
                   (yaml--parse-from-grammar 'ns-esc-line-feed)
                   (yaml--parse-from-grammar 'ns-esc-vertical-tab)
                   (yaml--parse-from-grammar 'ns-esc-form-feed)
                   (yaml--parse-from-grammar 'ns-esc-carriage-return)
                   (yaml--parse-from-grammar 'ns-esc-escape)
                   (yaml--parse-from-grammar 'ns-esc-space)
                   (yaml--parse-from-grammar 'ns-esc-double-quote)
                   (yaml--parse-from-grammar 'ns-esc-slash)
                   (yaml--parse-from-grammar 'ns-esc-backslash)
                   (yaml--parse-from-grammar 'ns-esc-next-line)
                   (yaml--parse-from-grammar 'ns-esc-non-breaking-space)
                   (yaml--parse-from-grammar 'ns-esc-line-separator)
                   (yaml--parse-from-grammar 'ns-esc-paragraph-separator)
                   (yaml--parse-from-grammar 'ns-esc-8-bit)
                   (yaml--parse-from-grammar 'ns-esc-16-bit)
                   (yaml--parse-from-grammar 'ns-esc-32-bit)))))

    ('ns-flow-map-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-map-entry"
         (yaml--any
          (yaml--all (yaml--chr ?\?)
                     (yaml--parse-from-grammar 's-separate n c)
                     (yaml--parse-from-grammar 'ns-flow-map-explicit-entry n c))
          (yaml--parse-from-grammar 'ns-flow-map-implicit-entry n c)))))

    ('l-explicit-document
     (yaml--frame "l-explicit-document"
       (yaml--all
        (yaml--parse-from-grammar 'c-directives-end)
        (yaml--any (yaml--parse-from-grammar 'l-bare-document)
                   (yaml--all (yaml--parse-from-grammar 'e-node)
                              (yaml--parse-from-grammar 's-l-comments))))))

    ('s-white
     (yaml--frame "s-white"
       (yaml--any (yaml--parse-from-grammar 's-space)
                  (yaml--parse-from-grammar 's-tab))))

    ('l-keep-empty
     (let ((n (nth 0 args)))
       (yaml--frame "l-keep-empty"
         (yaml--all
          (yaml--rep2 0 nil
            (lambda () (yaml--parse-from-grammar 'l-empty n "block-in")))
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 'l-trail-comments n)))))))

    ('ns-tag-prefix
     (yaml--frame "ns-tag-prefix"
       (yaml--any (yaml--parse-from-grammar 'c-ns-local-tag-prefix)
                  (yaml--parse-from-grammar 'ns-global-tag-prefix))))

    ('c-l+folded
     (let ((n (nth 0 args)))
       (yaml--frame "c-l+folded"
         (yaml--all
          (yaml--chr ?\>)
          (yaml--parse-from-grammar 'c-b-block-header
                                    n
                                    (yaml--state-curr-t))
          (yaml--parse-from-grammar 'l-folded-content
                                    (max (+ n (yaml--state-curr-m)) 1)
                                    (yaml--state-curr-t))))))

    ('ns-directive-name
     (yaml--frame "ns-directive-name"
       (yaml--rep 1 nil (lambda () (yaml--parse-from-grammar 'ns-char)))))

    ('b-char
     (yaml--frame "b-char"
       (yaml--any (yaml--parse-from-grammar 'b-line-feed)
                  (yaml--parse-from-grammar 'b-carriage-return))))

    ('ns-plain-multi-line
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-plain-multi-line"
         (yaml--all
          (yaml--parse-from-grammar 'ns-plain-one-line c)
          (yaml--rep2 0 nil
            (lambda ()
              (yaml--parse-from-grammar 's-ns-plain-next-line n c)))))))

    ('ns-char
     (yaml--frame "ns-char"
       (yaml--but (lambda () (yaml--parse-from-grammar 'nb-char))
                  (lambda () (yaml--parse-from-grammar 's-white)))))

    ('s-space
     (yaml--frame "s-space" (yaml--chr ?\x20)))

    ('c-l-block-seq-entry
     (yaml--frame "c-l-block-seq-entry"
       (yaml--all (yaml--chr ?\-)
                  (yaml--chk "!" (yaml--parse-from-grammar 'ns-char))
                  (yaml--parse-from-grammar 's-l+block-indented
                                            (nth 0 args)
                                            "block-in"))))

    ('c-ns-properties
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-ns-properties"
         (yaml--any
          (yaml--all
           (yaml--parse-from-grammar 'c-ns-tag-property)
           (yaml--rep 0 1
             (lambda ()
               (yaml--all
                (yaml--parse-from-grammar 's-separate n c)
                (yaml--parse-from-grammar 'c-ns-anchor-property)))))
          (yaml--all
           (yaml--parse-from-grammar 'c-ns-anchor-property)
           (yaml--rep 0 1
             (lambda () (yaml--all
                         (yaml--parse-from-grammar 's-separate n c)
                         (yaml--parse-from-grammar 'c-ns-tag-property)))))))))

    ('ns-directive-parameter
     (yaml--frame "ns-directive-parameter"
       (yaml--rep 1 nil (lambda () (yaml--parse-from-grammar 'ns-char)))))

    ('c-chomping-indicator
     (yaml--frame "c-chomping-indicator"
       (yaml--any (when (yaml--chr ?\-) (yaml--set t "strip") t)
                  (when (yaml--chr ?\+) (yaml--set t "keep") t)
                  (when (yaml--empty) (yaml--set t "clip") t))))

    ('ns-global-tag-prefix
     (yaml--frame "ns-global-tag-prefix"
       (yaml--all
        (yaml--parse-from-grammar 'ns-tag-char)
        (yaml--rep2 0 nil
          (lambda () (yaml--parse-from-grammar 'ns-uri-char))))))

    ('c-ns-flow-pair-json-key-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-ns-flow-pair-json-key-entry"
         (yaml--all
          (yaml--parse-from-grammar 'c-s-implicit-json-key "flow-key")
          (yaml--parse-from-grammar 'c-ns-flow-map-adjacent-value n c)))))

    ('l-literal-content
     (let ((n (nth 0 args))
           (tt (nth 1 args)))
       (yaml--frame "l-literal-content"
         (yaml--all
          (yaml--rep 0 1
            (lambda ()
              (yaml--all (yaml--parse-from-grammar 'l-nb-literal-text n)
                         (yaml--rep2 0 nil
                           (lambda ()
                             (yaml--parse-from-grammar 'b-nb-literal-next n)))
                         (yaml--parse-from-grammar 'b-chomped-last tt))))
          (yaml--parse-from-grammar 'l-chomped-empty n tt)))))

    ('c-document-end
     (yaml--frame "c-document-end"
       (yaml--all (yaml--chr ?\.)
                  (yaml--chr ?\.)
                  (yaml--chr ?\.))))

    ('nb-double-text
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "nb-double-text"
         (pcase c
           ("block-key" (yaml--parse-from-grammar 'nb-double-one-line))
           ("flow-in" (yaml--parse-from-grammar 'nb-double-multi-line n))
           ("flow-key" (yaml--parse-from-grammar 'nb-double-one-line))
           ("flow-out" (yaml--parse-from-grammar 'nb-double-multi-line n))))))

    ('s-b-comment
     (yaml--frame "s-b-comment"
       (yaml--all
        (yaml--rep 0 1
          (lambda ()
            (yaml--all
             (yaml--parse-from-grammar 's-separate-in-line)
             (yaml--rep 0 1
               (lambda () (yaml--parse-from-grammar 'c-nb-comment-text))))))
        (yaml--parse-from-grammar 'b-comment))))

    ('s-block-line-prefix
     (let ((n (nth 0 args)))
       (yaml--frame "s-block-line-prefix"
         (yaml--parse-from-grammar 's-indent n))))

    ('c-tag-handle
     (yaml--frame "c-tag-handle"
       (yaml--any (yaml--parse-from-grammar 'c-named-tag-handle)
                  (yaml--parse-from-grammar 'c-secondary-tag-handle)
                  (yaml--parse-from-grammar 'c-primary-tag-handle))))

    ('ns-plain-one-line
     (let ((c (nth 0 args)))
       (yaml--frame "ns-plain-one-line"
         (yaml--all (yaml--parse-from-grammar 'ns-plain-first c)
                    (yaml--parse-from-grammar 'nb-ns-plain-in-line c)))))

    ('nb-json
     (yaml--frame "nb-json"
       (yaml--any (yaml--chr ?\x09)
                  (yaml--chr-range ?\x20 ?\x10FFFF))))

    ('s-ns-plain-next-line
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "s-ns-plain-next-line"
         (yaml--all (yaml--parse-from-grammar 's-flow-folded n)
                    (yaml--parse-from-grammar 'ns-plain-char c)
                    (yaml--parse-from-grammar 'nb-ns-plain-in-line c)))))

    ('c-reserved
     (yaml--frame "c-reserved"
       (yaml--any (yaml--chr ?\@) (yaml--chr ?\`))))

    ('b-l-trimmed
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "b-l-trimmed"
         (yaml--all
          (yaml--parse-from-grammar 'b-non-content)
          (yaml--rep 1 nil
            (lambda () (yaml--parse-from-grammar 'l-empty n c)))))))

    ('l-document-prefix
     (yaml--frame "l-document-prefix"
       (yaml--all
        (yaml--rep 0 1
          (lambda () (yaml--parse-from-grammar 'c-byte-order-mark)))
        (yaml--rep2 0 nil
          (lambda () (yaml--parse-from-grammar 'l-comment))))))

    ('c-byte-order-mark
     (yaml--frame "c-byte-order-mark" (yaml--chr ?\xFEFF)))

    ('c-anchor
     (yaml--frame "c-anchor" (yaml--chr ?\&)))

    ('s-double-escaped
     (let ((n (nth 0 args)))
       (yaml--frame "s-double-escaped"
         (yaml--all
          (yaml--rep2 0 nil
            (lambda () (yaml--parse-from-grammar 's-white)))
          (yaml--chr ?\\)
          (yaml--parse-from-grammar 'b-non-content)
          (yaml--rep2 0 nil
            (lambda () (yaml--parse-from-grammar 'l-empty n "flow-in")))
          (yaml--parse-from-grammar 's-flow-line-prefix n)))))

    ('ns-esc-32-bit
     (yaml--frame "ns-esc-32-bit"
       (yaml--all
        (yaml--chr ?U)
        (yaml--rep 8 8 (lambda () (yaml--parse-from-grammar 'ns-hex-digit))))))


    ('b-non-content
     (yaml--frame "b-non-content" (yaml--parse-from-grammar 'b-break)))

    ('ns-tag-char
     (yaml--frame "ns-tag-char"
       (yaml--but (lambda () (yaml--parse-from-grammar 'ns-uri-char))
                  (lambda () (yaml--chr ?\!))
                  (lambda () (yaml--parse-from-grammar 'c-flow-indicator)))))

    ('b-carriage-return
     (yaml--frame "b-carriage-return" (yaml--chr ?\x0D)))

    ('s-double-next-line
     (let ((n (nth 0 args)))
       (yaml--frame "s-double-next-line"
         (yaml--all
          (yaml--parse-from-grammar 's-double-break n)
          (yaml--rep 0 1
            (lambda ()
              (yaml--all
               (yaml--parse-from-grammar 'ns-double-char)
               (yaml--parse-from-grammar 'nb-ns-double-in-line)
               (yaml--any
                (yaml--parse-from-grammar 's-double-next-line n)
                (yaml--rep2 0 nil
                  (lambda () (yaml--parse-from-grammar 's-white)))))))))))

    ('ns-esc-non-breaking-space
     (yaml--frame "ns-esc-non-breaking-space" (yaml--chr ?\_)))

    ('l-nb-diff-lines
     (let ((n (nth 0 args)))
       (yaml--frame "l-nb-diff-lines"
         (yaml--all
          (yaml--parse-from-grammar 'l-nb-same-lines n)
          (yaml--rep2 0 nil
            (lambda ()
              (yaml--all (yaml--parse-from-grammar 'b-as-line-feed)
                         (yaml--parse-from-grammar 'l-nb-same-lines n))))))))

    ('s-flow-folded
     (let ((n (nth 0 args)))
       (yaml--frame "s-flow-folded"
         (yaml--all
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 's-separate-in-line)))
          (yaml--parse-from-grammar 'b-l-folded n "flow-in")
          (yaml--parse-from-grammar 's-flow-line-prefix n)))))

    ('ns-flow-map-explicit-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-map-explicit-entry"
         (yaml--any
          (yaml--parse-from-grammar 'ns-flow-map-implicit-entry n c)
          (yaml--all
           (yaml--parse-from-grammar 'e-node)
           (yaml--parse-from-grammar 'e-node))))))

    ('ns-l-block-map-implicit-entry
     (yaml--frame "ns-l-block-map-implicit-entry"
       (yaml--all
        (yaml--any (yaml--parse-from-grammar 'ns-s-block-map-implicit-key)
                   (yaml--parse-from-grammar 'e-node))
        (yaml--parse-from-grammar 'c-l-block-map-implicit-value (nth 0 args)))))

    ('l-nb-folded-lines
     (let ((n (nth 0 args)))
       (yaml--frame "l-nb-folded-lines"
         (yaml--all
          (yaml--parse-from-grammar 's-nb-folded-text n)
          (yaml--rep2 0 nil
            (lambda ()
              (yaml--all (yaml--parse-from-grammar 'b-l-folded n "block-in")
                         (yaml--parse-from-grammar 's-nb-folded-text n))))))))

    ('c-l-block-map-explicit-key
     (let ((n (nth 0 args)))
       (yaml--frame "c-l-block-map-explicit-key"
         (yaml--all
          (yaml--chr ?\?)
          (yaml--parse-from-grammar 's-l+block-indented n "block-out")))))

    ('s-separate
     (let ((n (nth 0 args))
           (c (nth 1 args)))
       (yaml--frame "s-separate"
         (pcase c
           ("block-in" (yaml--parse-from-grammar 's-separate-lines n))
           ("block-key" (yaml--parse-from-grammar 's-separate-in-line))
           ("block-out" (yaml--parse-from-grammar 's-separate-lines n))
           ("flow-in" (yaml--parse-from-grammar 's-separate-lines n))
           ("flow-key" (yaml--parse-from-grammar 's-separate-in-line))
           ("flow-out" (yaml--parse-from-grammar 's-separate-lines n))))))

    ('ns-flow-pair-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-pair-entry"
         (yaml--any
          (yaml--parse-from-grammar 'ns-flow-pair-yaml-key-entry n c)
          (yaml--parse-from-grammar 'c-ns-flow-map-empty-key-entry n c)
          (yaml--parse-from-grammar 'c-ns-flow-pair-json-key-entry n c)))))

    ('c-flow-indicator
     (yaml--frame "c-flow-indicator"
       (yaml--any (yaml--chr ?\,)
                  (yaml--chr ?\[)
                  (yaml--chr ?\])
                  (yaml--chr ?\{)
                  (yaml--chr ?\}))))

    ('ns-flow-pair-yaml-key-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-pair-yaml-key-entry"
         (yaml--all
          (yaml--parse-from-grammar 'ns-s-implicit-yaml-key "flow-key")
          (yaml--parse-from-grammar 'c-ns-flow-map-separate-value n c)))))

    ('e-scalar
     (yaml--frame "e-scalar" (yaml--empty)))

    ('s-indent-lt
     (let ((n (nth 0 args)))
       (yaml--frame "s-indent-lt"
         (yaml--all
          (yaml--rep2 0 nil
            (lambda () (yaml--parse-from-grammar 's-space)))
          (< (length (yaml--match)) n)))))

    ('nb-single-one-line
     (yaml--frame "nb-single-one-line"
       (yaml--rep2 0 nil
         (lambda () (yaml--parse-from-grammar 'nb-single-char)))))

    ('c-collect-entry
     (yaml--frame "c-collect-entry" (yaml--chr ?\,)))

    ('ns-l-compact-sequence
     (let ((n (nth 0 args)))
       (yaml--frame "ns-l-compact-sequence"
         (yaml--all
          (yaml--parse-from-grammar 'c-l-block-seq-entry n)
          (yaml--rep2 0 nil
            (lambda ()
              (yaml--all
               (yaml--parse-from-grammar 's-indent n)
               (yaml--parse-from-grammar 'c-l-block-seq-entry n))))))))

    ('c-comment
     (yaml--frame "c-comment" (yaml--chr ?\#)))

    ('s-line-prefix
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "s-line-prefix"
         (pcase c
           ("block-in" (yaml--parse-from-grammar 's-block-line-prefix n))
           ("block-out" (yaml--parse-from-grammar 's-block-line-prefix n))
           ("flow-in" (yaml--parse-from-grammar 's-flow-line-prefix n))
           ("flow-out" (yaml--parse-from-grammar 's-flow-line-prefix n))))))

    ('s-tab
     (yaml--frame "s-tab" (yaml--chr ?\x09)))

    ('c-directive
     (yaml--frame "c-directive" (yaml--chr ?\%)))

    ('ns-flow-pair
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-pair"
         (yaml--any
          (yaml--all (yaml--chr ?\?)
                     (yaml--parse-from-grammar 's-separate n c)
                     (yaml--parse-from-grammar 'ns-flow-map-explicit-entry n c))
          (yaml--parse-from-grammar 'ns-flow-pair-entry n c)))))

    ('s-l+block-indented
     (yaml--frame "s-l+block-indented"
       (let ((m (yaml--auto-detect-indent (nth 0 args))))
         (yaml--any
          (yaml--all
           (yaml--parse-from-grammar 's-indent m)
           (yaml--any
            (yaml--parse-from-grammar 'ns-l-compact-sequence
                                      (+ (nth 0 args) (+ 1 m)))
            (yaml--parse-from-grammar 'ns-l-compact-mapping
                                      (+ (nth 0 args) (+ 1 m)))))
          (yaml--parse-from-grammar 's-l+block-node (nth 0 args) (nth 1 args))
          (yaml--all (yaml--parse-from-grammar 'e-node)
                     (yaml--parse-from-grammar 's-l-comments))))))

    ('c-single-quote
     (yaml--frame "c-single-quote" (yaml--chr ?\')))

    ('s-flow-line-prefix
     (let ((n (nth 0 args)))
       (yaml--frame "s-flow-line-prefix"
         (yaml--all
          (yaml--parse-from-grammar 's-indent n)
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 's-separate-in-line)))))))

    ('nb-double-char
     (yaml--frame "nb-double-char"
       (yaml--any
        (yaml--parse-from-grammar 'c-ns-esc-char)
        (yaml--but (lambda () (yaml--parse-from-grammar 'nb-json))
                   (lambda () (yaml--chr ?\\)) (lambda () (yaml--chr ?\"))))))

    ('l-comment
     (yaml--frame "l-comment"
       (yaml--all
        (yaml--parse-from-grammar 's-separate-in-line)
        (yaml--rep 0 1
          (lambda () (yaml--parse-from-grammar 'c-nb-comment-text)))
        (yaml--parse-from-grammar 'b-comment))))

    ('ns-hex-digit
     (yaml--frame "ns-hex-digit"
       (yaml--any
        (yaml--parse-from-grammar 'ns-dec-digit)
        (yaml--chr-range ?\x41 ?\x46)
        (yaml--chr-range ?\x61 ?\x66))))

    ('s-l+flow-in-block
     (let ((n (nth 0 args)))
       (yaml--frame "s-l+flow-in-block"
         (yaml--all
          (yaml--parse-from-grammar 's-separate (+ n 1) "flow-out")
          (yaml--parse-from-grammar 'ns-flow-node (+ n 1) "flow-out")
          (yaml--parse-from-grammar 's-l-comments)))))

    ('c-flow-json-node
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-flow-json-node"
         (yaml--all
          (yaml--rep 0 1
            (lambda ()
              (yaml--all
               (yaml--parse-from-grammar 'c-ns-properties n c)
               (yaml--parse-from-grammar 's-separate n c))))
          (yaml--parse-from-grammar 'c-flow-json-content n c)))))

    ('c-b-block-header
     (let ((m (nth 0 args))
           (tt (nth 1 args)))
       (yaml--frame "c-b-block-header"
         (yaml--all
          (yaml--any
           (and (not (string-match "\\`[-+][0-9]"
                                   (yaml--slice yaml--parsing-position)))
                ;; hack to not match this case if there is a number.
                (yaml--all
                 (yaml--parse-from-grammar 'c-indentation-indicator m)
                 (yaml--parse-from-grammar 'c-chomping-indicator tt)))
           (yaml--all
            (yaml--parse-from-grammar 'c-chomping-indicator tt)
            (yaml--parse-from-grammar 'c-indentation-indicator m)))
          (yaml--parse-from-grammar 's-b-comment)))))

    ('ns-esc-8-bit
     (yaml--frame "ns-esc-8-bit"
       (yaml--all (yaml--chr ?x)
                  (yaml--rep 2 2
                    (lambda () (yaml--parse-from-grammar 'ns-hex-digit))))))

    ('ns-anchor-name
     (yaml--frame "ns-anchor-name"
       (yaml--rep 1 nil
         (lambda () (yaml--parse-from-grammar 'ns-anchor-char)))))

    ('ns-esc-slash
     (yaml--frame "ns-esc-slash" (yaml--chr ?\/)))

    ('s-nb-folded-text
     (let ((n (nth 0 args)))
       (yaml--frame "s-nb-folded-text"
         (yaml--all (yaml--parse-from-grammar 's-indent n)
                    (yaml--parse-from-grammar 'ns-char)
                    (yaml--rep2 0 nil
                      (lambda () (yaml--parse-from-grammar 'nb-char)))))))

    ('ns-word-char
     (yaml--frame "ns-word-char"
       (yaml--any (yaml--parse-from-grammar 'ns-dec-digit)
                  (yaml--parse-from-grammar 'ns-ascii-letter)
                  (yaml--chr ?\-))))

    ('ns-esc-form-feed
     (yaml--frame "ns-esc-form-feed" (yaml--chr ?f)))

    ('ns-s-block-map-implicit-key
     (yaml--frame "ns-s-block-map-implicit-key"
       (yaml--any
        (yaml--parse-from-grammar 'c-s-implicit-json-key "block-key")
        (yaml--parse-from-grammar 'ns-s-implicit-yaml-key "block-key"))))

    ('ns-esc-null (yaml--frame "ns-esc-null" (yaml--chr ?\0)))

    ('c-ns-tag-property
     (yaml--frame "c-ns-tag-property"
       (yaml--any (yaml--parse-from-grammar 'c-verbatim-tag)
                  (yaml--parse-from-grammar 'c-ns-shorthand-tag)
                  (yaml--parse-from-grammar 'c-non-specific-tag))))

    ('c-ns-local-tag-prefix
     (yaml--frame "c-ns-local-tag-prefix"
       (yaml--all
        (yaml--chr ?\!)
        (yaml--rep2 0 nil
          (lambda () (yaml--parse-from-grammar 'ns-uri-char))))))

    ('ns-tag-directive
     (yaml--frame "ns-tag-directive"
       (yaml--all (yaml--chr ?T) (yaml--chr ?A) (yaml--chr ?G)
                  (yaml--parse-from-grammar 's-separate-in-line)
                  (yaml--parse-from-grammar 'c-tag-handle)
                  (yaml--parse-from-grammar 's-separate-in-line)
                  (yaml--parse-from-grammar 'ns-tag-prefix))))

    ('c-flow-mapping
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-flow-mapping"
         (yaml--all
          (yaml--chr ?\{)
          (yaml--rep 0 1
            (lambda () (yaml--parse-from-grammar 's-separate n c)))
          (yaml--rep 0 1
            (lambda ()
              (yaml--parse-from-grammar 'ns-s-flow-map-entries
                                        n
                                        (yaml--parse-from-grammar 'in-flow c))))
          (yaml--chr ?\})))))

    ('ns-double-char
     (yaml--frame "ns-double-char"
       (yaml--but (lambda () (yaml--parse-from-grammar 'nb-double-char))
                  (lambda () (yaml--parse-from-grammar 's-white)))))

    ('ns-ascii-letter
     (yaml--frame "ns-ascii-letter"
       (yaml--any (yaml--chr-range ?\x41 ?\x5A)
                  (yaml--chr-range ?\x61 ?\x7A))))

    ('b-break
     (yaml--frame "b-break"
       (yaml--any (yaml--all (yaml--parse-from-grammar 'b-carriage-return)
                             (yaml--parse-from-grammar 'b-line-feed))
                  (yaml--parse-from-grammar 'b-carriage-return)
                  (yaml--parse-from-grammar 'b-line-feed))))

    ('nb-ns-double-in-line
     (yaml--frame "nb-ns-double-in-line"
       (yaml--rep2 0 nil
         (lambda ()
           (yaml--all
            (yaml--rep2 0 nil
              (lambda () (yaml--parse-from-grammar 's-white)))
            (yaml--parse-from-grammar 'ns-double-char))))))

    ('s-l+block-node
     (yaml--frame "s-l+block-node"
       (yaml--any
        (yaml--parse-from-grammar 's-l+block-in-block (nth 0 args) (nth 1 args))
        (yaml--parse-from-grammar 's-l+flow-in-block (nth 0 args)))))

    ('ns-esc-bell
     (yaml--frame "ns-esc-bell" (yaml--chr ?a)))

    ('c-named-tag-handle
     (yaml--frame "c-named-tag-handle"
       (yaml--all
        (yaml--chr ?\!)
        (yaml--rep 1 nil (lambda () (yaml--parse-from-grammar 'ns-word-char)))
        (yaml--chr ?\!))))

    ('s-separate-lines
     (let ((n (nth 0 args)))
       (yaml--frame "s-separate-lines"
         (yaml--any (yaml--all (yaml--parse-from-grammar 's-l-comments)
                               (yaml--parse-from-grammar 's-flow-line-prefix n))
                    (yaml--parse-from-grammar 's-separate-in-line)))))

    ('l-directive
     (yaml--frame "l-directive"
       (yaml--all (yaml--chr ?\%)
                  (yaml--any (yaml--parse-from-grammar 'ns-yaml-directive)
                             (yaml--parse-from-grammar 'ns-tag-directive)
                             (yaml--parse-from-grammar 'ns-reserved-directive))
                  (yaml--parse-from-grammar 's-l-comments))))

    ('ns-esc-escape
     (yaml--frame "ns-esc-escape" (yaml--chr ?e)))

    ('b-nb-literal-next
     (let ((n (nth 0 args)))
       (yaml--frame "b-nb-literal-next"
         (yaml--all (yaml--parse-from-grammar 'b-as-line-feed)
                    (yaml--parse-from-grammar 'l-nb-literal-text n)))))

    ('ns-s-flow-map-entries
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-s-flow-map-entries"
         (yaml--all
          (yaml--parse-from-grammar 'ns-flow-map-entry n c)
          (yaml--rep 0 1 (lambda () (yaml--parse-from-grammar 's-separate n c)))
          (yaml--rep 0 1
            (lambda ()
              (yaml--all
               (yaml--chr ?\,)
               (yaml--rep 0 1
                 (lambda () (yaml--parse-from-grammar 's-separate n c)))
               (yaml--rep 0 1
                 (lambda ()
                   (yaml--parse-from-grammar 'ns-s-flow-map-entries
                                             n
                                             c))))))))))

    ('c-nb-comment-text
     (yaml--frame "c-nb-comment-text"
       (yaml--all
        (yaml--chr ?\#)
        (yaml--rep2 0 nil (lambda () (yaml--parse-from-grammar 'nb-char))))))

    ('ns-dec-digit
     (yaml--frame "ns-dec-digit"
       (yaml--chr-range ?\x30 ?\x39)))

    ('ns-yaml-directive
     (yaml--frame "ns-yaml-directive"
       (yaml--all (yaml--chr ?Y) (yaml--chr ?A) (yaml--chr ?M) (yaml--chr ?L)
                  (yaml--parse-from-grammar 's-separate-in-line)
                  (yaml--parse-from-grammar 'ns-yaml-version))))

    ('c-mapping-key
     (yaml--frame "c-mapping-key" (yaml--chr ?\?)))

    ('b-as-line-feed
     (yaml--frame "b-as-line-feed"
       (yaml--parse-from-grammar 'b-break)))

    ('s-l+block-in-block
     (yaml--frame "s-l+block-in-block"
       (yaml--any
        (yaml--parse-from-grammar 's-l+block-scalar
                                  (nth 0 args)
                                  (nth 1 args))
        (yaml--parse-from-grammar 's-l+block-collection
                                  (nth 0 args)
                                  (nth 1 args)))))

    ('ns-esc-paragraph-separator
     (yaml--frame "ns-esc-paragraph-separator" (yaml--chr ?P)))

    ('c-double-quoted
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-double-quoted"
         (yaml--all (yaml--chr ?\")
                    (yaml--parse-from-grammar 'nb-double-text n c)
                    (yaml--chr ?\")))))

    ('b-line-feed
     (yaml--frame "b-line-feed" (yaml--chr ?\x0A)))

    ('ns-esc-horizontal-tab
     (yaml--frame "ns-esc-horizontal-tab"
       (yaml--any (yaml--chr ?t) (yaml--chr ?\x09))))

    ('c-ns-flow-map-empty-key-entry
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-ns-flow-map-empty-key-entry"
         (yaml--all
          (yaml--parse-from-grammar 'e-node)
          (yaml--parse-from-grammar 'c-ns-flow-map-separate-value n c)))))

    ('l-any-document
     (yaml--frame "l-any-document"
       (yaml--any (yaml--parse-from-grammar 'l-directive-document)
                  (yaml--parse-from-grammar 'l-explicit-document)
                  (yaml--parse-from-grammar 'l-bare-document))))

    ('c-tag (yaml--frame "c-tag" (yaml--chr ?\!)))

    ('c-escape (yaml--frame "c-escape" (yaml--chr ?\\)))

    ('c-sequence-end (yaml--frame "c-sequence-end" (yaml--chr ?\])))

    ('l+block-mapping
     (yaml--frame "l+block-mapping"
       (let ((new-m (yaml--auto-detect-indent (nth 0 args))))
         (if (= 0 new-m)
             nil ;; For some fixed auto-detected m > 0 ;; Is this right???
           (yaml--all
            (yaml--set m new-m)
            (yaml--rep 1 nil
              (lambda ()
                (yaml--all
                 (yaml--parse-from-grammar 's-indent
                                           (+ (nth 0 args) new-m))
                 (yaml--parse-from-grammar 'ns-l-block-map-entry
                                           (+ (nth 0 args) new-m))))))))))

    ('c-ns-flow-map-adjacent-value
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-ns-flow-map-adjacent-value"
         (yaml--all
          (yaml--chr ?\:)
          (yaml--any
           (yaml--all
            (yaml--rep 0 1
              (lambda () (yaml--parse-from-grammar 's-separate n c)))
            (yaml--parse-from-grammar 'ns-flow-node n c))
           (yaml--parse-from-grammar 'e-node))))))

    ('s-single-next-line
     (let ((n (nth 0 args)))
       (yaml--frame "s-single-next-line"
         (yaml--all
          (yaml--parse-from-grammar 's-flow-folded n)
          (yaml--rep 0 1
            (lambda ()
              (yaml--all
               (yaml--parse-from-grammar 'ns-single-char)
               (yaml--parse-from-grammar 'nb-ns-single-in-line)
               (yaml--any
                (yaml--parse-from-grammar 's-single-next-line n)
                (yaml--rep2 0 nil
                  (lambda () (yaml--parse-from-grammar 's-white)))))))))))

    ('s-separate-in-line
     (yaml--frame "s-separate-in-line"
       (yaml--any (yaml--rep 1 nil
                    (lambda () (yaml--parse-from-grammar 's-white)))
                  (yaml--start-of-line))))

    ('b-comment
     (yaml--frame "b-comment"
       (yaml--any (yaml--parse-from-grammar 'b-non-content)
                  (yaml--end-of-stream))))

    ('ns-esc-backslash
     (yaml--frame "ns-esc-backslash" (yaml--chr ?\\)))

    ('c-ns-anchor-property
     (yaml--frame "c-ns-anchor-property"
       (yaml--all (yaml--chr ?\&)
                  (yaml--parse-from-grammar 'ns-anchor-name))))

    ('ns-plain-safe
     (let ((c (nth 0 args)))
       (yaml--frame "ns-plain-safe"
         (pcase c
           ("block-key" (yaml--parse-from-grammar 'ns-plain-safe-out))
           ("flow-in" (yaml--parse-from-grammar 'ns-plain-safe-in))
           ("flow-key" (yaml--parse-from-grammar 'ns-plain-safe-in))
           ("flow-out" (yaml--parse-from-grammar 'ns-plain-safe-out))))))

    ('ns-flow-content
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-content"
         (yaml--any (yaml--parse-from-grammar 'ns-flow-yaml-content n c)
                    (yaml--parse-from-grammar 'c-flow-json-content n c)))))

    ('c-ns-flow-map-separate-value
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "c-ns-flow-map-separate-value"
         (yaml--all
          (yaml--chr ?\:)
          (yaml--chk "!" (yaml--parse-from-grammar 'ns-plain-safe c))
          (yaml--any (yaml--all (yaml--parse-from-grammar 's-separate n c)
                                (yaml--parse-from-grammar 'ns-flow-node n c))
                     (yaml--parse-from-grammar 'e-node))))))

    ('in-flow
     (let ((c (nth 0 args)))
       (yaml--frame "in-flow"
         (pcase c
           ("block-key" "flow-key")
           ("flow-in" "flow-in")
           ("flow-key" "flow-key")
           ("flow-out" "flow-in")))))

    ('c-verbatim-tag
     (yaml--frame "c-verbatim-tag"
       (yaml--all
        (yaml--chr ?\!)
        (yaml--chr ?\<)
        (yaml--rep 1 nil (lambda () (yaml--parse-from-grammar 'ns-uri-char)))
        (yaml--chr ?\>))))

    ('c-literal
     (yaml--frame "c-literal" (yaml--chr ?\|)))

    ('ns-esc-line-feed
     (yaml--frame "ns-esc-line-feed" (yaml--chr ?n)))

    ('nb-double-multi-line
     (let ((n (nth 0 args)))
       (yaml--frame "nb-double-multi-line"
         (yaml--all
          (yaml--parse-from-grammar 'nb-ns-double-in-line)
          (yaml--any (yaml--parse-from-grammar 's-double-next-line n)
                     (yaml--rep2 0 nil
                       (lambda () (yaml--parse-from-grammar 's-white))))))))

    ('b-l-spaced
     (let ((n (nth 0 args)))
       (yaml--frame "b-l-spaced"
         (yaml--all
          (yaml--parse-from-grammar 'b-as-line-feed)
          (yaml--rep2 0 nil
            (lambda () (yaml--parse-from-grammar 'l-empty n "block-in")))))))

    ('ns-flow-yaml-content
     (let ((n (nth 0 args)) (c (nth 1 args)))
       (yaml--frame "ns-flow-yaml-content"
         (yaml--parse-from-grammar 'ns-plain n c))))
    (_ (error "Unknown parsing grammar state: %s %s" state args))))

;;; Encoding

(defun yaml-encode (object)
  "Encode OBJECT to a YAML string."
  (with-temp-buffer
    (yaml--encode-object object 0)
    (goto-char (point-min))
    (while (looking-at-p "\n")
      (delete-char 1))
    (buffer-string)))

(defun yaml--encode-object (object indent &optional auto-indent)
  "Encode a Lisp OBJECT to YAML.

INDENT indicates how deeply nested the object will be displayed
in the YAML.  If AUTO-INDENT is non-nil, then emit the object
without first inserting a newline."
  (cond
   ((yaml--scalarp object) (yaml--encode-scalar object))
   ((hash-table-p object) (yaml--encode-hash-table object indent auto-indent))
   ((listp object) (yaml--encode-list object indent auto-indent))
   ((arrayp object) (yaml--encode-array object indent auto-indent))
   (t (error "Unknown object %s" object))))

(defun yaml--scalarp (object)
  "Return non-nil if OBJECT correlates to a YAML scalar."
  (or (numberp object)
      (symbolp object)
      (stringp object)
      (not object)))

(defun yaml--encode-escape-string (s)
  "Escape yaml special characters in string S."
  (let* ((s (replace-regexp-in-string "\\\\" "\\\\" s))
         (s (replace-regexp-in-string "\n" "\\\\n" s))
         (s (replace-regexp-in-string "\t" "\\\\t" s))
         (s (replace-regexp-in-string "\r" "\\\\r" s))
         (s (replace-regexp-in-string "\"" "\\\\\"" s)))
    s))

(defun yaml--encode-array (a indent &optional auto-indent)
  "Encode array A to a string in the context of being INDENT deep.

If AUTO-INDENT is non-nil, start the list on the current line,
auto-detecting the indentation.  Functionality defers to
`yaml--encode-list'."
  (yaml--encode-list (seq-map #'identity a)
                     indent
                     auto-indent))


(defun yaml--encode-scalar (s)
  "Encode scalar S to buffer."
  (cond
   ((not s) (insert "null"))
   ((eql t s) (insert "true"))
   ((symbolp s)
    (cond
     ((eql s :null) (insert "null"))
     ((eql s :false) (insert "false"))
     (t (insert (symbol-name s)))))
   ((numberp s) (insert (number-to-string s)))
   ((stringp s)
    (if (string-match "\\`[-_a-zA-Z0-9]+\\'" s)
        (insert s)
      (insert "\"" (yaml--encode-escape-string s) "\"")))))

(defun yaml--alist-to-hash-table (l)
  "Return hash representation of L if it is an alist, nil otherwise."
  (when (and (listp l)
             (seq-every-p (lambda (x) (and (consp x) (atom (car x)))) l))
    (let ((h (make-hash-table)))
      (seq-map (lambda (cpair)
                 (let* ((k (car cpair))
                        (v (alist-get k l)))
                   (puthash k v h)))
               l)
      h)))

(defun yaml--encode-list (l indent &optional auto-indent)
  "Encode list L to a string in the context of being INDENT deep.

If AUTO-INDENT is non-nil, start the list on the current line,
auto-detecting the indentation"
  (let ((ht (yaml--alist-to-hash-table l)))
    (cond (ht
           (yaml--encode-hash-table ht indent auto-indent))
          ((zerop (length l))
           (insert "[]"))
          ((seq-every-p #'yaml--scalarp l)
           (insert "[")
           (yaml--encode-object (car l) 0)
           (seq-do (lambda (object)
                     (insert ", ")
                     (yaml--encode-object object 0))
                   (cdr l))
           (insert "]"))
          (t
           (when (zerop indent)
             (setq indent 2))
           (let* ((first t)
                  (indent-string (make-string (- indent 2) ?\s)))
             (seq-do
              (lambda (object)
                (if (not first)
                    (insert "\n" indent-string "- ")
                  (if auto-indent
                      (let ((curr-indent (yaml--encode-auto-detect-indent)))
                        (insert (make-string (- indent curr-indent) ?\s)  "- "))
                    (insert "\n" indent-string "- "))
                  (setq first nil))
                (if (or (hash-table-p object)
                        (yaml--alist-to-hash-table object))
                    (yaml--encode-object object indent t)
                  (yaml--encode-object object (+ indent 2) nil)))
              l))))))

(defun yaml--encode-auto-detect-indent ()
  "Return the amount of indentation at current place in encoding."
  (length (thing-at-point 'line)))

(defun yaml--encode-hash-table (m indent &optional auto-indent)
  "Encode hash table M to a string in the context of being INDENT deep.

If AUTO-INDENT is non-nil, auto-detect the indent on the current
line and insert accordingly."
  (cond ((zerop (hash-table-size m))
         (insert "{}"))
        (t
         (let ((first t)
               (indent-string (make-string indent ?\s)))
           (maphash
            (lambda (k v)
              (if (not first)
                  (insert "\n" indent-string)
                (if auto-indent
                    (let ((curr-indent (yaml--encode-auto-detect-indent)))
                      (when (> curr-indent indent)
                        (setq indent (+ curr-indent 1)))
                      (insert (make-string (- indent curr-indent) ?\s)))
                  (insert "\n" indent-string))
                (setq first nil))
              (yaml--encode-object k indent nil)
              (insert ": ")
              (yaml--encode-object v (+ indent 2)))
            m)))))

(provide 'yaml)

;;; yaml.el ends here
