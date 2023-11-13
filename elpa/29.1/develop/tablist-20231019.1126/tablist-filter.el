;;; tablist-filter.el --- Filter expressions for tablists.  -*- lexical-binding:t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: extensions, lisp

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

;;

(defvar python-mode-hook)
(let (python-mode-hook)                 ;FIXME: Why?
(require 'semantic/wisent/comp)
(require 'semantic/wisent/wisent))

;;; Code:

(defvar wisent-eoi-term)
(declare-function wisent-parse "semantic/wisent/wisent.el")

;;
;; *Variables
;;

(defvar tablist-filter-binary-operator
  '((== . tablist-filter-op-equal)
    (=~ . tablist-filter-op-regexp)
    (< . tablist-filter-op-<)
    (> . tablist-filter-op->)
    (<= . tablist-filter-op-<=)
    (>= . tablist-filter-op->=)
    (= . tablist-filter-op-=)))

(defvar tablist-filter-unary-operator nil)

(defvar tablist-filter-wisent-parser nil)

(defvar tablist-filter-lexer-regexps nil)

(defvar tablist-filter-wisent-grammar
  '(
    ;; terminals
    ;; Use lowercase for better looking error messages.
    (operand unary-operator binary-operator or and not)

    ;; terminal associativity & precedence
    ((left binary-operator)
     (left unary-operator)
     (left or)
     (left and)
     (left not))

    ;; rules
    (filter-or-empty
     ((nil))
     ((?\( ?\)) nil)
     ((filter) $1))
    (filter
     ((operand) $1) ;;Named filter
     ((operand binary-operator operand) `(,(intern $2) ,$1 ,$3))
     ((unary-operator operand) `(,(intern $1) ,$2))
     ((not filter) `(not ,$2))
     ((filter and filter) `(and ,$1 ,$3))
     ((filter or filter) `(or ,$1 ,$3))
     ((?\( filter ?\)) $2))))

;;
;; *Filter Parsing
;;

(defun tablist-filter-parser-init (&optional reinitialize interactive)
  (interactive (list t t))
  (unless (and tablist-filter-lexer-regexps
               (not reinitialize))
    (let ((re (mapcar
               (lambda (l)
                 (let ((re (regexp-opt
                            (mapcar 'symbol-name
                                    (mapcar 'car l)) t)))
                   (if (= (length re) 0)
                       ".\\`" ;;matches nothing
                     re)))
               (list tablist-filter-binary-operator
                     tablist-filter-unary-operator))))
      (setq tablist-filter-lexer-regexps
            (nreverse
             (cons (concat "\\(?:" (car re) "\\|" (cadr re)
                           "\\|[ \t\f\r\n\"!()]\\|&&\\|||\\)")
                   re)))))
  (unless (and tablist-filter-wisent-parser
               (not reinitialize))
    (let ((wisent-compile-grammar*
           (symbol-function
            'wisent-compile-grammar)))
      (setq tablist-filter-wisent-parser
            ;; Trick the byte-compile into not using the byte-compile
            ;; handler in semantic/wisent/comp.el, since it does not
            ;; always work (wisent-context-compile-grammar n/a).
            (funcall wisent-compile-grammar*
                     tablist-filter-wisent-grammar))))
  (when interactive
    (message "Parser reinitialized."))
  nil)

(defun tablist-filter-wisent-lexer ()
  (cl-destructuring-bind (unary-op binary-op keywords)
      tablist-filter-lexer-regexps
    (skip-chars-forward " \t\f\r\n")
    (cond
     ((eobp) (list wisent-eoi-term))
     ((= ?\" (char-after))
      `(operand , (condition-case err
                    (read (current-buffer))
                  (error (signal (car err) (cons
                                            "invalid lisp string"
                                            (cdr err)))))))
     ((looking-at unary-op)
      (goto-char (match-end 0))
      `(unary-operator ,(match-string-no-properties 0)))
     ((looking-at binary-op)
      (goto-char (match-end 0))
      `(binary-operator ,(match-string-no-properties 0)))
     ((looking-at "&&")
      (forward-char 2)
      `(and "&&"))
     ((looking-at "||")
      (forward-char 2)
      `(or "||"))
     ((= ?! (char-after))
      (forward-char)
      `(not "!"))
     ((= ?\( (char-after))
      (forward-char)
      `(?\( "("))
     ((= ?\) (char-after))
      (forward-char)
      `(?\) ")"))
     (t
      (let ((beg (point)))
        (when (re-search-forward keywords nil 'move)
          (goto-char (match-beginning 0)))
        `(operand ,(buffer-substring-no-properties
                  beg
                  (point))))))))

(defun tablist-filter-parse (filter)
  (interactive "sFilter: ")
  (tablist-filter-parser-init)
  (with-temp-buffer
    (save-excursion (insert filter))
    (condition-case error
        (wisent-parse tablist-filter-wisent-parser
                      'tablist-filter-wisent-lexer
                      (lambda (msg)
                        (signal 'error
                                (replace-regexp-in-string
                                 "\\$EOI" "end of input"
                                 msg t t))))
      (error
       (signal 'error
               (append (if (consp (cdr error))
                           (cdr error)
                         (list (cdr error)))
                       (list (point))))))))

(defun tablist-filter-unparse (filter &optional noerror)
  (cl-labels
    ((unparse (filter &optional noerror)
       (cond
        ((stringp filter)
         (if (or (string-match (nth 2 tablist-filter-lexer-regexps)
                               filter)
                 (= 0 (length filter)))
             (format "%S" filter)
           filter))
        ((and (eq (car-safe filter) 'not)
              (= (length filter) 2))
         (let ((paren (memq (car-safe (nth 1 filter)) '(or and))))
           (format "!%s%s%s"
                   (if paren "(" "")
                   (unparse (cadr filter) noerror)
                   (if paren ")" ""))))
        ((and (memq (car-safe filter) '(and or))
              (= (length filter) 3))
         (let ((lparen (and (eq (car filter) 'and)
                            (eq 'or (car-safe (car-safe (cdr filter))))))
               (rparen (and (eq (car filter) 'and)
                            (eq 'or (car-safe (car-safe (cddr filter)))))))
           (format "%s%s%s %s %s%s%s"
                   (if lparen "(" "")
                   (unparse (cadr filter) noerror)
                   (if lparen ")" "")
                   (cl-case (car filter)
                     (and "&&") (or "||"))
                   (if rparen "(" "")
                   (unparse (car (cddr filter)) noerror)
                   (if rparen ")" ""))))
        ((and (assq (car-safe filter) tablist-filter-binary-operator)
              (= (length filter) 3))
         (format "%s %s %s"
                 (unparse (cadr filter) noerror)
                 (car filter)
                 (unparse (car (cddr filter)) noerror)))
        ((and (assq (car-safe filter) tablist-filter-unary-operator)
              (= (length filter) 2))
         (format "%s %s"
                 (car filter)
                 (unparse (cadr filter) noerror)))
        ((not filter) "")
        (t (funcall (if noerror 'format 'error)
                    "Invalid filter: %s" filter)))))
    (tablist-filter-parser-init)
    (unparse filter noerror)))

(defun tablist-filter-eval (filter id entry &optional named-alist)
  (cl-labels
    ((feval (filter)
       (pcase filter
         (`(not . ,(and operand (guard (not (cdr operand)))))
          (not (feval (car operand))))
         (`(and . ,(and operands (guard (= 2 (length operands)))))
          (and
           (feval (nth 0 operands))
           (feval (nth 1 operands))))
         (`(or . ,(and operands (guard (= 2 (length operands)))))
          (or
           (feval (nth 0 operands))
           (feval (nth 1 operands))))
         (`(,op . ,(and operands (guard (= (length operands) 1))))
          (let ((fn (assq op tablist-filter-unary-operator)))
            (unless fn
              (error "Undefined unary operator: %s" op))
            (funcall fn id entry (car operands))))
         (`(,op . ,(and operands (guard (= (length operands) 2))))
          (let ((fn (cdr (assq op tablist-filter-binary-operator))))
            (unless fn
              (error "Undefined binary operator: %s" op))
            (funcall fn id entry (car operands)
                     (cadr operands))))
         ((guard (stringp filter))
          (let ((fn (cdr (assoc filter named-alist))))
            (unless fn
              (error "Undefined named filter: %s" filter))
            (if (functionp fn)
                (funcall fn id entry))
            (feval
             (if (stringp fn) (tablist-filter-unparse fn) fn))))
         (`nil t)
         (_ (error "Invalid filter: %s" filter)))))
    (feval filter)))

;;
;; *Filter Operators
;;

(defun tablist-filter-get-item-by-name (entry col-name)
  (let* ((col (cl-position col-name tabulated-list-format
                           :key 'car
                           :test
                           (lambda (s1 s2)
                             (eq t (compare-strings
                                    s1 nil nil s2 nil nil t)))))
         (item (and col (elt entry col))))
    (unless col
      (error "No such column: %s" col-name))
    (if (consp item)                  ;(LABEL . PROPS)
        (car item)
      item)))

(defun tablist-filter-op-equal (_id entry op1 op2)
  "COLUMN == STRING : Matches if COLUMN's entry is equal to STRING."
  (let ((item (tablist-filter-get-item-by-name entry op1)))
    (string= item op2)))

(defun tablist-filter-op-regexp (_id entry op1 op2)
  "COLUMN =~ REGEXP : Matches if COLUMN's entry matches REGEXP."
  (let ((item (tablist-filter-get-item-by-name entry op1)))
    (string-match op2 item)))

(defun tablist-filter-op-< (id entry op1 op2)
  "COLUMN < NUMBER : Matches if COLUMN's entry is less than NUMBER."
  (tablist-filter-op-numeric '< id entry op1 op2))

(defun tablist-filter-op-> (id entry op1 op2)
  "COLUMN > NUMBER : Matches if COLUMN's entry is greater than NUMBER."
  (tablist-filter-op-numeric '> id entry op1 op2))

(defun tablist-filter-op-<= (id entry op1 op2)
  "COLUMN <= NUMBER : Matches if COLUMN's entry is less than or equal to NUMBER."
  (tablist-filter-op-numeric '<= id entry op1 op2))

(defun tablist-filter-op->= (id entry op1 op2)
  "COLUMN >= NUMBER : Matches if COLUMN's entry is greater than or equal to NUMBER."
  (tablist-filter-op-numeric '>= id entry op1 op2))

(defun tablist-filter-op-= (id entry op1 op2)
  "COLUMN = NUMBER : Matches if COLUMN's entry as a number is equal to NUMBER."
  (tablist-filter-op-numeric '= id entry op1 op2))

(defun tablist-filter-op-numeric (op _id entry op1 op2)
  (let ((item (tablist-filter-get-item-by-name entry op1)))
    (funcall op (string-to-number item)
             (string-to-number op2))))

(defun tablist-filter-help (&optional temporary)
  (interactive)
  (cl-labels
    ((princ-op (op)
       (princ (car op))
       (princ (concat (make-string (max 0 (- 4 (length (symbol-name (car op)))))
                                   ?\s)
                      "- "
                      (car (split-string
                            (or (documentation (cdr op))
                                (format "FIXME: Not documented: %s"
                                        (cdr op)))
                            "\n" t))
                      "\n"))))
    (with-temp-buffer-window
     "*Help*"
     (if temporary
         '((lambda (buf alist)
             (let ((win
                    (or (display-buffer-reuse-window buf alist)
                        (display-buffer-in-side-window buf alist))))
               (fit-window-to-buffer win)
               win))
           (side . bottom)))
     nil
     (princ "Filter entries with the following operators.\n\n")
     (princ "&&  - FILTER1 && FILTER2 : Locical and.\n")
     (princ "||  - FILTER1 || FILTER2 : Locical or.\n")
     (dolist (op tablist-filter-binary-operator)
       (princ-op op))
     (princ "!  - ! FILTER : Locical not.\n\n")
     (dolist (op tablist-filter-unary-operator)
       (princ-op op))
     (princ "\"...\" may be used to quote names and values if necessary,
and \(...\) to group expressions.")
     (with-current-buffer standard-output
       (help-mode)))))

;;
;; *Filter Functions
;;

;; filter ::= nil | named | fn | (OP OP1 [OP2])

(defun tablist-filter-negate (filter)
  "Return a filter not matching filter."
  (cond
   ((eq (car-safe filter) 'not)
    (cadr filter))
   (filter
    (list 'not filter))))

(defun tablist-filter-push (filter new-filter &optional or-p)
  "Return a filter combining FILTER and NEW-FILTER.

By default the filters are and'ed, unless OR-P is non-nil."
  (if (or (null filter)
          (null new-filter))
      (or filter
          new-filter)
    (list (if or-p 'or 'and)
          filter new-filter)))

(defun tablist-filter-pop (filter)
  "Remove the first operator or operand from filter.

If filter starts with a negation, return filter unnegated,
if filter starts with a dis- or conjunction, remove the first operand,
if filter is nil, raise an error,
else return nil."
  (pcase filter
    (`(,(or `and `or) . ,tail)
     (car (cdr tail)))
    (`(not . ,op1)
     (car op1))
    (_ (unless filter
         (error "Filter is empty")))))

(defun tablist-filter-map (fn filter)
  (pcase filter
    (`(,(or `and `or `not) . ,tail)
     (cons (car filter)
           (mapcar (lambda (f)
                     (tablist-filter-map fn f))
                   tail)))
    (_ (funcall fn filter))))

;;
;; *Reading Filter
;;

(defvar tablist-filter-edit-history nil)
(defvar tablist-filter-edit-display-help t)

(defun tablist-filter-edit-filter (prompt &optional
                                          initial-filter history
                                          validate-fn)
  (let* ((str (tablist-filter-unparse initial-filter))
         (filter initial-filter)
         (validate-fn (or validate-fn 'identity))
         error done)
    (save-window-excursion
      (when tablist-filter-edit-display-help
        (tablist-filter-help t))
      (while (not done)
        (minibuffer-with-setup-hook
            (lambda ()
              (when error
                (when (car error)
                  (goto-char (+ (field-beginning)
                                (car error)))
                  (skip-chars-backward " \t\n"))
                (minibuffer-message "%s" (cdr error))
                (setq error nil)))
          (setq str (propertize
                     (read-string prompt str
                                  (or history 'tablist-filter-edit-history)))
                done t))
        (condition-case err
            (progn
              (setq filter (tablist-filter-parse str))
              (funcall validate-fn filter))
          (error
           (setq done nil)
           (setq error (cons (car-safe (cddr err)) nil))
           (when (car error)
             (setq str (with-temp-buffer
                         (insert str)
                         (goto-char (car error))
                         (set-text-properties
                          (progn
                            (skip-chars-backward " \t\n")
                            (backward-char)
                            (point))
                          (min (car error) (point-max))
                          '(face error rear-nonsticky t))
                         (buffer-string))))
           (setcdr error (error-message-string err)))))
      filter)))

(provide 'tablist-filter)
;; Local Variables:
;; outline-regexp: ";;\\(\\(?:[;*]+ \\| \\*+\\)[^\s\t\n]\\|###autoload\\)\\|("
;; indent-tabs-mode: nil
;; End:
;;; tablist-filter.el ends here
