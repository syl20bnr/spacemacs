;;; core-spacebind.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'core-keybindings)

;; TODO: Make this configurable.
(define-inline spacemacs/major-mode-prefix ()
  "Get current prefix for major modes."
  "m")

(define-inline spacemacs/leader-key ()
  "Returns `dotspacemacs-leader-key'"
  dotspacemacs-leader-key)

(defun spacebind//strip-docstring (binding-form)
  "Remove second element of BINDING-FORM if it is a string."
  (if (stringp (cadr binding-form))
      (cons (car binding-form) (cddr binding-form))
    binding-form))

(defun spacebind//nosp (str)
  "Remove all white-spaces from STR."
  (replace-regexp-in-string " +" "" str))

(defun spacemacs/add-key-based-replacements-for-minor-mode
    (mode key-sequence replacement &rest more)
  "Proxy for `which-key-add-key-based-replacements'
MODE currently ignored.
FIXME: We should disable replacements when the mode is disabled.
The function `which-key-add-major-mode-key-based-replacements' might give
a clue how to do this.
Also there is discussion about the feature:
https://github.com/justbur/emacs-which-key/issues/212"
  (apply #'which-key-add-key-based-replacements key-sequence replacement more))

(cl-defstruct spacemacs--spacebind-state
  "State object for `spacebind' macro implementation.
CTYPE - current binding type.
RSEXP - accumulator with the macro output.
This structure has one interpreter method for each supported CTYPE.
CTYPE is a type of a currently processed binding."
  ctype rsexp)

(cl-defgeneric spacemacs//spacebind-dispatch (state binding)
  (:documentation "Based on BINDING type modify STATE using BINDING value."))

(cl-defmethod spacemacs//spacebind-dispatch ((state spacemacs--spacebind-state)
                                             (keyword-or-symbol symbol))
  "If KEYWORD is a keyword, set STATE slot ctype(current type) to its value.
If KEYWORD is a non-keyword symbol (without \":\" prefix in the name), wrap
its value into a list and re-apply the function to it."
  (if (not (keywordp keyword-or-symbol))
      (spacemacs//spacebind-dispatch state (list keyword-or-symbol))
    (setf (spacemacs--spacebind-state-ctype state) keyword-or-symbol)
    state))

(cl-defmethod spacemacs//spacebind-dispatch ((state spacemacs--spacebind-state)
                                             (sexp list))
  "Apply STATE method from ctype slot to SEXP and append output to rsexp slot."
  (cl-callf append (spacemacs--spacebind-state-rsexp state)
    (funcall (spacemacs--spacebind-state-ctype state) state sexp))
  state)

(cl-defmethod spacemacs//spacebind-dispatch ((state spacemacs--spacebind-state)
                                             (_ string))
  "Append STR to the RSEXP of STATE. Can be used as a doc-string."
  state)

(defun spacemacs//spacebind-form-visitor (form path k-fn p-fn)
  "Applies K-FN to FORM if it is a key binding form. Otherwise applies P-FN.
PATH passed to the applied function.
NOTE: This function strips all newline characters, replaces successive spaces
with a singular in string elements of FORM and trims tails of function labels
delimited by \"|\" character."
  (when-let ((fm (and (stringp (car-safe form))
                      (seq-take form 3))))
    (list
     (cl-destructuring-bind
         (key-or-prefix
          leader-label-or-fn-symbol
          leader-label-or-next-form)
         (mapcar (lambda (el)
                   (if (stringp el)
                       (replace-regexp-in-string "[\n[:space:]]+" " " el)
                     el))
                 fm)
       (let ((full-key-or-prefix (concat path " " key-or-prefix)))
         (if (symbolp leader-label-or-fn-symbol)
             (funcall k-fn
                      full-key-or-prefix
                      leader-label-or-fn-symbol
                      (replace-regexp-in-string
                       "[[:punct:][:space:]]*|.*"
                       ""
                       leader-label-or-next-form))
           (funcall p-fn
                    full-key-or-prefix
                    leader-label-or-fn-symbol)))))))

(defun spacemacs//spacebind-form-walker-rec (path k-fn p-fn form)
  "Recursive body of `spacemacs//spacebind-form-walker'."
  (append
   (spacemacs//spacebind-form-visitor form path k-fn p-fn)
   (let* ((is-prefix-form (stringp (cadr form)))
          (cur-path (if is-prefix-form
                        (concat path (car form))
                      path))
          ;; Strip key and label from prefix forms.
          (bindings (if is-prefix-form
                        (cddr form)
                      form)))
     ;; Is it a list of bind forms?
     (when (consp (car-safe bindings))
       (seq-mapcat
        (apply-partially
         'spacemacs//spacebind-form-walker-rec
         cur-path
         k-fn
         p-fn)
        bindings)))))

(defun spacemacs//spacebind-form-walker (b-forms k-fn p-fn)
  "Part of `spacemacs--spacebind-state' interpreters implementation.
B-FORMS is a root node of a binding tree without mode (car of the root form).
K-FN called for each key binding node with 3 arguments: full_key_sequence,
function_symbol and label_for_leader_menu.
P-FN called for each prefix binding node with 2 arguments:
full_key_prefix_sequence and label_for_leader_menu.
Both K-FN and P-FN should return binding evaluation forms.
The forms will be concatenated and substituted by `spacebind' macro."
  (spacemacs//spacebind-form-walker-rec "" k-fn p-fn b-forms))

;; Key bindings - keywords handlers

(cl-defmethod :print-debug ((_ spacemacs--spacebind-state) form)
  "`message' logging interpreter for debugging."
  (let* ((form (spacebind//strip-docstring form))
         (mode (pop form)))
    (spacemacs//spacebind-form-walker
     form
     (lambda (key-seq fn-symbol label)
       `(message "Key binding visitor args: key-seq: %S fn-symbol: %S label: %S"
                 ,key-seq ,fn-symbol ,label))
     (lambda (key-prefix label)
       `(message "Prefix binding visitor args: key-prefix: %S label: %S"
                 ,key-prefix ,label)))))

(cl-defmethod :global ((_ spacemacs--spacebind-state) form)
  "Interpreter for global binding forms."
  (spacemacs//spacebind-form-walker
   ;; Strip optional doc-string.
   (if (char-or-string-p (car form))
       (cdr form)
     form)
   (lambda (key-seq fn-symbol label)
     `(progn
        (which-key-add-key-based-replacements
          (concat (spacemacs/leader-key) " " ,key-seq)
          ,label)
        (spacemacs/set-leader-keys ,(spacebind//nosp key-seq) ',fn-symbol)))
   (lambda (key-prefix label)
     `(spacemacs/declare-prefix ,(spacebind//nosp key-prefix) ,label))))

(cl-defmethod :major ((_ spacemacs--spacebind-state) form)
  "Interpreter for major mode binding forms."
  (let* ((form (spacebind//strip-docstring form))
         (mode (pop form)))
    (spacemacs//spacebind-form-walker
     form
     (lambda (key-seq fn-symbol label)
       `(progn
          (which-key-add-major-mode-key-based-replacements
            ',mode
            (string-join
             (list (spacemacs/leader-key)
                   (spacemacs/major-mode-prefix)
                   ,key-seq)
             " ")
            ,label)
          (spacemacs/set-leader-keys-for-major-mode
            ',mode
            (concat (spacemacs/major-mode-prefix) ,(spacebind//nosp key-seq))
            ',fn-symbol)))
     (lambda (key-prefix label)
       `(spacemacs/declare-prefix-for-mode
          ',mode
          (concat (spacemacs/major-mode-prefix) ,(spacebind//nosp key-prefix))
          ,label)))))

(cl-defmethod :minor ((_ spacemacs--spacebind-state) form)
  "Interpreter for minor mode binding forms."
  (let* ((form (spacebind//strip-docstring form))
         (mode (pop form)))
    (spacemacs//spacebind-form-walker
     form
     (lambda (key-seq fn-symbol label)
       `(progn
          (spacemacs/add-key-based-replacements-for-minor-mode
           ',mode
           (concat (spacemacs/leader-key) " " ,key-seq)
           ,label)
          (spacemacs/set-leader-keys-for-minor-mode
            ',mode
            ,(spacebind//nosp key-seq)
            ',fn-symbol)))
     (lambda (key-prefix label)
       `(spacemacs/declare-prefix-for-mode
          ',mode
          ,(spacebind//nosp key-prefix)
          ,label)))))

(defmacro spacebind (&rest bindings)
  "Bind keys and their prefixes declared via BINDINGS tree like structure.
BINDINGS format:
 <DELIMITER_KEYWORD>
  <BINDING_FORM>
  <BINDING_FORM>
  ...
 <DELIMITER_KEYWORD>
  <BINDING_FORM>
  ...
 ...

DELIMITER_KEYWORD - specifies a type of following <BINDING_FORM> (or forms).
Currently supported types: :major, :minor and :global.

:major and :minor bindings have this shape:
(<MODE>
 <OPTIONAL_DOC_STRING>
 <PREFIX_OR_BINDING>
 <PREFIX_OR_BINDING>
 ...)

:global forms have a similar shape:
(<OPTIONAL_DOC_STRING>
 <PREFIX_OR_BINDING>
 <PREFIX_OR_BINDING>
 ...)

<OPTIONAL_DOC_STRING> is a string that will be used to generate a key-bindings
section in the corresponding README.org files.

<PREFIX_OR_BINDING> is a recursive form that can be:
  A prefix form:
  (<PREFIX_KEY> <TEXT> <PREFIX_OR_BINDING>)
  Or a key-binding form:
  (<KEY> <FUNCTION_SYMBOL> <TEXT>)

<TEXT> is what will be displayed in the menu and used for the documentation
generation.

<PREFIX_KEY> and <KEY> are singular keys represented as strings.

<FUNCTION_SYMBOL> is the function that will be bound to the <KEY>.

See core-spacebind-utest.el for examples.

NOTE: This macro also has `use-package' integration via `:spacebind' key

\(fn <<DELIMITER_KEYWORD> <BINDING_FORMS>...>...)"
  (spacemacs--spacebind-state-rsexp
   (seq-reduce 'spacemacs//spacebind-dispatch
               bindings
               (make-spacemacs--spacebind-state
                :rsexp `(progn)))))

(provide 'core-spacebind)
