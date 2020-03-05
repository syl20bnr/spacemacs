;;; core-spacebind.el --- Spacemacs Core File -*- lexical-binding: t -*-
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

(defvar spacebind--eager-bind nil
  "If true bind keys right after `spacmeacs|spacebind' macro-expanse.
Otherwise binding happens at the next event loop.")

;; Binding stacks
(defvar spacebind--bs-add-minor-mode-replacements '()
  "Binding stack for `spacemacs/add-key-based-replacements-for-minor-mode'.")
(defvar spacebind--bs-add-major-mode-replacements '()
  "Binding stack for `which-key-add-major-mode-key-based-replacements'.")
(defvar spacebind--bs-declare-prefix '()
  "Binding stack for `spacemacs/declare-prefix'.")
(defvar spacebind--bs-declare-prefix-for-mode '()
  "Binding stack for `spacemacs/declare-prefix-for-mode'.")
(defvar spacebind--bs-set-leader-keys '()
  "Binding stack for `spacemacs/set-leader-keys'.")
(defvar spacebind--bs-set-leader-keys-for-major-mode '()
  "Binding stack for `spacemacs/set-leader-keys-for-major-mode'.")
(defvar spacebind--bs-set-leader-keys-for-minor-mode '()
  "Binding stack for `spacemacs/set-leader-keys-for-minor-mode'.")
(defvar spacebind--bs-add-global-replacements '()
  "Binding stack for `which-key-add-key-based-replacements'.")

(defvar spacebind--timer [t]
  "`run-with-idle-timer' return value for `spacebind//process-bind-stack'.")

(defun spacebind//process-bind-stack ()
  "Drains bind stacks and binds keys and prefixes."
  (unwind-protect
      (progn
        ;; `spacemacs/add-key-based-replacements-for-minor-mode'
        (dolist (args spacebind--bs-add-minor-mode-replacements)
          (let ((mode (car args))
                (keys (string-join (append `(,(spacemacs/leader-key))
                                           (cadr args))
                                   " "))
                (label (caddr args)))
            (spacemacs/add-key-based-replacements-for-minor-mode
             mode keys label)))

        ;; `which-key-add-major-mode-key-based-replacements'
        (dolist (args spacebind--bs-add-major-mode-replacements)
          (let ((mode (car args))
                (keys (string-join (append `(,(spacemacs/leader-key))
                                           `(,(spacemacs/major-mode-prefix))
                                           (cadr args))
                                   " "))
                (label (caddr args)))
            (which-key-add-major-mode-key-based-replacements mode keys label)))

        ;; `spacemacs/declare-prefix'
        (dolist (args spacebind--bs-declare-prefix)
          (let ((prefix (string-join (car args) " "))
                (label (cadr args)))
            (spacemacs/declare-prefix prefix label)))

        ;; `spacemacs/declare-prefix-for-mode'
        (dolist (args spacebind--bs-declare-prefix-for-mode)
          (let ((mode (car args))
                (prefix (string-join (cadr args) " "))
                (label (caddr args)))
            (spacemacs/declare-prefix prefix label)))

        ;; `spacemacs/set-leader-keys'
        (dolist (args spacebind--bs-set-leader-keys)
          (let ((keys (string-join (car args) " "))
                (fn-sym (cadr args)))
            (spacemacs/set-leader-keys keys fn-sym)))

        ;; `spacemacs/set-leader-keys-for-major-mode'
        (dolist (args spacebind--bs-set-leader-keys-for-major-mode)
          (let ((mode (car args))
                (keys (string-join (cadr args) " "))
                (fn-sym (caddr args)))
            (spacemacs/set-leader-keys-for-major-mode mode keys fn-sym)))

        ;; `spacemacs/set-leader-keys-for-minor-mode'
        (dolist (args spacebind--bs-set-leader-keys-for-minor-mode)
          (let ((mode (car args))
                (keys (string-join (cadr args) " "))
                (fn-sym (caddr args)))
            (spacemacs/set-leader-keys-for-minor-mode mode keys fn-sym)))

        ;; `which-key-add-key-based-replacements'
        (dolist (args spacebind--bs-add-global-replacements)
          (let ((keys (string-join (append `(,(spacemacs/leader-key))
                                           (car args))
                                   " "))
                (label (cadr args)))
            (which-key-add-key-based-replacements keys label))))

    ;; Reset stacks
    (setq spacebind--bs-add-global-replacements nil
          spacebind--bs-set-leader-keys-for-minor-mode nil
          spacebind--bs-set-leader-keys-for-major-mode nil
          spacebind--bs-set-leader-keys nil
          spacebind--bs-declare-prefix-for-mode nil
          spacebind--bs-declare-prefix nil
          spacebind--bs-add-major-mode-replacements nil
          spacebind--bs-add-minor-mode-replacements nil
          ;; Reset timer var
          spacebind--timer [t])))

;; TODO: Make this configurable.
(defun spacemacs/major-mode-prefix ()
  "Get current prefix for major modes.
NOTE: `dotspacemacs-major-mode-leader-key' isn't the same."
  "m")

(defun spacemacs/leader-key ()
  "Returns `dotspacemacs-leader-key'"
  dotspacemacs-leader-key)

(defun spacebind//strip-docstring (binding-form)
  "Remove second element of BINDING-FORM if it is a string."
  (if (stringp (cadr binding-form))
      (cons (car binding-form) (cddr binding-form))
    binding-form))

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
       (let ((full-key-or-prefix (append path `(,key-or-prefix))))
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
                        (append path `(,(car form)))
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
  (spacemacs//spacebind-form-walker-rec nil k-fn p-fn b-forms))

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
        (push (list ',key-seq ,label) spacebind--bs-add-global-replacements)
        (push (list ',key-seq ',fn-symbol) spacebind--bs-set-leader-keys)))
   (lambda (key-prefix label)
     `(push (list ',key-prefix ,label) spacebind--bs-declare-prefix))))

(cl-defmethod :major ((_ spacemacs--spacebind-state) form)
  "Interpreter for major mode binding forms."
  (let* ((form (spacebind//strip-docstring form))
         (mode (pop form)))
    (spacemacs//spacebind-form-walker
     form
     (lambda (key-seq fn-symbol label)
       `(progn
          (push (list ',mode ',key-seq ,label)
                spacebind--bs-add-major-mode-replacements)
          (push (list ',mode ',key-seq ',fn-symbol)
                spacebind--bs-set-leader-keys-for-major-mode)))
     (lambda (key-prefix label)
       `(push (list ',mode ',key-prefix ,label)
              spacebind--bs-declare-prefix-for-mode)))))

(cl-defmethod :minor ((_ spacemacs--spacebind-state) form)
  "Interpreter for minor mode binding forms."
  (let* ((form (spacebind//strip-docstring form))
         (mode (pop form)))
    (spacemacs//spacebind-form-walker
     form
     (lambda (key-seq fn-symbol label)
       `(progn
          (push (list ',mode ',key-seq ,label)
                spacebind--bs-add-minor-mode-replacements)
          (push (list ',mode ',key-seq ',fn-symbol)
                spacebind--bs-set-leader-keys-for-minor-mode)))
     (lambda (key-prefix label)
       `(push (list ',mode ',key-prefix ,label)
              spacebind--bs-declare-prefix-for-mode)))))

(defmacro spacemacs|spacebind (&rest bindings)
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
  (append
   (spacemacs--spacebind-state-rsexp
    (seq-reduce 'spacemacs//spacebind-dispatch
                bindings
                (make-spacemacs--spacebind-state
                 :rsexp `(progn))))
   `((if (not spacebind--eager-bind)
         (when (aref spacebind--timer 0)
           (setq spacebind--timer
                 (run-with-idle-timer 0 nil #'spacebind//process-bind-stack)))
       (when (timerp spacebind--timer)
         (cancel-timer spacebind--timer))
       (spacebind//process-bind-stack)))))

(provide 'core-spacebind)
