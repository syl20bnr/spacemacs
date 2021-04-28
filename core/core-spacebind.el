;;; core-spacebind.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'core-keybindings)

(defvar spacebind--eager-bind t
  "If true bind keys right after `spacmeacs|spacebind' macro-expanse.
Otherwise binding happens at the next event loop.")

;;;; Binding stacks
(defvar spacebind--bs-minor-mode-replacements '()
  "Binding stack for `spacemacs/add-key-based-replacements-for-minor-mode'.")
(defvar spacebind--bs-major-mode-replacements '()
  "Binding stack for `which-key-add-major-mode-key-based-replacements'.")
(defvar spacebind--bs-declare-prefix '()
  "Binding stack for `spacemacs/declare-prefix'.")
(defvar spacebind--bs-declare-prefix-for-mode '()
  "Binding stack for `spacemacs/declare-prefix-for-mode'.")
(defvar spacebind--bs-leader-keys '()
  "Binding stack for `spacemacs/set-leader-keys'.")
(defvar spacebind--bs-leader-keys-for-major-mode '()
  "Binding stack for `spacemacs/set-leader-keys-for-major-mode'.")
(defvar spacebind--bs-leader-keys-for-minor-mode '()
  "Binding stack for `spacemacs/set-leader-keys-for-minor-mode'.")
(defvar spacebind--bs-global-replacements '()
  "Binding stack for `which-key-add-key-based-replacements'.")
(defvar spacebind--bs-fn-key-seq-override '()
  "Binding stack for `spacemacs/add-which-key-fn-key-seq-override'.")

(defvar spacebind--timer [t]
  "`run-with-idle-timer' return value for `spacebind//process-bind-stack'.")

(defun spacebind//process-bind-stack ()
  "Binds keys and prefixes popping the binding stacks."
  (unwind-protect
      (progn
        ;; `spacemacs/add-key-based-replacements-for-minor-mode'
        (dolist (args spacebind--bs-minor-mode-replacements)
          (let ((mode (car args))
                (keys (string-join (append `(,(spacemacs/leader-key))
                                           (cadr args))
                                   " "))
                (label (caddr args)))
            (spacemacs/add-key-based-replacements-for-minor-mode
             mode keys label)))

        ;; `which-key-add-major-mode-key-based-replacements'
        (dolist (args spacebind--bs-major-mode-replacements)
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
            (spacemacs/declare-prefix-for-mode mode prefix label)))

        ;; `spacemacs/set-leader-keys'
        (dolist (args spacebind--bs-leader-keys)
          (let ((keys (string-join (car args) " "))
                (fn-sym (cadr args)))
            (spacemacs/set-leader-keys keys fn-sym)))

        ;; `spacemacs/set-leader-keys-for-major-mode'
        (dolist (args spacebind--bs-leader-keys-for-major-mode)
          (let ((mode (car args))
                (keys (string-join (cadr args) " "))
                (fn-sym (caddr args)))
            (spacemacs/set-leader-keys-for-major-mode mode keys fn-sym)))

        ;; `spacemacs/set-leader-keys-for-minor-mode'
        (dolist (args spacebind--bs-leader-keys-for-minor-mode)
          (let ((mode (car args))
                (keys (string-join (cadr args) " "))
                (fn-sym (caddr args)))
            (spacemacs/set-leader-keys-for-minor-mode mode keys fn-sym)))

        ;; `which-key-add-key-based-replacements'
        (dolist (args spacebind--bs-global-replacements)
          (let ((keys (string-join (append `(,(spacemacs/leader-key))
                                           (car args))
                                   " "))
                (label (cadr args)))
            (which-key-add-key-based-replacements keys label)))

        ;; `spacemacs/add-which-key-fn-key-seq-override'
        (dolist (args spacebind--bs-fn-key-seq-override)
          (let ((sym (car args))
                (rep (cadr args))
                (label (caddr args)))
            (spacemacs/add-which-key-fn-key-seq-override sym rep label))))

    ;; Reset stacks
    (setq spacebind--bs-global-replacements nil
          spacebind--bs-leader-keys-for-minor-mode nil
          spacebind--bs-leader-keys-for-major-mode nil
          spacebind--bs-leader-keys nil
          spacebind--bs-declare-prefix-for-mode nil
          spacebind--bs-declare-prefix nil
          spacebind--bs-major-mode-replacements nil
          spacebind--bs-minor-mode-replacements nil
          spacebind--bs-fn-key-seq-override nil
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

(defun spacebind//strip-docstring (mode-binding-form)
  "Remove second element of MODE-BINDING-FORM if it is a string."
  (if (stringp (cadr mode-binding-form))
      (cons (car mode-binding-form) (cddr mode-binding-form))
    mode-binding-form))

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

(defun spacemacs/add-which-key-fn-key-seq-override
    (fn-name key-rep label)
  "Replace FN-NAME function's key sequence with KEY-REP and LABEL."
  (push `((nil . ,fn-name) . (,key-rep . ,label)) which-key-replacement-alist))

(cl-defstruct spacemacs--spacebind-state
  "State object for `spacebind' macro implementation.
CTYPE - current binding type.
BSTACK - plist of generated key-binding stacks of the shape:
(:minor-mode-replacements <STACK>
 :major-mode-replacements <STACK>
 :declare-prefix <STACK>
 :declare-prefix-for-mode <STACK>
 :set-leader-keys <STACK>
 :set-leader-keys-for-major-mode <STACK>
 :set-leader-keys-for-minor-mode <STACK>
 :global-replacements <STACK>
 :fn-key-seq-override <STACK>)"
  ctype bstack)

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
  "Apply STATE method from ctype slot to SEXP."
  (funcall (spacemacs--spacebind-state-ctype state) state sexp)
  state)

(cl-defmethod spacemacs//spacebind-dispatch ((state spacemacs--spacebind-state)
                                             (_ string))
  "Ignore strings - used to implement doc-strings."
  state)

(defun spacemacs//spacebind-form-visitor (form path k-fn p-fn)
  "Applies K-FN to FORM if it is a key binding form. Otherwise applies P-FN.
PATH passed to the applied function.
NOTE: This function strips all newline characters, replaces successive spaces
with a singular in string elements of FORM and trims tails of function labels
delimited by \"|\" character."
  (list
   (cl-labels ((apply-str-fmt
                (el)
                (thread-last el
                  ;; Convert new lines and multiply spaces into singular.
                  ;; This is done to enable better code formatting.
                  (replace-regexp-in-string "[\n[:space:]]+" " ")
                  ;; Discard everything after | symbol in labels.
                  ;; This way we can add extra text into the README.org
                  ;; files while omitting it in labels.
                  (replace-regexp-in-string "[[:punct:][:space:]]*|.+" "")))
               (str-fmt-rec
                (depth el)
                (cond
                 ((stringp el) (apply-str-fmt el))
                 ((and (= depth 0)
                       (listp el))
                  ;; We don't want to go deeper than a single level.
                  (mapcar (apply-partially #'str-fmt-rec (1+ depth)) el))
                 (t el)))
               (str-fmt
                (el)
                (str-fmt-rec 0 el)))
     (cl-destructuring-bind
         (key-or-prefix-form
          leader-label-or-fn-symbol
          leader-label-or-next-form)
         (mapcar #'str-fmt (seq-take form 3))
       (let ((full-key-or-prefix (append
                                  path
                                  ;; ("key" :label "label") or "key".
                                  `(,(or (car-safe key-or-prefix-form)
                                         key-or-prefix-form))))
             (key-or-prefix-label (thread-first key-or-prefix-form
                                    (cdr-safe)
                                    (plist-get :label))))
         (if (symbolp leader-label-or-fn-symbol)
             (funcall k-fn
                      full-key-or-prefix
                      key-or-prefix-label
                      leader-label-or-fn-symbol
                      ;; Either "label" or ("doc label" :label "label").
                      (or (thread-first leader-label-or-next-form
                            (cdr-safe)
                            (plist-get :label))
                          leader-label-or-next-form))
           (funcall p-fn full-key-or-prefix leader-label-or-fn-symbol)))))))

(defun spacemacs//spacebind-form-walker-rec (path k-fn p-fn form)
  "Recursive body of `spacemacs//spacebind-form-walker'."
  (let* ((fn-sym-or-label (car-safe (cdr-safe form)))
         (prefix-form? (stringp fn-sym-or-label))
         (binding-form? (and fn-sym-or-label (symbolp fn-sym-or-label)))
         (list-of-forms? (and form (cl-every #'consp form)))
         (binding-or-prefix-form? (or binding-form?
                                      prefix-form?))
         (head (car form))
         (cur-path (if prefix-form?
                       (append path `(,(or (car-safe head)
                                           head)))
                     path))
         ;; Strip key and label from prefix forms.
         (bindings (if prefix-form?
                       (cddr form)
                     form)))
    (append
     (when binding-or-prefix-form?
       (spacemacs//spacebind-form-visitor form path k-fn p-fn))
     (when (or prefix-form?
               list-of-forms?)
       (seq-mapcat
        (apply-partially
         'spacemacs//spacebind-form-walker-rec cur-path k-fn p-fn)
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

(defun spacemacs//spacebind-stack-push (state stack-k sexp)
  "Push SEXP onto stack specified by STACK-K in the STATE bstack plist slot."
  (cl-callf (lambda (bs)
              (plist-put bs stack-k (cons sexp (plist-get bs stack-k))))
      (spacemacs--spacebind-state-bstack state))
  state)

;;;; Key bindings - keywords handlers

(cl-defmethod :global ((state spacemacs--spacebind-state) form)
  "Interpreter for global binding forms."
  (spacemacs//spacebind-form-walker
   ;; Strip optional doc-string.
   (if (char-or-string-p (car form))
       (cdr form)
     form)
   (lambda (key-seq key-label fn-symbol label)
     (spacemacs//spacebind-stack-push
      state
      :global-replacements `(,key-seq ,label))
     (spacemacs//spacebind-stack-push
      state
      :set-leader-keys `(,key-seq ,fn-symbol))
     (when key-label
       (spacemacs//spacebind-stack-push
        state
        :fn-key-seq-override `(,(symbol-name fn-symbol)
                               ,key-label
                               ,label))))
   (lambda (key-prefix label)
     (spacemacs//spacebind-stack-push
      state
      :declare-prefix `(,key-prefix ,label)))))

(cl-defmethod :major ((state spacemacs--spacebind-state) form)
  "Interpreter for major mode binding forms."
  (let* ((form (spacebind//strip-docstring form))
         (mode (pop form)))
    (spacemacs//spacebind-form-walker
     form
     (lambda (key-seq key-label fn-symbol label)
       (spacemacs//spacebind-stack-push
        state
        :major-mode-replacements `(,mode ,key-seq ,label))
       (spacemacs//spacebind-stack-push
        state
        :set-leader-keys-for-major-mode `(,mode ,key-seq ,fn-symbol))
       (when key-label
         (spacemacs//spacebind-stack-push
          state
          :fn-key-seq-override `(,(symbol-name fn-symbol)
                                 ,key-label
                                 ,label))))
     (lambda (key-prefix label)
       (spacemacs//spacebind-stack-push
        state
        :declare-prefix-for-mode `(,mode ,key-prefix ,label))))))

(cl-defmethod :minor ((state spacemacs--spacebind-state) form)
  "Interpreter for minor mode binding forms."
  (let* ((form (spacebind//strip-docstring form))
         (mode (pop form)))
    (spacemacs//spacebind-form-walker
     form
     (lambda (key-seq key-label fn-symbol label)
       (spacemacs//spacebind-stack-push
        state
        :minor-mode-replacements `(,mode ,key-seq ,label))
       (spacemacs//spacebind-stack-push
        state
        :set-leader-keys-for-minor-mode `(,mode ,key-seq ,fn-symbol))
       (when key-label
         (spacemacs//spacebind-stack-push
          state
          :fn-key-seq-override `(,(symbol-name fn-symbol)
                                 ,key-label
                                 ,label))))
     (lambda (key-prefix label)
       (spacemacs//spacebind-stack-push
        state
        :declare-prefix-for-mode `(,mode ,key-prefix ,label))))))

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

NOTE: <TEXT> strings support formatting:
      - \n and multiply spaces are converted into single spaces in the <TEXT>.
      - Everything after and including | symbol is ignored and punctuation
        before the character trimmed. This is done so you can provide additional
        information for the binding documentation while keeping labels brief.

NOTE: You can override key labels and displayed key sequences with :label <TEXT>
      Example: ((\"k\" :label \"press k\")
                foo-fn
                (\"for docs\" :label \"displayed\"))

\(fn <<DELIMITER_KEYWORD> <BINDING_FORMS>...>...)"
  (cl-list*
   'progn
   (let ((state (make-spacemacs--spacebind-state)))
     (dolist (sexp bindings)
       ;; Fill stacks
       (spacemacs//spacebind-dispatch state sexp))
     (cl-flet ((get-stack (key) (thread-first state
                                  (spacemacs--spacebind-state-bstack)
                                  (plist-get key))))
       (seq-reduce
        (lambda (acc pair)
          (if-let ((stack (get-stack (car pair)))
                   (stack-var (cadr pair)))
              ;; We do it this way because `nconc' should have
              ;; the shortest list as the first argument.
              (append acc `(,stack-var (nconc ',stack ,stack-var)))
            acc))
        `((:minor-mode-replacements
           spacebind--bs-minor-mode-replacements)
          (:major-mode-replacements
           spacebind--bs-major-mode-replacements)
          (:declare-prefix
           spacebind--bs-declare-prefix)
          (:declare-prefix-for-mode
           spacebind--bs-declare-prefix-for-mode)
          (:set-leader-keys
           spacebind--bs-leader-keys)
          (:set-leader-keys-for-major-mode
           spacebind--bs-leader-keys-for-major-mode)
          (:set-leader-keys-for-minor-mode
           spacebind--bs-leader-keys-for-minor-mode)
          (:global-replacements
           spacebind--bs-global-replacements)
          (:fn-key-seq-override
           spacebind--bs-fn-key-seq-override))
        '(setq))))
   ;; Schedule stacks processing with `spacebind//process-bind-stack' function.
   `((when (aref spacebind--timer 0)
       (if (not spacebind--eager-bind)
           (setq spacebind--timer
                 (run-with-idle-timer 0 nil #'spacebind//process-bind-stack))
         (setq spacebind--timer [t])
         (spacebind//process-bind-stack))))))

(provide 'core-spacebind)
