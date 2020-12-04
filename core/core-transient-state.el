;;; -*- lexical-binding: t -*-
;;; core-transient-state.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//transient-state-func-name (name)
  "Return the name of the transient state function."
  (intern (format "space-macs/%S-transient-state" name)))

(defun space-macs//transient-state-props-var-name (name)
  "Return the name of the variable use to store the transient state properties."
  (intern (format "space-macs--%S-transient-state-props" name)))

(defun space-macs//transient-state-body-func-name (name)
  "Return the name of the transient state function."
  (intern (format "space-macs/%S-transient-state/body" name)))

(defun space-macs//transient-state-heads-name (name)
  "Return the name of the transient state heads variable which
holds the key bindings."
  (intern (format "space-macs/%S-transient-state/heads" name)))

(defun space-macs//transient-state-add-bindings-name (name)
  "Return the name of the transient state add-bindings variable which
may hold the additional key bindings. The variable may be unbound."
  (intern (format "space-macs-%s-transient-state-add-bindings" name)))

(defun space-macs//transient-state-remove-bindings-name (name)
  "Return the name of the transient state remove-bindings variable which
may hold the keys to be removed. The variable may be unbound."
  (intern (format "space-macs-%s-transient-state-remove-bindings" name)))

(defun space-macs//transient-state-adjust-bindings (bindings to-remove to-add)
  (append
   (cl-remove-if
    (lambda (bnd)
      (and (boundp to-remove)
           (listp (symbol-value to-remove))
           (member (car bnd) (symbol-value to-remove))))
    bindings)
   (when (and (boundp to-add)
              (listp (symbol-value to-add)))
     (symbol-value to-add))))

(defun space-macs//transient-state-make-doc
    (transient-state docstring &optional body)
  "Use `hydra' internal function to format and apply DOCSTRING."
  (let ((heads (space-macs//transient-state-heads-name transient-state)))
    (setq body (if body body '(nil nil :hint nil :foreign-keys nil)))
    (eval
     (hydra--format nil body docstring (symbol-value heads)))))

(defun space-macs/transient-state-register-add-bindings (name bindings)
  "Register additional BINDINGS for the transient state NAME.

BINDINGS should be a list of Hydra head definitions. See `defhydra'.

Since a transient state initializes its Hydra right after
the `dotspace-macs/user-config', this function will have no
effect if called after that point."
  (declare (indent defun))
  (let ((var-name (space-macs//transient-state-add-bindings-name name)))
    (or (boundp var-name)
        (set var-name '()))
    (set var-name (append (symbol-value var-name) bindings))))

(defun space-macs/transient-state-register-remove-bindings (name keys)
  "Register KEYS to be removed from the transient state NAME.

KEYS should be a list of strings.

Since a transient state initializes its Hydra right after
the `dotspace-macs/user-config', this function will have no
effect if called after that point."
  (declare (indent defun))
  (let ((var-name (space-macs//transient-state-remove-bindings-name name)))
    (or (boundp var-name)
        (set var-name '()))
    (set var-name (append (symbol-value var-name) keys))))

(defmacro space-macs|transient-state-format-hint (name var hint)
  "Format HINT and store the result in VAR for transient state NAME."
  (declare (indent 1))
  `(add-hook 'space-macs-post-user-config-hook
             (lambda ()
               (let* ((props-var ,(space-macs//transient-state-props-var-name
                                   name))
                      (prop-hint (cadr (assq 'hint props-var)))
                      (prop-columns (cadr (assq 'columns props-var)))
                      (prop-foreign-keys (cadr (assq 'foreign-keys props-var)))
                      (prop-entry-sexp (cadr (assq 'entry-sexp props-var)))
                      (prop-exit-sexp (cadr (assq 'exit-sexp props-var))))
                 (setq ,var (space-macs//transient-state-make-doc
                             ',name
                             ,hint
                             `(nil
                               nil
                               :hint ,prop-hint
                               :columns ,prop-columns
                               :foreign-keys ,prop-foreign-keys
                               :body-pre ,prop-entry-sexp
                               :before-exit ,prop-exit-sexp)))
                 'append))))

(defface space-macs-transient-state-title-face
  `((t :inherit mode-line))
  "Face for title of transient states.")

(defmacro space-macs|define-transient-state (name &rest props)
  "Define a transient state called NAME.
NAME is a symbol.
Available PROPS:
`:on-enter SEXP'
    Evaluate SEXP when the transient state is switched on.
`:on-exit SEXP'
    Evaluate SEXP when leaving the transient state.
`:doc STRING or SEXP'
    A docstring supported by `defhydra'.
`:additional-docs cons cells (VARIABLE . STRING)'
    Additional docstrings to format and store in the corresponding VARIABLE.
    This can be used to dynamically change the docstring.
`:title STRING'
    Provide a title in the header of the transient state
`:columns INTEGER'
    Automatically generate :doc with this many number of columns.
`:hint BOOLEAN'
    Whether to display hints. Default is nil.
`:hint-is-doc BOOLEAN'
    Whether the hints act as a documentation, the only effect of this value is
    to change where the hints are displayed. If non-nil the hints are displayed
    on the same line as the `:title', otherwise they are displayed below it.
    Default is nil.
`:dynamic-hint SEXP'
    An sexp evaluating to a string for dynamic hinting.
    When provided `:hint' has no effect. Default is nil.
`:foreign-keys SYMBOL'
    What to do when keys not bound in the transient state are entered. This
    can be nil (default), which means to exit the transient state, warn,
    which means to not exit but warn the user that the key is not part
    of the transient state, or run, which means to try to run the key binding
    without exiting.
`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form
    (STRING1 SYMBOL1 DOCSTRING
                     :exit SYMBOL)
    where:
    - STRING1 is a key to be bound to the function or key map SYMBOL1.
    - DOCSTRING is a STRING or an SEXP that evaluates to a string
    - :exit SYMBOL or SEXP, if non nil then pressing this key will
      leave the transient state (default is nil).
      Important note: due to inner working of transient-maps in e-macs
      the `:exit' keyword is evaluate *before* the actual execution
      of the bound command.
All properties supported by `space-macs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((func (space-macs//transient-state-func-name name))
         (props-var (space-macs//transient-state-props-var-name name))
         (body-func (space-macs//transient-state-body-func-name name))
         (add-bindings (space-macs//transient-state-add-bindings-name name))
         (remove-bindings (space-macs//transient-state-remove-bindings-name name))
         (bindings (space-macs/mplist-get-values props :bindings))
         (doc (or (plist-get props :doc) "\n"))
         (title (plist-get props :title))
         (hint-var (intern (format "%s/hint" func)))
         (columns (plist-get props :columns))
         (entry-sexp (plist-get props :on-enter))
         (exit-sexp (plist-get props :on-exit))
         (hint (plist-get props :hint))
         (hint-doc-p (plist-get props :hint-is-doc))
         (dyn-hint (plist-get props :dynamic-hint))
         (additional-docs (space-macs/mplist-get-values props :additional-docs))
         (foreign-keys (plist-get props :foreign-keys))
         (bindkeys (space-macs//create-key-binding-form props body-func)))
    `(progn
       (defvar ,props-var nil
         ,(format (concat "Association list containing a copy of some "
                          "properties of the transient state %S. Those "
                          "properties are used in macro "
                          "`space-macs|transient-state-format-hint'.") name))
       (add-to-list ',props-var '(hint ,hint))
       (add-to-list ',props-var '(columns ,columns))
       (add-to-list ',props-var '(foreign-keys ,foreign-keys))
       (add-to-list ',props-var '(entry-sexp ,entry-sexp))
       (add-to-list ',props-var '(exit-sexp ,exit-sexp))
       (space-macs/defer-until-after-user-config
        '(lambda ()
           (eval
            (append
             '(defhydra ,func
                (nil nil
                 :hint ,hint
                 :columns ,columns
                 :foreign-keys ,foreign-keys
                 :body-pre ,entry-sexp
                 :before-exit ,exit-sexp)
                ,doc)
             (space-macs//transient-state-adjust-bindings
              ',bindings ',remove-bindings ',add-bindings)))
           (when ,title
             (let ((guide (concat "[" (propertize "KEY" 'face 'hydra-face-blue)
                                  "] exits state  ["
                                  (if ',foreign-keys
                                      (propertize "KEY" 'face 'hydra-face-pink)
                                    (propertize "KEY" 'face 'hydra-face-red))
                                  "] will not exit")))
               ;; (add-face-text-property 0 (length guide) '(:height 0.9) t guide)
               (add-face-text-property 0 (length guide) 'italic t guide)
               (setq ,hint-var
                     (list 'concat
                           (when dotspace-macs-show-transient-state-title
                             (concat
                              (propertize
                               ,title
                               'face 'space-macs-transient-state-title-face)
                              (if ,hint-doc-p " " "\n"))) ,hint-var
                              ',dyn-hint
                              (when dotspace-macs-show-transient-state-color-guide
                                (concat "\n" guide))))))
           ,@bindkeys)))))

(provide 'core-transient-state)


