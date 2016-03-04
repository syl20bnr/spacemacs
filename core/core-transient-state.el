;;; -*- lexical-binding: t -*-
;;; core-transient-state.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//transient-state-func-name (name)
  "Return the name of the transient state function."
  (intern (format "spacemacs/%S-transient-state" name)))

(defun spacemacs//transient-state-props-var-name (name)
  "Return the name of the variable use to store the transient state properties."
  (intern (format "spacemacs--%S-transient-state-props" name)))

(defun spacemacs//transient-state-body-func-name (name)
  "Return the name of the transient state function."
  (intern (format "spacemacs/%S-transient-state/body" name)))

(defun spacemacs//transient-state-heads-name (name)
  "Return the name of the transient state heads variable which
holds the key bindings."
  (intern (format "spacemacs/%S-transient-state/heads" name)))

(defun spacemacs//transient-state-adjust-bindings (bindings to-remove to-add)
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

(defun spacemacs//transient-state-make-doc
    (transient-state docstring &optional body)
  "Use `hydra' internal function to format and apply DOCSTRING."
  (let ((heads (spacemacs//transient-state-heads-name transient-state)))
    (setq body (if body body '(nil nil :hint nil :foreign-keys nil)))
    (eval
     (hydra--format nil body docstring (symbol-value heads)))))

(defmacro spacemacs|transient-state-format-hint (name var hint)
  "Format HINT and store the result in VAR for transient state NAME."
  (declare (indent 1))
  `(add-hook 'spacemacs-post-user-config-hook
             (lambda ()
               (let* ((props-var ,(spacemacs//transient-state-props-var-name
                                   name))
                      (prop-hint (cadr (assq 'hint props-var)))
                      (prop-columns (cadr (assq 'columns props-var)))
                      (prop-foreign-keys (cadr (assq 'foreign-keys props-var)))
                      (prop-entry-sexp (cadr (assq 'entry-sexp props-var)))
                      (prop-exit-sexp (cadr (assq 'exit-sexp props-var))))
                 (setq ,var (spacemacs//transient-state-make-doc
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

(defface spacemacs-transient-state-title-face
  `((t :inherit mode-line))
  "Face for title of transient states.")

(defmacro spacemacs|define-transient-state (name &rest props)
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
      Important note: due to inner working of transient-maps in Emacs
      the `:exit' keyword is evaluate *before* the actual execution
      of the bound command.
All properties supported by `spacemacs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((func (spacemacs//transient-state-func-name name))
         (props-var (spacemacs//transient-state-props-var-name name))
         (body-func (spacemacs//transient-state-body-func-name name))
         (add-bindings
          (intern (format "spacemacs-%s-transient-state-add-bindings" name)))
         (remove-bindings
          (intern (format "spacemacs-%s-transient-state-remove-bindings" name)))
         (bindings (spacemacs/mplist-get props :bindings))
         (doc (or (plist-get props :doc) "\n"))
         (title (plist-get props :title))
         (hint-var (intern (format "%s/hint" func)))
         (columns (plist-get props :columns))
         (entry-sexp (plist-get props :on-enter))
         (exit-sexp (plist-get props :on-exit))
         (hint (plist-get props :hint))
         (hint-doc-p (plist-get props :hint-is-doc))
         (dyn-hint (plist-get props :dynamic-hint))
         (additional-docs (spacemacs/mplist-get props :additional-docs))
         (foreign-keys (plist-get props :foreign-keys))
         (bindkeys (spacemacs//create-key-binding-form props body-func)))
    `(progn
       (defvar ,props-var nil
         ,(format (concat "Association list containing a copy of some "
                          "properties of the transient state %S. Those "
                          "properties are used in macro "
                          "`spacemacs|transient-state-format-hint'.") name))
       (add-to-list ',props-var '(hint ,hint))
       (add-to-list ',props-var '(columns ,columns))
       (add-to-list ',props-var '(foreign-keys ,foreign-keys))
       (add-to-list ',props-var '(entry-sexp ,entry-sexp))
       (add-to-list ',props-var '(exit-sexp ,exit-sexp))
       (spacemacs/defer-until-after-user-config
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
             (spacemacs//transient-state-adjust-bindings
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
                           (when dotspacemacs-show-transient-state-title
                             (concat
                              (propertize
                               ,title
                               'face 'spacemacs-transient-state-title-face)
                              (if ,hint-doc-p " " "\n"))) ,hint-var
                              ',dyn-hint
                              (when dotspacemacs-show-transient-state-color-guide
                                (concat "\n" guide))))))
           ,@bindkeys)))))

(provide 'core-transient-state)
