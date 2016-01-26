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

(defun spacemacs//transient-state-body-func-name (name)
  "Return the name of the transient state function."
  (intern (format "spacemacs/%S-transient-state/body" name)))

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

(defface spacemacs-transient-state-title-face
  '((t :inherit header-line))
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
`:title STRING'
   Provide a title in the header of the transient state
`:columns INTEGER'
    Automatically generate :doc with this many number of columns.
`:hint BOOLEAN'
    Whether to automatically add hints to the docstring. Default is nil.
`:foreign-keys SYMBOL'
    What to do when keys not bound in the transient state are entered. This
    can be nil (default), which means to exit the transient state, warn,
    which means to not exit but warn the user that the key is not part
    of the transient state, or run, which means to try to run the key binding
    without exiting.
`:entry-binding MAP KEY'
    Key binding to use for entering the transient state.
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
         (body-func (spacemacs//transient-state-body-func-name name))
         (entry-binding (spacemacs/mplist-get props :entry-binding))
         (add-bindings (intern (format "spacemacs-%s-transient-state-add-bindings"
                                       name)))
         (remove-bindings (intern (format "spacemacs-%s-transient-state-remove-bindings"
                                       name)))
         (bindings (spacemacs/mplist-get props :bindings))
         (doc (or (plist-get props :doc) "\n"))
         (title (plist-get props :title))
         (hint-var (intern (format "%s/hint" func)))
         (columns (plist-get props :columns))
         (entry-sexp (plist-get props :on-enter))
         (exit-sexp (plist-get props :on-exit))
         (hint (plist-get props :hint))
         (foreign-keys (plist-get props :foreign-keys))
         (bindkeys (spacemacs//create-key-binding-form props body-func)))
    `(progn
       (spacemacs/defer-until-after-user-config
        '(lambda ()
           (eval
            (append
             '(defhydra ,func
                (,(car entry-binding) ,(cadr entry-binding)
                 :hint ,hint
                 :columns ,columns
                 :foreign-keys ,foreign-keys
                 :body-pre ,entry-sexp
                 :before-exit ,exit-sexp)
                ,doc)
             (spacemacs//transient-state-adjust-bindings
              ',bindings ',remove-bindings ',add-bindings)))
           (when ,title
             (setq ,hint-var
                   (list 'concat
                     (propertize ,title
                                 'face 'spacemacs-transient-state-title-face)
                     "\n" ,hint-var "\nColor Guide: ["
                     (propertize "KEY" 'face 'hydra-face-blue)
                     "] exits transient state  ["
                     (propertize "KEY" 'face 'hydra-face-red)
                     "] will not exit")))
           ,@bindkeys)))))

(provide 'core-transient-state)
