;;; -*- lexical-binding: t -*-
;;; core-micro-states.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defmacro spacemacs/define-micro-state (name &rest props)
  "Define a micro-state called NAME.

NAME is a symbol.

Available PROPS:

`:on-enter BODY'
    Evaluate BODY when the micro-state is switched on.

`:on-exit BODY'
    Evaluate BODDY when leaving the micro-state.

`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form (STRING SYMBOL) where STRING
    is a key to bound to the function SYMBOL."
  (declare (indent 1))
  (let* ((func (intern (format "spacemacs/%s-micro-state" (symbol-name name))))
         (on-enter (spacemacs/mplist-get props :on-enter))
         (on-exit (spacemacs/mplist-get props :on-exit))
         (bindings (spacemacs/mplist-get props :bindings))
         (wrappers (spacemacs//micro-state-create-wrappers name bindings))
         (keymap-body (spacemacs//micro-state-fill-map-sexps wrappers)))
    `(defun ,func ()
       ,(format "%s micro-state." (symbol-name name))
       (interactive)
       ,@on-enter
       (,(if (version< emacs-version "24.4")
             'set-temporary-overlay-map
           'set-transient-map)
        (let ((map (make-sparse-keymap)))
          ,@keymap-body map) ',(spacemacs//micro-state-create-exit-func
                                name wrappers on-exit))))) 

(defun spacemacs//micro-state-create-wrappers (name bindings)
  "Return an alist (key wrapper) for each binding in BINDINGS."
  (mapcar (lambda (x)
            (apply 'spacemacs//micro-state-create-wrapper
                   name x)) bindings))

(defun spacemacs//micro-state-create-wrapper (name key func)
  "Create a wrapper of FUNC and return a tuple (KEY wrapper)."
  (let* ((wrapper-name (intern (format "spacemacs//%s-%s" (symbol-name name)
                                       (symbol-name func))))
         (wrapper-func (eval `(defun ,wrapper-name ()
                                "Auto-generated function"
                                (interactive)
                                (call-interactively ',func)))))
    (cons key wrapper-func)))

(defun spacemacs//micro-state-fill-map-sexps (wrappers)
  "Return a list of `define-key' sexp to fill the micro-state temporary map."
  (mapcar (lambda (x) `(define-key map ,(car x) ',(cdr x)))
          wrappers))

(defun spacemacs//micro-state-create-exit-func (name wrappers on-exit)
  "Return a function to execute when leaving the micro-state."
  (let ((func (intern (format "spacemacs//%s-on-exit" name))))
    (eval `(defun ,func ()
             "Function executed after each micro-state command."
             (if (reduce (lambda (x y) (or x y))
                         (mapcar (lambda (x)
                                   (eq this-command (cdr x))) ',wrappers)
                         :initial-value nil)
                 't ,@on-exit nil)))))

(provide 'core-micro-states)
