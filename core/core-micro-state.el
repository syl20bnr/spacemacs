;;; -*- lexical-binding: t -*-
;;; core-micro-state.el --- Spacemacs Core File
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

(defmacro spacemacs|define-micro-state (name &rest props)
  "Define a micro-state called NAME.

NAME is a symbol.

Available PROPS:

`:on-enter BODY'
    Evaluate BODY when the micro-state is switched on.

`:on-exit BODY'
    Evaluate BODY when leaving the micro-state.

`:documentation BODY'
    Evaluate BODY which must return a string

`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form
    (STRING1 SYMBOL1 :documentation STRING :exit SYMBOL)
    where:
    - STRING1 is a key to bound to the function SYMBOL1.
    - :documentation STRING is a doc string (not used for now)
    - :exit SYMBOL is either `:exit t' or `:exit nil', if non nil then
      pressing this key will leave the micro-state (default is nil)."
  (declare (indent 1))
  (let* ((func (spacemacs//micro-state-func-name name))
         (doc (spacemacs/mplist-get props :documentation))
         (on-enter (spacemacs/mplist-get props :on-enter))
         (on-exit (spacemacs/mplist-get props :on-exit))
         (bindings (spacemacs/mplist-get props :bindings))
         (wrappers (spacemacs//micro-state-create-wrappers name doc bindings))
         (keymap-body (spacemacs//micro-state-fill-map-sexps wrappers)))
    `(defun ,func ()
       ,(format "%s micro-state." (symbol-name name))
       (interactive)
       (let ((doc ,@doc)) (when doc (echo doc)))
       ,@on-enter
       (,(if (version< emacs-version "24.4")
             'set-temporary-overlay-map
           'set-transient-map)
        (let ((map (make-sparse-keymap)))
          ,@keymap-body map) ',(spacemacs//micro-state-create-exit-func
                                name wrappers on-exit))))) 

(defun spacemacs//micro-state-func-name (name)
  "Return the name of the micro-state function."
  (intern (format "spacemacs/%s-micro-state" (symbol-name name))))

(defun spacemacs//micro-state-create-wrappers (name doc bindings)
  "Return an alist (key wrapper) for each binding in BINDINGS."
  (mapcar (lambda (x) (spacemacs//micro-state-create-wrapper name doc x))
          bindings))

(defun spacemacs//micro-state-create-wrapper (name doc binding)
  "Create a wrapper of FUNC and return a tuple (key wrapper BINDING)."
  (let* ((wrapped (cadr binding))
         (wrapper-name (intern (format "spacemacs//%s-%s" (symbol-name name)
                                       (symbol-name wrapped))))
         (wrapper-func (eval `(defun ,wrapper-name ()
                                "Auto-generated function"
                                (interactive)
                                (let ((doc ,@doc)) (when doc (echo doc)))
                                (when ',wrapped
                                  (call-interactively ',wrapped))))))
    (append (list (car binding) wrapper-func) binding)))

(defun spacemacs//micro-state-fill-map-sexps (wrappers)
  "Return a list of `define-key' sexp to fill the micro-state temporary map."
  (mapcar (lambda (x) `(define-key map ,(kbd (car x)) ',(cadr x)))
          wrappers))

(defun spacemacs//micro-state-create-exit-func (name wrappers on-exit)
  "Return a function to execute when leaving the micro-state.

The returned function returns nil if the executed command exits the
micro-state."
  (let ((func (intern (format "spacemacs//%s-on-exit" name))))
    (eval `(defun ,func ()
             "Function executed after each micro-state command."
             (if (reduce (lambda (x y) (or x y))
                         (mapcar (lambda (x)
                                   (spacemacs//micro-state-stay? ',name x))
                                 ',wrappers)
                         :initial-value nil)
                 't ,@on-exit nil)))))

(defun spacemacs//micro-state-stay? (name wrapper)
  "Return non nil if WRAPPER does not leave the micro-state."
  (let ((micro-state-fun (spacemacs//micro-state-func-name name))
        (key (car wrapper))
        (func (cadr wrapper)))
    (when (and (or (eq this-command micro-state-fun)
                   (eq this-command func))
               (equal (this-command-keys) (kbd key)))
      (not (plist-get wrapper :exit)))))

(provide 'core-micro-state)
