;;; core-toggle.el --- Spacemacs Core File
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
(require 'core-funcs)

(defvar spacemacs-toggles '()
  "List of all declared toggles. The structure of an element is a
property list (name :func FUNCTION :doc STRING :key STRING).")

(defmacro spacemacs|add-toggle (name &rest props)
  "Add a toggle with NAME symbol.

This macro creates the following functions:
- spacemacs/toggle-NAME switches on or off depending on the current state
- spacemacs/toggle-NAME-on only switches on if currently disabled
- spacemacs/toggle-NAME-off only switches off if currently enabled

Avaiblabe PROPS:

`:status EXPRESSION'
    The EXPRESSION to evaluate to get the current status of the toggle.

`:if EXPRESSION'
    If this EXPRESSION evaluate to nil then no attempt to update the toggle
    status will be performed.

`:on BODY'
    Evaluate BODY when the toggle is switched on.

`:off BODY'
    Evaluate BODY when the toggle is switched off.

`:documentation STRING'
    STRING describes what the toggle does.

All properties supported by `spacemacs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((wrapper-func (intern (format "spacemacs/toggle-%s"
                                       (symbol-name name))))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (status (plist-get props :status))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (spacemacs/mplist-get props :on))
         (off-body (spacemacs/mplist-get props :off))
         (bindkeys (spacemacs//create-key-binding-form props wrapper-func))
         ;; we evaluate condition and status only if they are a list or
         ;; a bound symbol
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))
    `(progn
       (push (append '(,name) '(:function ,wrapper-func) ',props)
             spacemacs-toggles)
       ;; toggle function
       (defun ,wrapper-func ()
         ,(format "Toggle %s on and off." (symbol-name name))
         (interactive)
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (if ,status-eval
                 (progn ,@off-body
                        (message ,(format "%s disabled." name)))
               ,@on-body
               (message ,(format "%s enabled." name)))
           (message "This toggle is not supported.")))
       ;; Only define on- or off-functions when status is available
       ,@(when status
           ;; on-function
           `((defun ,wrapper-func-on ()
               ,(format "Toggle %s on." (symbol-name name))
               (interactive)
               (unless ,status-eval (,wrapper-func)))
             ;; off-function
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when ,status-eval (,wrapper-func)))))
       ,@bindkeys)))

(provide 'core-toggle)
