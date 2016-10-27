;;; core-toggle.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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

`:prefix SYMBOL'
    SYMBOL is bound to the raw value of prefix-arg (same as calling
    (interactive \"P\")) in the wrapper function.

`:on-message EXPRESSION'
    EXPRESSION is evaluated and displayed when the \"on\" toggle is activated.

`:mode SYMBOL'
    If given, must be a minor mode. This overrides `:on', `:off' and `:status'.

All properties supported by `spacemacs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((wrapper-func (intern (format "spacemacs/toggle-%s"
                                       (symbol-name name))))
         (wrapper-func-status (intern (format "%s-p" wrapper-func)))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (mode (plist-get props :mode))
         (status (or mode (plist-get props :status)))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (if mode `((,mode)) (spacemacs/mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (spacemacs/mplist-get props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (bindkeys (spacemacs//create-key-binding-form props wrapper-func))
         ;; we evaluate condition and status only if they are a list or
         ;; a bound symbol
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status))
         (condition-eval `(or (null ',condition)
                              (and (or (and (symbolp ',condition) (boundp ',condition))
                                       (listp ',condition))
                                   ,condition))))
    `(progn
       (push (append '(,name) '(:function ,wrapper-func
                                          :predicate ,wrapper-func-status) ',props)
             spacemacs-toggles)
       ;; toggle function
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "Toggle %s on and off." (symbol-name name))
         ,(if prefix-arg-var '(interactive "P") '(interactive))
         (if ,condition-eval
             (if (,wrapper-func-status)
                 (progn ,@off-body
                        (when (called-interactively-p 'any)
                          (message ,(format "%s disabled." name))))
               ,@on-body
               (when (called-interactively-p 'any)
                 (message ,(or on-message (format "%s enabled." name)))))
           (message "This toggle is not supported.")))
       ;; predicate function
       (defun ,wrapper-func-status ()
         ,(format "Check if %s is on." (symbol-name name))
         (and ,condition-eval ,status-eval))
       ;; Only define on- or off-functions when status is available
       ,@(when status
           ;; on-function
           `((defun ,wrapper-func-on ()
               ,(format "Toggle %s on." (symbol-name name))
               (interactive)
               (unless (,wrapper-func-status) (,wrapper-func)))
             ;; off-function
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when (,wrapper-func-status) (,wrapper-func)))))
       ,@bindkeys)))

(provide 'core-toggle)
