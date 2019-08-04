;;; core-toggle.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
- spacemacs/toggle-NAME-status returns non-nil if the toggle is on

Additional sets of functions are created when the toggle is major mode
specific (i.e. it uses the keyword `:evil-leader-for-mode'):
- spacemacs/toggle-NAME-register-on-hook-MODE to add a hook to call the toggle on
  function
- spacemacs/toggle-NAME-on-unregister-hook-MODE to remove the hook
- spacemacs/toggle-NAME-on-register-hooks to add hooks for all supported major modes
- spacemacs/toggle-NAME-on-unregister-hooks to remove all the hooks

Available PROPS:

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

`:off-message EXPRESSION'
    EXPRESSION is evaluated and displayed when the \"off\" toggle is activated.

`:mode SYMBOL'
    If given, must be a minor mode. This overrides `:on', `:off' and `:status'.

All properties supported by `spacemacs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((wrapper-func (intern (format "spacemacs/toggle-%s"
                                       (symbol-name name))))
         (wrapper-func-status (intern (format "%s-status" wrapper-func)))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (mode (plist-get props :mode))
         (status (or mode (plist-get props :status)))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (if mode `((,mode)) (spacemacs/mplist-get-values props :on)))
         (off-body (if mode `((,mode -1)) (spacemacs/mplist-get-values props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (off-message (plist-get props :off-message))
         (evil-leader-for-mode (spacemacs/mplist-get-values props :evil-leader-for-mode))
         (supported-modes-string (mapconcat (lambda (x) (symbol-name (car x)))
                                            evil-leader-for-mode ", "))
         (bindkeys (spacemacs//create-key-binding-form props wrapper-func))
         ;; we evaluate condition and status only if they are a list or
         ;; a bound symbol
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status))
         (condition-eval (if condition
                             `(and (or (and (symbolp ',condition)
                                            (boundp ',condition))
                                       (listp ',condition))
                                   ,condition)
                           t)))
    `(progn
       (let ((properties (append '(:function ,wrapper-func :predicate ,wrapper-func-status)
                                 ',props))
             (cell (assq ',name spacemacs-toggles)))
         (if cell
             (setcdr cell properties)
           (push (cons ',name properties) spacemacs-toggles)))
       ;; toggle function
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "Toggle %s on and off.%s"
                  (symbol-name name)
                  (if doc (concat "\n\n" doc) ""))
         ,(if prefix-arg-var '(interactive "P") '(interactive))
         (if ,condition-eval
             ;; check if current buffer major mode supports the toggle
             (if (and ',evil-leader-for-mode
                      (not (assq major-mode ',evil-leader-for-mode)))
                 (message (concat
                           "Toggle: %S\n"
                           "This toggle is not supported with major mode: %S\n"
                           "Supported major modes are: %s")
                          ',name
                          major-mode
                          ,supported-modes-string
                          )
               (if (,wrapper-func-status)
                   (progn ,@off-body
                          (when (called-interactively-p 'any)
                            (message ,(or off-message (format "%s disabled." name)))))
                 ,@on-body
                 (when (called-interactively-p 'any)
                   (message ,(or on-message (format "%s enabled." name))))))
           (message (concat
                     "Toggle: %S\n"
                     "This toggle is not supported.")
                    ',name)))
       ;; predicate function
       (defun ,wrapper-func-status ()
         ,(format "Check if %s is on." (symbol-name name))
         (and ,condition-eval ,status-eval))
       ;; Only define on or off functions when status is available
       ,@(when status
           `(
             ;; on function
             (defun ,wrapper-func-on ()
               ,(format "Toggle %s on." (symbol-name name))
               (interactive)
               (unless (,wrapper-func-status) (,wrapper-func)))
             ;; off function
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when (,wrapper-func-status) (,wrapper-func)))
             ;; on and off functions for each mode specific toggles
             ,@(when evil-leader-for-mode
                 (let ((wrapper-func-register-hooks
                        (intern (format "%s-register-hooks" wrapper-func-on)))
                       (wrapper-func-unregister-hooks
                        (intern (format "%s-unregister-hooks" wrapper-func-on)))
                       wrapper-mode-funcs)
                   ;; register all hooks to turn on toggle
                   (push `(defun ,wrapper-func-register-hooks ()
                            ,(format (concat
                                      "Register hooks to toggle %s on for all "
                                      "supported buffers.\n"
                                      "Supported buffer major modes are: %s")
                                     (symbol-name name)
                                     supported-modes-string)
                            (interactive)
                            (dolist (m ',(mapcar 'car evil-leader-for-mode))
                              (let ((mode-hook (intern (format "%s-hook" m))))
                                (add-hook mode-hook ',wrapper-func-on))))
                         wrapper-mode-funcs)
                   ;; unregister all hooks to turn on toggle
                   (push `(defun ,wrapper-func-unregister-hooks ()
                            ,(format (concat
                                      "Unregister hooks to toggle %s on for all"
                                      " supported buffers.\n"
                                      "Supported buffer major modes are: %s")
                                     (symbol-name name)
                                     supported-modes-string)
                            (interactive)
                            (dolist (m ',(mapcar 'car evil-leader-for-mode))
                              (let ((mode-hook (intern (format "%s-hook" m))))
                                (remove-hook mode-hook ',wrapper-func-on))))
                         wrapper-mode-funcs)
                   (dolist (m (mapcar 'car evil-leader-for-mode))
                     (let* ((mode-hook (intern (format "%s-hook" m)))
                            (wrapper-func-register-hook
                             (intern (format "%s-register-hook-%s"
                                             wrapper-func-on m)))
                            (wrapper-func-unregister-hook
                             (intern (format "%s-unregister-hook-%s"
                                             wrapper-func-on m))))
                       ;; register hook to turn on toggle
                       (push `(defun ,wrapper-func-register-hook ()
                                ,(format (concat
                                          "Register hook to toggle %s on for "
                                          "all `%s' buffers.")
                                         (symbol-name name) m)
                                (interactive)
                                (add-hook ',mode-hook ',wrapper-func-on))
                             wrapper-mode-funcs)
                       ;; unregister hook to turn on toggle
                       (push `(defun ,wrapper-func-unregister-hook ()
                                ,(format (concat
                                          "Unregister hook to toggle %s off for"
                                          " all `%s' buffers.")
                                         (symbol-name name) m)
                                (interactive)
                                (remove-hook ',mode-hook ',wrapper-func-on))
                             wrapper-mode-funcs)))
                   wrapper-mode-funcs))))
       ,@bindkeys)))

(provide 'core-toggle)
