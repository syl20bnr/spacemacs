;;; core-toggles.el --- Spacemacs Core File
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

`:evil-leader STRING'
    A key sequence string to be set with `evil-leader/set-key'.

`:evil-leader-for-mode CONS CELL'
    A cons cell (MODE . KEY) where MODE is a major-mode symbol and KEY is a
    key sequence string to be set with `evil-leader/set-key-for-mode'.

`:global-key STRING'
    A key sequence string to be set with `global-set-key'.

`:define-key CONS CELL'
    A cons cell (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'. "
  (let* ((wrapper-func (intern (format "spacemacs/toggle-%s"
                                       (symbol-name name))))
         (status (plist-get props :status))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (spacemacs/mplist-get props :on))
         (off-body (spacemacs/mplist-get props :off))
         (evil-leader (plist-get props :evil-leader))
         (evil-leader-for-mode (plist-get props :evil-leader-for-mode))
         (global-key (plist-get props :global-key))
         (def-key (plist-get props :define-key)))
    (push (append (list name) (list :function wrapper-func) props)
          spacemacs-toggles)
    `(progn
       ;; toggle function
       (defun ,wrapper-func ()
         ,(format "Toggle %s on and off." (symbol-name name))
         (interactive)
         ;; we evaluate condition and status only if they are a list or
         ;; a bound symbol
         (if (or (null ',condition)
                   (and (or (and (symbolp ',condition) (boundp ',condition))
                            (listp ',condition))
                        ,condition))
             (if (and (or (and (symbolp ',status) (boundp ',status))
                          (listp ',status))
                      ,status) (progn ,@off-body) ,@on-body)
           (message "This toggle is not supported.")))
       ;; key bindings
       (when ,evil-leader
         (evil-leader/set-key ,evil-leader ',wrapper-func))
       (when ,evil-leader-for-mode
         (evil-leader/set-key-for-mode
           '(car ,evil-leader-for-mode)
           (cdr ,evil-leader-for-mode) ',wrapper-func))
       (when ,global-key
         (global-set-key (kbd ,global-key) ',wrapper-func))
       (when ,def-key
         (define-key (car ,def-key) (kbd ,(cdr def-key)) ',wrapper-func)))))

(provide 'core-toggles)
