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

`:function FUNCTION'
    A symbol of a function to handle the toggle or one of the following
    symbols: `global' or `globalized'.

    `global' define a default function to handle the toggle of a minor mode
    defined with the property `:global t'

    `globalized' define a default function to handle the toogle of a minor
    mode defined with `define-globalized-minor-mode'.

`:documentation STRING'
    A docstring to describe what the toggle does.

`:key STRING'
    A key sequence to use the toggle."
  (let* ((wrapper-func (intern (format "spacemacs/toggle-%s"
                                       (symbol-name name))))
         (toggle-func (plist-get props :toggle-function))
         (toggle-var (if (plist-get props :toggle-variable)
                         (plist-get props :toggle-variable)
                       toggle-func))
         (doc (plist-get props :documentation))
         (on-body (spacemacs/mplist-get props :on))
         (off-body (spacemacs/mplist-get props :off))
         (key (plist-get props :key)))
    (push `(,name :func ,wrapper-func :doc ,doc :key ,key)
          spacemacs-toggles)
    `(progn
       (defun ,wrapper-func ()
         ,(format "Toggle %s on and off." (symbol-name name))
         (interactive)
         (if (and (boundp ',toggle-var) ,toggle-var)
             (progn
               (,toggle-func -1)
               ,@on-body)
           (,toggle-func)
           ,@off-body))
       (when ,key
         (evil-leader/set-key ,key ',wrapper-func)))))

(provide 'core-toggles)
