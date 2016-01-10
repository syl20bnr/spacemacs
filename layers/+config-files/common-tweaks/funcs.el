;;; funcs.el --- common-tweaks Layer packages File for Spacemacs
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


(defun ct//replace-in-list-rec (lst elem repl)
  "Replace recursively all occurrences of `elem' by `repl' in the list `lst'."
  (declare (indent 0))
  (if (typep lst 'list)
      (let* ((body-position (cl-position elem lst)))
        (if body-position
            ;; The element is in the list, replace it
            (progn
              (setf (nth body-position lst) repl)
              lst)
          ;; The element is not in the list, recurse
          (dolist (l lst)
            (ct//replace-in-list-rec l elem repl))))))


(defmacro ct|tweak (name &rest props)
  "Macro used for defining a tweak.

Usage:

    (ct|tweak tweak-name
       [:property value]...)

:disable       Boolean, whether the tweak is disabled or not.
:description   String, documents what the tweak does.
:functions     Code, functions definitions.
:loader        Code, used to load the tweak. Must contains `BODY'.
               where the real tweak should be placed.
:tweak         Code, the tweak's code
:pre           Code executed before the tweak, without being wrapped inside
               the `:loader'.
:post          Code executed after the tweak, without being wrapped inside
               the `:loader'.

All properties are optional, except for `:tweak'."
  (declare (indent 1))
  (let* ((disable (plist-get props :disable))
         (description (plist-get props :description))
         (functions (plist-get props :functions))
         (loader (plist-get props :loader))
         (tweak (plist-get props :tweak))
         (pre (plist-get props :pre))
         (post (plist-get props :post))
         (intro (concat  "This variable is part of the `common-tweaks' layer.\n"
                         "Read the `README.org' there for more information."))
         (body tweak))
    ;; If the tweak is not disabled
    (when (not disable)
      ;; Define the variable
      (eval `(defvar ,name nil ,(concat intro "\n\n" description)))
      ;; Use the `:loader' if it exists
      (when loader
        (ct//replace-in-list-rec loader 'BODY body)
        (setq body loader))
      ;; If the tweak is enabled by the user
      (when (symbol-value name)
          (when dotspacemacs-verbose-loading
            (message (format "[common-tweaks] Tweak enabled: '%s'" name)))
          `(progn
             ,pre
             ,functions
             ,body
             ,post
             ,description
             )))))
