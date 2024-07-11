;;; core-customization.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'validate)
(require 'seq)
(eval-when-compile (require 'cl-lib))

(defalias 'spacemacs-customization//validate 'validate-value)

(defvar spacemacs-customization--current-group nil
  "Used to auto-set `spacemacs|defc' group.")

(defgroup spacemacs-layers nil
  "Spacemacs layers customizations."
  :group 'spacemacs
  :prefix 'spacemacs-layers-)

(defgroup spacemacs--uncustomizable nil
  "Dummy group that contains variables that can't be customized."
  :group 'spacemacs--phony-group)

(defgroup spacemacs-dotspacemacs-init nil
  "Dotspacemacs init customizations."
  :group 'spacemacs--uncustomizable)

(defgroup spacemacs-dotspacemacs-layers nil
  "Dotspacemacs layers customizations."
  :group 'spacemacs--uncustomizable)

(defconst spacemacs-customization-uncustomizable-groups
  '(spacemacs--uncustomizable
    spacemacs-dotspacemacs-init
    spacemacs-dotspacemacs-layers)
  "List of variable groups that can't be customized.")

(defmacro spacemacs|defc (symbol standard doc type &optional group-override safe)
  "Spacemacs flavored `defcustom' for .spacemacs configurations.
SYMBOL         is the variable name; it should not be quoted.
STANDARD       is an expression specifying the variable's standard value.
DOC            is a doc-string.
TYPE           should be a widget type for editing the symbol's value.
               See Info node `(elisp) Customization Types' for a list of
               base types and useful composite types.
GROUP-OVERRIDE should be provided if you don't want Spacemacs to infer the
               configuration group from the currently configured layer name.
SAFE           should either be a function or t to be set to
               safe-local-variable property. When it's t, use TYPE to determine
               the safety.

NOTE: Use interactive function `spacemacs/customization-valid-p' to test if a
      variable has a proper type. In interactive mode it will also `message'
      variable's symbol, value and type - so you can call this function with a
      similar .spacemacs variable and use its type as an example.
NOTE: Spacemacs checks variables using validate.el package. Currently it
      doesn't support: `:inline', `plist', `coding-system', `color', `hook',
      `restricted-sexp' types so more general ones should be used instead.
NOTE: Variables defined with a group listed in
      `spacemacs-customization-uncustomizable-groups' won't appear in
      `spacemacs' customization subgroups. Also their doc-string won't provide
      customization  menu link when viewed via `describe-variable'."
  (declare (indent defun) (doc-string 3) (debug (name body)))
  `(let ((group (or ,group-override
                    spacemacs-customization--current-group
                    'spacemacs--uncustomizable)))
     (put ',symbol 'spacemacs-customization--variable t)
     (custom-declare-variable
      ',symbol
      ;; Took this from the `defcustom' implementation.
      ,(if lexical-binding
           ``(funcall #',(lambda () ,standard))
         `',standard)
      ,(format "%s\n\nTYPE: %s\n" doc type)
      :type ,type
      :group group)
     (pcase ,safe
       ('t (put ',symbol 'safe-local-variable
                (apply-partially 'spacemacs-customization//get-variable-validator
                                 ',symbol)))
       ((pred functionp) (put ',symbol 'safe-local-variable ,safe)))
     (when (memq group spacemacs-customization-uncustomizable-groups)
       ;; HACK: This will make `custom-variable-p' return nil
       ;; so the `describe-variable' function won't add customization
       ;; link. Will there be the reckoning? Will see!
       (put ',symbol 'standard-value nil)
       (put ',symbol 'custom-autoload nil))))

(defun spacemacs/customization-valid-p (var-symbol)
  "Return t if VAR-SYMBOL is a spacemacs custom variable with valid value.
Emits message with the result when called interactively."
  (interactive "v")
  (let* ((defc? (get var-symbol 'spacemacs-customization--variable))
         (val (symbol-value var-symbol))
         (type (custom-variable-type var-symbol))
         (valid? (and defc? (validate-value val type t))))
    (when (called-interactively-p 'interactive)
      (if valid?
          (message "symbol: \"%s\" value: \"%s\" type: \"%s\" is valid."
                   var-symbol val type)
        (if defc?
            (condition-case err (validate-value val type) (error (message err)))
          (message "%s is not Spacemacs customization variable" var-symbol))))
    valid?))

(defun spacemacs-customization//get-variable-validator (var-symbol val)
  "Check the validity of VAL for the spacemacs custom variable VAR-SYMBOL.

Return t if VAL is valid, and return an error when VAL is invalid or when
VAR-SYMBOL is not a spacemacs custom variable.

This function should be used with `apply-partially', and be set to the
`safe-local-variable' perperty of VAR-SYMBOL. `apply-partially' returns a
validation function that takes one argument and validate it against the scheme
of VAR-SYMBOL.

When loading local variables, `safe-local-variable-p' would call the validation
function, and it would demote any error to a message and a nil return value.
Thus it returns t when VAL is valid and nil otherwise."
  (if-let* ((_ (get var-symbol 'spacemacs-customization--variable))
            (type (custom-variable-type var-symbol)))
      (or (validate-value val type nil) t)
    (user-error "%s is not a Spacemacs customization variable" var-symbol)))

(defun spacemacs-customization//group-variables (group-symbol)
  "Given customization group symbol get its variables."
  (let (ret-val)
    (cl-labels ((rec (gs)
                     (cl-dolist (el (get gs 'custom-group))
                       (cl-case (cadr el)
                         (custom-variable (push (car el) ret-val))
                         (custom-group (rec (car el)))))))
      (rec group-symbol))
    ret-val))

(defun spacemacs-customization//validate-group-vars (group-symbol)
  "Given customization group symbol validate its variables."
  (dolist (var (spacemacs-customization//group-variables group-symbol))
    (let ((val (symbol-value var))
          (type (custom-variable-type var)))
      (condition-case err (validate-value val type)
        (error (error (concat "Variable: \"%s\" "
                              "has value: \"%s\" "
                              "that doesn't match its type: \"%s\". "
                              "Validator message: \"%s\"")
                      var val type err))))))

(defun spacemacs-customization//validate-dotspacemacs-init-vars ()
  "Validate variables set in `dotspacemacs/init' function."
  (spacemacs-customization//validate-group-vars 'spacemacs-dotspacemacs-init))

(defun spacemacs-customization//validate-dotspacemacs-layers-vars ()
  "Validate variables set in `dotspacemacs/layers' function."
  (spacemacs-customization//validate-group-vars 'spacemacs-dotspacemacs-layers))

(defun spacemacs-customization//create-layer-group (layer-name category-name)
  "Create customization group heirarchy for the LAYER-NAME configurations.
Layers customization group symbol is returned."
  (let* ((category-group-name (format "spacemacs-layers-%s" category-name))
         (layer-group-name (format "%s-%s" category-group-name layer-name))
         (category-group-symbol (intern category-group-name)))
    (custom-declare-group category-group-symbol
                          nil
                          (format "Spacemacs %s category customizations."
                                  category-name)
                          :group 'spacemacs-layers
                          :prefix (intern (concat category-group-name "-")))
    (custom-declare-group
     (intern layer-group-name)
     nil
     (format "Spacemacs %s layer customizations." layer-name)
     :group category-group-symbol
     :prefix (intern (concat category-group-name "-"
                             layer-group-name "-")))))

(provide 'core-customization)
