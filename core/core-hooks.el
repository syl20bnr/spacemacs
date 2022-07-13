;;; core-hooks.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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



(defun spacemacs/run-prog-mode-hooks ()
  "Runs `prog-mode-hook'. Useful for modes that don't derive from
`prog-mode' but should."
  (run-hooks 'prog-mode-hook))

(defun spacemacs/run-text-mode-hooks ()
  "Runs `text-mode-hook'. Useful for modes that don't derive from
`text-mode' but should."
  (run-hooks 'text-mode-hook))

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el#L38
(defun spacemacs/add-to-hooks (fun hooks &optional append local)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun append local)))

(defun spacemacs/add-all-to-hook (hook &rest funs)
  "Add functions to hook."
  (spacemacs/add-to-hook hook funs))

(defun spacemacs/add-to-hook (hook funs)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

;; Transient hooks

(defmacro spacemacs|add-transient-hook (hook func &optional fname)
  "Add transient hook by hook FUNC to HOOK.
Transient hooks are ephemeral hooks that vanishes when executed.
If FUNC is a lambda you must give it a name with FNAME. "
  (declare (indent 1))
  (let ((hfunc (intern (format "spacemacs//transient-hook-%s"
                               (if fname fname func))))
        result)
    (setq result
          (append (when fname
                    `((fset ',fname (lambda (&rest _) (funcall #',func)))))
                  `((fset ',hfunc (lambda (&rest _)
                                    ,(if fname (list fname) (list func))
                                    ,(if (functionp hook)
                                         `(advice-remove ',hook ',hfunc)
                                       `(remove-hook ',hook ',hfunc))
                                    ;; instead of unbinding we reset the
                                    ;; functions to be the `ignore' function.
                                    ;; see: https://github.com/syl20bnr/spacemacs/issues/10930
                                    (fset ',hfunc 'ignore)
                                    ,(when fname `(fset ',fname 'ignore)))))
                  (if (functionp hook)
                      `((advice-add ',hook :before ',hfunc))
                    `((add-hook ',hook ',hfunc)))))
    (push 'progn result)))

(provide 'core-hooks)
