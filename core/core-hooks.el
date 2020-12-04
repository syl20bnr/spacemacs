;;; core-hooks.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


(defun space-macs/run-prog-mode-hooks ()
  "Runs `prog-mode-hook'. Useful for modes that don't derive from
`prog-mode' but should."
  (run-hooks 'prog-mode-hook))

(defun space-macs/run-text-mode-hooks ()
  "Runs `text-mode-hook'. Useful for modes that don't derive from
`text-mode' but should."
  (run-hooks 'text-mode-hook))

;; from https://github.com/cofi/dotfiles/blob/master/e-macs.d/config/cofi-util.el#L38
(defun space-macs/add-to-hooks (fun hooks &optional append local)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun append local)))

(defun space-macs/add-all-to-hook (hook &rest funs)
  "Add functions to hook."
  (space-macs/add-to-hook hook funs))

(defun space-macs/add-to-hook (hook funs)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

;; Transient hooks

(defmacro space-macs|add-transient-hook (hook func &optional fname)
  "Add transient hook by hook FUNC to HOOK.
Transient hooks are ephemeral hooks that vanishes when executed.
If FUNC is a lambda you must give it a name with FNAME. "
  (declare (indent 1))
  (let ((hfunc (intern (format "space-macs//transient-hook-%s"
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
                                    ;; see: https://github.com/syl20bnr/space-macs/issues/10930
                                    (fset ',hfunc 'ignore)
                                    ,(when fname `(fset ',fname 'ignore)))))
                  (if (functionp hook)
                      `((advice-add ',hook :before ',hfunc))
                    `((add-hook ',hook ',hfunc)))))
    (push 'progn result)))

(provide 'core-hooks)


