;;; funcs.el --- Smex Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/smex ()
  "Execute smex with a better prompt."
  (interactive)
  (let ((smex-prompt-string "e-macs commands: "))
    (smex)))

(defun space-macs/smex-major-mode-commands ()
  "Reexecute smex with major mode commands only."
  (interactive)
  (let ((smex-prompt-string (format "%s commands: " major-mode)))
    (smex-major-mode-commands)))


