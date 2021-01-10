;;; funcs.el --- reasonml layer functions file for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Dave Aitken <dave.aitken@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun reason/rtop-prompt ()
  "The rtop prompt function."
  (let ((prompt (format "rtop[%d]> " utop-command-number)))
    (add-text-properties 0 (length prompt) '(face utop-prompt) prompt)
    prompt))

(defun reason/refmt-re-to-ml ()
  (interactive)
  (if (use-region-p)
      (apply-refmt (region-beginning) (region-end) "re" "ml")
    (apply-refmt nil nil "re" "ml")))

(defun reason/refmt-ml-to-re ()
  (interactive)
  (if (use-region-p)
      (apply-refmt (region-beginning) (region-end) "ml" "re")
    (apply-refmt nil nil "ml" "re")))
