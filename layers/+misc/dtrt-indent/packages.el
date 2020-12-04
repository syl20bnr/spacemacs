;;; packages.el --- dtrt-indent layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Kevin Doherty <kjd@csail.mit.edu>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst dtrt-indent-packages '(dtrt-indent))

(defun dtrt-indent/init-dtrt-indent ()
  (use-package dtrt-indent
    :hook (prog-mode .
              (lambda ()
                (modify-syntax-entry ?_ "w")
                (dtrt-indent-mode)
                (dtrt-indent-adapt)))
    :config
    (progn
      (space-macs|hide-lighter dtrt-indent-mode))))

;;; packages.el ends here


