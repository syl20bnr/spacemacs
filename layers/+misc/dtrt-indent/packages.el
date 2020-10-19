;;; packages.el --- dtrt-indent layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Kevin Doherty <kjd@csail.mit.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
      (spacemacs|hide-lighter dtrt-indent-mode))))

;;; packages.el ends here
