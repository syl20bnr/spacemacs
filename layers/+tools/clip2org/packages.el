;;; packages.el --- clip2org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst clip2org-packages
  '((clip2org :location (recipe
                         :fetcher github
                         :repo "thamer/clip2org"))))

(defun clip2org/init-clip2org ()
  (use-package clip2org))

;;; packages.el ends here
