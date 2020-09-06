;;; packages.el --- ligatures layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Ade Attwood <hello@adeattwood.co.uk>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(if (> emacs-major-version 26)
  (defconst ligatures-packages
    '((ligature :location (recipe :fetcher github :repo "mickeynp/ligature.el"))))
  (defconst ligatures-packages '()))

(defun ligatures/init-ligature ()
  "Initialise the ligatures"
  (dolist (mode ligature-modes)
    (ligature-set-ligatures mode ligature-set))
  (global-ligature-mode t))

;;; packages.el ends here
