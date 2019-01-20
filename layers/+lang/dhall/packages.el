;;; packages.el --- Dhall Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Colin Woodbury <colin@fosskers.ca>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst dhall-packages
  '(
    dhall-mode
    ))

(defun dhall/init-dhall-mode ()
  "Initialize dhall-mode."
  (use-package dhall-mode
    :defer t

    ))

;;; packages.el ends here
