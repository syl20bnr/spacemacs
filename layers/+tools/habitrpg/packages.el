;;; packages.el --- habitrpg layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Blasphemy <ksonney@sonnney.com>
;; URL: https://github.com/ksonney/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `habitrpg/init-PACKAGE' to load and initialize the package.

;;; Code:

(defconst habitrpg-packages
  '(
    (habitrpg :location (recipe
                         :fetcher github
                         :repo "ryjm/habitrpg.el"))))

(defun habitrpg/init-habitrpg ()
  (require 'org-habit)
  (require 'habitrpg))

;;; packages.el ends here
