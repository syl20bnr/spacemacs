;;; packages.el --- evil-remap layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Caleb Meyer <kiaulen@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

;; The list of Lisp packages required by the evil-remap layer.
(defconst evil-remap-packages
  '(
    (evil-remap :location (recipe
                           :fetcher github
                           :repo "GuiltyDolphin/evil-remap"))
    ))

(defun evil-remap/init-evil-remap ()
    (use-package "evil-remap"))

;;; packages.el ends here
