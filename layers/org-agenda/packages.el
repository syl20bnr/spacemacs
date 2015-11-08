;;; packages.el --- Org-agenda Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-agenda-packages
      '(
        org
        org-agenda
        ))

(defun org/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      (evil-leader/set-key
        "aoT" 'org-agenda/append-todo-list
        "aoA" 'org-agenda/append-agenda-list
        ))))
