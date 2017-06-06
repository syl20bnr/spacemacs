;;; config.el --- mu4e Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; CEDETDIR 
(defvar cedet-installation-path nil
  "Installation path for cedet.")

(when cedet-installation-path
  (push cedet-installation-path load-path)
  (let ((cedet-contrib-installation-path
         (concat cedet-installation-path "contrib")))
    (push cedet-contrib-installation-path load-path)))

