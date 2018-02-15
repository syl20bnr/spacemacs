;;; funcs.el --- GitHub layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode'."
  (interactive)
  (magit-gh-pulls-mode)
  (magit-gh-pulls-popup))
