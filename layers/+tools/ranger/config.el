;;; config.el --- ranger Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner
;; Copyright (c) 2020-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar ranger-enter-with-minus 'deer
  "Option to enter `deer' or `ranger' when `-' is pressed.
Idea from `vim-vinegar'.

The possible values are:
'deer (default)
'ranger
nil (restores the default behavior of `-')")
