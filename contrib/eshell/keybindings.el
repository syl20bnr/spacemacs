;;; keybindings.el --- Eshell configuration File for Spacemacs
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

(evil-leader/set-key
  "Ep" 'spacemacs/eshell-pop
  "Es" 'spacemacs/eshell
  "Ed" 'spacemacs/eshell-for-dir)
