;;; keybindings.el --- bepo Layer key bindings File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bepo|config spacemacs
  :description
  "Customize some `spacemacs' bindings."
  :config
  (bepo/leader-correct-keys
    "jh"
    "jj"
    "jk"
    "jl"
    ;;
    "jJ"))
