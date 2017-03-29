;;; config.el --- symon Layer Packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq symon-packages '(symon))

(defun symon/init-symon ()
  (use-package symon
    :defer t
    :init
    (spacemacs|add-toggle minibuffer-system-monitor
      :mode symon-mode
      :evil-leader "tM")))
