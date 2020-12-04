;;; config.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(when (configuration-layer/layer-usedp 'nlinum)
  (space-macs-buffer/warning (concat "nlinum layer is deprecated for e-macs 26.1 and above."
                                    " You can safely remove it from your dotfile.")))


