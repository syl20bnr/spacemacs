;;; config.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (configuration-layer/layer-usedp 'nlinum)
  (spacemacs-buffer/warning (concat "nlinum layer is deprecated for Emacs 26.1 and above."
                                    " You can safely remove it from your dotfile.")))
