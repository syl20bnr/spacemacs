;;; layers.el --- Spacemacs base distribution layers File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; We use `spacemacs/declare-layers' instead of
;; `configuration-layer/declare-layer-dependencies' in order to declare
;; the layers right away
;; This is a special case only for distribution layers.
(configuration-layer/declare-layers '(spacemacs-defaults))
