;;; layers.el --- Spacemacs Layer layers File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers '(spacemacs))

;; TODO: Find a better place for it. Also better hook to allow customization.
(add-hook 'spacemacs-post-user-config-hook 'spacemacs-docker//load-env)
