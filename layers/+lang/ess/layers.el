;;; layers.el ---  ESS Layer declarations File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and (boundp 'ess-r-backend) (eq ess-r-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
