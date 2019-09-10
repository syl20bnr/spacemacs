;;; layers.el --- Java layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layer 'groovy)

(when (and (boundp 'java-backend)
           (eq java-backend 'lsp))
  (configuration-layer/declare-layers '(lsp dap)))
