;;; layers.el --- cmake Layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and (boundp 'cmake-backend)
           (eq cmake-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
