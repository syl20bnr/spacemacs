;;; layers.el --- Purescript Layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Ryan L. Bell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layer-dependencies '(node))

(when (and (boundp 'purescript-backend)
           (eq purescript-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
