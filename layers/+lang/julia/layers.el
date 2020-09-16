;;; layers.el --- Julia Layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and (boundp 'julia-backend)
           (eq julia-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
