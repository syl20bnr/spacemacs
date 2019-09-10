;;; layers.el --- Julia Layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and (boundp 'julia-mode-enable-lsp)
           julia-mode-enable-lsp t)
  (configuration-layer/declare-layer 'lsp))
