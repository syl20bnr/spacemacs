;;; layers.el --- typescript Layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layer-dependencies '(node javascript prettier))

(when (and (boundp 'typescript-backend)
           (eq typescript-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
