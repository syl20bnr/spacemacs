;;; layers.el --- typescript Layer layers File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(configuration-layer/declare-layer-dependencies '(node javascript prettier tide))

(when (and (boundp 'typescript-backend)
           (eq typescript-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))


