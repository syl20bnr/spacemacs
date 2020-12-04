;;; layers.el --- Space-macs distribution Layer layers File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; We use `space-macs/declare-layers' instead of
;; `configuration-layer/declare-layer-dependencies' in order to declare
;; the layers right away
;; This is a special case only for distribution layers.
(configuration-layer/declare-layers
 '(
   helm
   tree-macs
   space-macs-base
   space-macs-completion
   space-macs-layouts
   space-macs-editing
   space-macs-editing-visual
   space-macs-evil
   space-macs-language
   space-macs-misc
   space-macs-modeline
   space-macs-navigation
   space-macs-org
   space-macs-project
   space-macs-purpose
   space-macs-visual
   ))


