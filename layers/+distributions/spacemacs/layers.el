;;; layers.el --- Spacemacs Layer layers File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers '(
                                      spacemacs-base
                                      spacemacs-completion
                                      spacemacs-layouts
                                      spacemacs-editing
                                      spacemacs-editing-visual
                                      spacemacs-evil
                                      spacemacs-language
                                      spacemacs-misc
                                      spacemacs-modeline
                                      spacemacs-navigation
                                      spacemacs-org
                                      spacemacs-purpose
                                      spacemacs-visual
                                      ))
;; If the user has not explicitly declared `helm' or `ivy'
;; and they are using the standard distribution, assume they
;; want `helm' completion.
(unless (or (configuration-layer/layer-used-p 'ivy)
            (configuration-layer/layer-used-p 'helm))
  (configuration-layer/declare-layers '(helm)))

(when (and (configuration-layer/layer-used-p 'ivy)
           (configuration-layer/layer-used-p 'helm))
  (spacemacs-buffer/warning (concat "Both the `helm' and `ivy' layers are enabled. "
                             "This may lead to unexpected behaviour.")))
