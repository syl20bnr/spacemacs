;;; layers.el --- Spacemacs Layer layers File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers '(spacemacs-base
                                      spacemacs-completion
                                      spacemacs-layouts
                                      spacemacs-editing
                                      spacemacs-editing-visual
                                      spacemacs-evil
                                      spacemacs-language
                                      spacemacs-misc
                                      spacemacs-purpose
                                      spacemacs-ui
                                      spacemacs-ui-visual
                                      spacemacs-org))
;; If the user has not explicitly declared `helm' or `ivy'
;; and they are using the standard distribution, assume they
;; want `helm' completion.
(unless (or (configuration-layer/layer-usedp 'ivy)
            (configuration-layer/layer-usedp 'helm))
  (configuration-layer/declare-layers '(helm)))

(when (and (configuration-layer/layer-usedp 'ivy)
           (configuration-layer/layer-usedp 'helm))
  (spacemacs-buffer/warning (concat "Both the `helm' and `ivy' layers are enabled. "
                             "This may lead to unexpected behaviour.")))
