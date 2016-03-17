;;; config.el --- Spacemacs Layer configuration File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Prerequisites

(configuration-layer/declare-layers '(spacemacs-base
                                      spacemacs-layouts
                                      spacemacs-editing
                                      spacemacs-editing-visual
                                      spacemacs-evil
                                      spacemacs-language
                                      spacemacs-ui
                                      spacemacs-ui-visual))
;; If the user has not explicitly declared spacemacs-helm or spacemacs-ivy and
;; they are using the standard distribution, assume they want helm completion.
(unless (or (configuration-layer/layer-usedp 'spacemacs-ivy)
            (configuration-layer/layer-usedp 'spacemacs-helm))
  (configuration-layer/declare-layers '(spacemacs-helm)))
