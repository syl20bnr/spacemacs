;;; core-configuration-layer-ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--spacemacs-layer-always-first ()
  (let ((dotspacemacs-configuration-layers '(emacs-lisp git)))
    (configuration-layer//declare-layers)
    (should (eq 'spacemacs (oref (first configuration-layer--layers) :name)))))

(ert-deftest test-declare-layers--spacemacs-layer-always-first-all ()
  (let ((dotspacemacs-configuration-layers 'all))
    (configuration-layer//declare-layers)
    (should (eq 'spacemacs (oref (first configuration-layer--layers) :name)))))
