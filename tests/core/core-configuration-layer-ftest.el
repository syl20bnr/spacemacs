;;; core-configuration-layer-ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

(ert-deftest test-declare-layers--bootstrap-layer-always-first ()
  (let ((dotspacemacs-distribution 'spacemacs)
        (dotspacemacs-configuration-layers '(emacs-lisp
                                             (git :variables foo 'bar))))
    (let (configuration-layer--layers)
      (configuration-layer//declare-layers)
      (should (eq 'spacemacs-bootstrap
                  (oref (first configuration-layer--layers) :name))))))

(ert-deftest test-declare-layers--distribution-layer-is-second ()
  (let ((dotspacemacs-distribution 'spacemacs-base)
        (dotspacemacs-configuration-layers '(emacs-lisp
                                             (git :variables foo 'bar))))
    (let (configuration-layer--layers)
      (configuration-layer//declare-layers)
      (should (eq 'spacemacs-base
                  (oref (second configuration-layer--layers) :name))))))
