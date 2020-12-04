;;; packages.el --- Sphinx layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author:  <wwguo@hiGDP>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst sphinx-packages
  '(
    rst
    (rst-sphinx :location local)
    ))

(defun sphinx/init-rst-sphinx ()
  (use-package rst-sphinx))

(defun sphinx/pre-init-rst ()
  (space-macs|use-package-add-hook rst
    :post-config
    (space-macs/declare-prefix-for-mode 'rst-mode "mc" "compile")
    (space-macs/declare-prefix-for-mode 'rst-mode "mg" "goto")
    (space-macs/set-leader-keys-for-major-mode 'rst-mode
      "cc" 'rst-sphinx-compile
      "cC" 'rst-sphinx-clean
      "cr" 'rst-sphinx-rebuild
      "gc" 'rst-sphinx-open-conf
      "o"  'rst-sphinx-target-open)))


