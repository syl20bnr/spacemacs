;;; packages.el --- Sphinx layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <wwguo@hiGDP>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
  (spacemacs|use-package-add-hook rst
    :post-config
    (spacemacs/declare-prefix-for-mode 'rst-mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode 'rst-mode "mg" "goto")
    (spacemacs/set-leader-keys-for-major-mode 'rst-mode
      "cc" 'rst-sphinx-compile
      "cC" 'rst-sphinx-clean
      "cr" 'rst-sphinx-rebuild
      "gc" 'rst-sphinx-open-conf
      "o"  'rst-sphinx-target-open)))
