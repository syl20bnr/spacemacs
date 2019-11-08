;;; packages.el --- Conda Layer packages File for Spacemacs
;;
;; Copyright (C) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Zach Pearson <zach@zjp.codes>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq conda-packages
      '(
        conda
        ))

(defun conda/init-conda ()
  (use-package conda
    :defer t
    :commands (conda-env-list
               conda-env-activate
               conda-env-deactivate
               conda-env-autoactivate-mode
               conda-env-activate-for-buffer)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'python-mode "mn" "anaconda")
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "nl" 'conda-env-list
        "na" 'conda-env-activate
        "nd" 'conda-env-deactivate
        "nA" 'conda-env-autoactivate-mode
        "nb" 'conda-env-activate-for-buffer))))
