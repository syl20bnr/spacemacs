;;; packages.el --- forth layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Tim Jaeger <jger.tm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst forth-packages '(forth-mode))

(defun forth/init-forth-mode ()
  (use-package forth-mode
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'forth-mode
            "ds" 'forth-see
            "eE" 'forth-eval
            "ee" 'forth-eval-last-expression
            "er" 'forth-eval-region
            "sb" 'forth-load-file
            "si" 'run-forth
            "sk" 'forth-kill)))
