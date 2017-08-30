;;; packages.el --- node layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Juan Placencia <juan.placencia.512@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq node-packages
      '(
        (add-node-modules-path :toggle node-add-modules-path)
        ))

(defun node/init-add-node-modules-path ()
  (use-package add-node-modules-path :defer t))
