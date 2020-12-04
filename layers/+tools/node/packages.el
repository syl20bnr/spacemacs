;;; packages.el --- node layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Juan Placencia <juan.placencia.512@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq node-packages
      '(
        (add-node-modules-path :toggle node-add-modules-path)
        ))

(defun node/init-add-node-modules-path ()
  (use-package add-node-modules-path :defer t))


