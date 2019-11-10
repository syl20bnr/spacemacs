;;; funcs.el --- docker Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//docker-dockerfile-backend ()
  "Returns selected backend."
  ;; backend must be choosed explicitly with this layer
  docker-dockerfile-backend)

(defun spacemacs//docker-dockerfile-setup-backend ()
  "Conditionally setup docker backend."
  (pcase (spacemacs//docker-dockerfile-backend)
    (`lsp (spacemacs//docker-dockerfile-setup-lsp))))


;; lsp

(defun spacemacs//docker-dockerfile-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
