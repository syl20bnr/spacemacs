;;; config.el --- docker Layer Configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(space-macs|define-jump-handlers dockerfile-mode)

;; Variables

(defvar docker-dockerfile-backend 'nil
  "The backend to use for IDE features. Possible values are `lsp' or `nil'.")


