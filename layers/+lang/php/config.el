;;; config.el --- PHP Layer config File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Kosta Harlan <kosta@kostaharlan.net>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; variables


(space-macs|define-jump-handlers php-mode)

(defvar php-backend 'nil
  "The backend to use for IDE features.
Possible values are `lsp'.")


