;;; config.el --- Crystal Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers crystal-mode crystal-def-jump)

(defvar crystal-enable-auto-format nil
  "If non-nil then auto-format on save.")
