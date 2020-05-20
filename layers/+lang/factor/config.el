;;; config.el --- Factor Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: timor <timor.dd@googlemail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers factor-mode 'fuel-edit-word-at-point)

(defvar factor-ui-listener-args ""
  "Extra arguments to the factor VM binary when starting the graphical listener.")
