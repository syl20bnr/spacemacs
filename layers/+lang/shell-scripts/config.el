;;; config.el --- Shell Scripts Layer Configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; variables

(space-macs|define-jump-handlers sh-mode)

(defvar shell-scripts-backend nil
  "The backend to use for IDE features.
Possible values are `lsp'.")


