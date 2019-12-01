;;; config.el --- docker Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers dockerfile-mode)

;; Variables

(defvar docker-dockerfile-backend 'nil
  "The backend to use for IDE features. Possible values are `lsp' or `nil'.")
