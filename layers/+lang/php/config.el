;;; config.el --- PHP Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Kosta Harlan <kosta@kostaharlan.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables


(spacemacs|define-jump-handlers php-mode)

(defvar php-backend nil
  "The backend to use for IDE features.
Possible values are `lsp'.")
