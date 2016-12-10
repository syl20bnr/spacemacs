;;; packages.el --- Java configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|defvar-company-backends java-mode)

(spacemacs|define-jump-handlers java-mode)

(defvar java-backend 'eclim
  "The backend to use for IDE features. Possibly values are `eclim' and
  `ensime'.")
