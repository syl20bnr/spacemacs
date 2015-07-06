;;; packages.el --- Java configuration File for Spacemacs
;;
;; Copyright (c) 2015 Lukasz Klich
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Define the buffer local company backend variable
(spacemacs|defvar-company-backends java-mode)
(spacemacs/declare-prefix "me" "errors")
(spacemacs/declare-prefix "mf" "find")
(spacemacs/declare-prefix "mg" "goto")
(spacemacs/declare-prefix "mr" "refactor")
(spacemacs/declare-prefix "mh" "documentation")
(spacemacs/declare-prefix "mm" "maven")
(spacemacs/declare-prefix "ma" "ant")
(spacemacs/declare-prefix "mp" "project")
(spacemacs/declare-prefix "mt" "test")
