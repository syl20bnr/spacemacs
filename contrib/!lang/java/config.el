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

;; Command prefixes
(setq java/key-binding-prefixes '(("me" . "errors")
                                  ("mf" . "find")
                                  ("mg" . "goto")
                                  ("mr" . "refactor")
                                  ("mh" . "documentation")
                                  ("mm" . "maven")
                                  ("ma" . "ant")
                                  ("mp" . "project")
                                  ("mt" . "test")))

(mapc (lambda(x) (spacemacs/declare-prefix-for-mode
                  'java-mode (car x) (cdr x)))
      java/key-binding-prefixes)
