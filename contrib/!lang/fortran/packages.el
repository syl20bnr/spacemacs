;;; packages.el --- Fortran Layer packages File for Spacemacs
;; Copyright (c) 2015 Noah Evans
;;
;; Author: Noah Evans <noah.evans@gmail.com>
;; URL:  https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq fortran-packages
      `(
        company
        company-semantic
        flycheck
        flycheck-gfortran
        ;; fortran-mode
        ;; fortran snippets?
        ;; fortran indentation?
        ;; structured editing would be nice too
        f90-mode
        ))

(defun fortran/init-fortran-mode ()
  (use-package fortran-mode
    :defer t
    ;; :config
    ;;
    (evil-leader/set-key-for-mode 'fortran-mode
      "mn" 'f90-next-statement
      "mp" 'f90-previous-statement
      "mN" 'f90-next-block
      "mP" 'f90-previous-block
      )
    ))
                                        ;; I need to be able to handle fortran api lookup*
