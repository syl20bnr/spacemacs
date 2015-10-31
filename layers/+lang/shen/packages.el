;;; packages.el --- Shen Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq shen-packages
      '(shen-mode
        ;; inf-shen is not in GNU ELPA, pending FSF copyright paperwork
        (inf-shen :location (recipe :fetcher git
                                    :repo "https://github.com/eschulte/shen-mode.git"
                                    :files ("inf-shen.el")))))

(defun shen/init-shen-mode ()
  "Initialize my package"
  (use-package shen-mode))

(defun shen/init-inf-shen ()
  (use-package inf-shen))
