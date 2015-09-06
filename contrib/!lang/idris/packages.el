;;; packages.el --- idris Layer packages File for Spacemacs
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

(setq idris-packages
  '(
    flycheck
    idris-mode
    helm-idris
    ))

(defun idris/init-idris-mode ()
  (use-package idris-mode
    :defer t
    :config
    (progn
      (add-to-list 'completion-ignored-extensions ".ibc")
      (evil-leader/set-key-for-mode 'idris-mode
        "mr" 'idris-load-file
        "mt" 'idris-type-at-point
        "md" 'idris-add-clause
        "ml" 'idris-make-lemma
        "mc" 'idris-case-split
        "mw" 'idris-make-with-block
        "mm" 'idris-add-missing
        "mp" 'idris-proof-search
        "mh" 'idris-docs-at-point))))

(defun idris/init-helm-idris ()
  (use-package helm-idris
    :defer t))
