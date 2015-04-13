;;; packages.el --- ocaml Layer packages File for Spacemacs
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

(defvar ocaml-packages
  '(
    tuareg
    merlin
;;    flycheck
;;    flycheck-ocaml
    ;; package ocamls go here
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ocaml-excluded-packages '()
  "List of packages to exclude.")

(defun ocaml/init-tuareg ()
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  )

(defun ocaml/init-merlin ())

;; (defun ocaml/init-flycheck-ocaml ()
;;   (progn
;;     (setq merlin-error-after-save nil)
;;     (flycheck-ocaml-setup)
;;     )
;; )

;; For each package, define a function ocaml/init-<package-ocaml>
;;
;; (defun ocaml/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
