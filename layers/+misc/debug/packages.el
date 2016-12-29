;;; packages.el --- debug layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <mfulz@olznet.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `debug-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `debug/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `debug/pre-init-PACKAGE' and/or
;;   `debug/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq debug-packages
  '(
    gdb-mi
    gud
    ))

(defun debug/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun debug/init-gud ()
  (use-package gud
    :defer t
    :config
    (progn
      (spacemacs|define-transient-state debug
        :title "Debug Transient State"
        :doc "
        [_n_]^^^^      next            [_i_]^^    nexti
        [_b_]^^^^      break           [_x_]^^    remove
        [_c_]^^^^      continue        [_r_]^^    run
        [_w_]^^^^      watch"
        :bindings
        ("n" gud-next)
        ("i" gud-nexti)
        ("b" gud-break)
        ("x" gud-remove)
        ("c" gud-cont)
        ("r" gud-run)
        ("w" gud-watch)
        ("q" nil :exit t))
      (spacemacs/set-leader-keys "dn" 'gud-next)
      (spacemacs/set-leader-keys "di" 'gud-nexti)
      (spacemacs/set-leader-keys "db" 'gud-break)
      (spacemacs/set-leader-keys "dx" 'gud-remove)
      (spacemacs/set-leader-keys "dc" 'gud-cont)
      (spacemacs/set-leader-keys "dr" 'gud-run)
      (spacemacs/set-leader-keys "dw" 'gud-watch)
      (spacemacs/set-leader-keys "d." 'spacemacs/debug-transient-state/body))))

;;; packages.el ends here
