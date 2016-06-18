;;; packages.el --- GLSL Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alan Love <alan@cattes.us>
;; URL: https://github.com/ell
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GLPv3
(setq glsl-packages '(glsl-mode))

(defun glsl/init-glsl-mode ()
  "Initialize GLSL mode"
  (use-package glsl-mode
    :mode (("\\.glsl\\'" . glsl-mode)
           ("\\.vert\\'" . glsl-mode)
           ("\\.frag\\'" . glsl-mode)
           ("\\.geom\\'" . glsl-mode))))
