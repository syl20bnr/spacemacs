;;; packages.el --- shaders layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alan Love <alan@cattes.us>
;; URL: https://github.com/ell
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GLPv3
(setq shaders-packages '(glsl-mode))

(defun shaders/init-glsl-mode ()
  "Initialize GLSL mode"
  (use-package glsl-mode
    :mode (("\\.fsh\\'"  . glsl-mode)
           ("\\.vsh\\'"  . glsl-mode))))
