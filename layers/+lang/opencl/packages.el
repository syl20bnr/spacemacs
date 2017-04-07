;;; packages.el --- OpenCL layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Evan Klitzke <evan@eklitzke.org>
;; URL: https://github.com/eklitzke
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq opencl-packages
      '(opencl-mode))

(defun opencl/init-opencl-mode ()
  "Initialize OpenCL mode"
  (use-package opencl-mode
    :defer t
    :mode (("\\.cl\\'" . opencl-mode))))
