;;; packages.el --- GPU layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Evan Klitzke <evan@eklitzke.org>
;; URL: https://github.com/eklitzke
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq gpu-packages
      '(cuda-mode
        opencl-mode))

(defun gpu/init-cuda-mode ()
  (use-package cuda-mode :defer t))

(defun gpu/init-opencl-mode ()
  (use-package opencl-mode
    :defer t
    :mode (("\\.cl\\'" . opencl-mode))))
