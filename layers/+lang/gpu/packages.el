;;; packages.el --- GPU layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Evan Klitzke <evan@eklitzke.org>
;; URL: https://github.com/eklitzke
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq gpu-packages
      '(
        (company-glsl :location (recipe
                                 :fetcher github
                                 :repo "Kaali/company-glsl")
                      :requires company
                      :toggle (executable-find "glslangValidator"))
        cuda-mode
        glsl-mode
        opencl-mode
        ))


(defun gpu/init-company-glsl ()
  (use-package company-glsl
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-glsl
            :modes glsl-mode)))

(defun gpu/init-cuda-mode ()
  (use-package cuda-mode :defer t))

(defun gpu/init-glsl-mode ()
  "Initialize GLSL mode"
  (use-package glsl-mode
    :defer t
    :mode (("\\.fsh\\'" . glsl-mode)
           ("\\.vsh\\'" . glsl-mode))))

(defun gpu/init-opencl-mode ()
  (use-package opencl-mode
    :defer t
    :mode (("\\.cl\\'" . opencl-mode))))
