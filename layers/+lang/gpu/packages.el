;;; packages.el --- GPU layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Evan Klitzke <evan@eklitzke.org>
;; URL: https://github.com/eklitzke
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
