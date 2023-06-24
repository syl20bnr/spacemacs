;;; packages.el --- CMake layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Alexander Dalshov <dalshov@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
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

(defconst cmake-packages
  '(
    (cmake-ide :toggle cmake-enable-cmake-ide-support)
    cmake-mode
    company
    (helm-ctest :requires helm)))

(defun cmake/init-cmake-ide ()
  (use-package cmake-ide
    :if cmake-enable-cmake-ide-support
    :commands (cmake-ide-delete-file cide--mode-hook)
    :init
    (dolist (hook '(c-mode-hook c++-mode-hook))
      ;; append the `cide--mode-hook' in order to load it last
      (add-hook hook 'cide--mode-hook 'append))
    (dolist (mode cmake-modes)
      (spacemacs/declare-prefix-for-mode mode "mc" "compile")
      (spacemacs/declare-prefix-for-mode mode "mp" "project")
      (spacemacs/set-leader-keys-for-major-mode mode
        "cc" 'cmake-ide-compile
        "pc" 'cmake-ide-run-cmake
        "pC" 'cmake-ide-maybe-run-cmake
        "pd" 'cmake-ide-delete-file))
    :config (cmake-ide-setup)))

(defun cmake/init-cmake-mode ()
  (use-package cmake-mode
    :defer t
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (add-hook 'cmake-mode-hook #'spacemacs//cmake-setup-backend)))

(defun cmake/post-init-company ()
  (spacemacs//cmake-setup-company))

(defun cmake/init-helm-ctest ()
  (use-package helm-ctest
    :defer t
    :init (dolist (mode cmake-modes)
            (spacemacs/set-leader-keys-for-major-mode mode
              "pt" 'helm-ctest))))
