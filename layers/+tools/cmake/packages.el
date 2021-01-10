;;; packages.el --- CMake layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Alexander Dalshov <dalshov@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
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
    (progn
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
          "pd" 'cmake-ide-delete-file)))
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
