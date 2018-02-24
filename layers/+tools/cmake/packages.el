;;; packages.el --- CMake layer packages  fuke for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Alexander Dalshov <dalshov@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq cmake-packages
      '(
        cmake-ide
        cmake-mode
        company
        (helm-ctest :requires helm)
        ))

(defun cmake/init-cmake-ide ()
  (use-package cmake-ide
    :if cmake-enable-cmake-ide-support
    :defer t
    :init (spacemacs/add-to-hooks 'cmake-ide--mode-hook '(c-mode-hook
                                                          c++-mode-hook))
    :config
    (progn
      (cmake-ide-setup)
      (dolist (mode cmake-modes)
        (spacemacs/declare-prefix-for-mode mode "mc" "compile")
        (spacemacs/declare-prefix-for-mode mode "mp" "project")
        (spacemacs/set-leader-keys-for-major-mode mode
          "cc" 'cmake-ide-compile
          "pc" 'cmake-ide-run-cmake
          "pC" 'cmake-ide-maybe-run-cmake
          "pd" 'cmake-ide-delete-file)))))

(defun cmake/init-cmake-mode ()
  (use-package cmake-mode
    :defer t
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))))

(defun cmake/post-init-company ()
  (when (configuration-layer/package-used-p 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode)))

(defun cmake/init-helm-ctest ()
  (use-package helm-ctest
    :defer t
    :config
    (dolist (mode cmake-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "pt" 'helm-ctest))))
