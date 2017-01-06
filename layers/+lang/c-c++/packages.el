;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq c-c++-packages
  '(
    cc-mode
    disaster
    clang-format
    cmake-mode
    company
    (company-c-headers :toggle (configuration-layer/package-usedp 'company))
    company-ycmd
    flycheck
    gdb-mi
    ggtags
    helm-cscope
    helm-gtags
    semantic
    srefactor
    stickyfunc-enhance
    ycmd
    xcscope
    ))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,c-c++-default-mode-for-headers)))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window))))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "D" 'disaster)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "D" 'disaster))))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :if c-c++-enable-clang-support))

(defun c-c++/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

(defun c-c++/post-init-company ()
  (spacemacs|add-company-hook c-mode-common)
  (spacemacs|add-company-hook cmake-mode)

  (when c-c++-enable-clang-support
    (push 'company-clang company-backends-c-mode-common)

    (defun company-mode/more-than-prefix-guesser ()
      (c-c++/load-clang-args)
      (company-clang-guess-prefix))

    (setq company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)
    (spacemacs/add-to-hooks 'c-c++/load-clang-args '(c-mode-hook c++-mode-hook))))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (push 'company-c-headers company-backends-c-mode-common)))

(defun c-c++/post-init-flycheck ()
  (dolist (mode '(c-mode c++-mode))
    (spacemacs/add-flycheck-hook mode))
  (when c-c++-enable-clang-support
    (spacemacs/add-to-hooks 'c-c++/load-clang-args '(c-mode-hook c++-mode-hook))))

(defun c-c++/post-init-ggtags ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun c-c++/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun c-c++/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'c-mode)
  (spacemacs/helm-gtags-define-keys-for-mode 'c++-mode))

(defun c-c++/post-init-semantic ()
  (spacemacs/add-to-hooks 'semantic-mode '(c-mode-hook c++-mode-hook)))

(defun c-c++/post-init-srefactor ()
  (spacemacs/set-leader-keys-for-major-mode 'c-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-srefactor '(c-mode-hook c++-mode-hook)))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance '(c-mode-hook c++-mode-hook)))

(defun c-c++/post-init-ycmd ()
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-to-list 'spacemacs-jump-handlers-c++-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-c-mode '(ycmd-goto :async t))
  (dolist (mode '(c++-mode c-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "gG" 'ycmd-goto-imprecise)))

(defun c-c++/post-init-company-ycmd ()
  (push 'company-ycmd company-backends-c-mode-common))

(defun c-c++/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

(defun c-c++/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/setup-helm-cscope mode))))
