;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar c-c++-packages
  '(
    cc-mode
    cmake-mode
    flycheck
    stickyfunc-enhance
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(unless (version< emacs-version "24.4")
  (add-to-list 'c-c++-packages 'srefactor))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    (progn
      (require 'compile)
      (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
      (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
      (semantic-mode 1)
      (c-toggle-auto-newline 1)
      (setq srecode-map-save-file (concat spacemacs-cache-directory "srecode-map.el"))
      (setq semanticdb-default-save-directory (concat spacemacs-cache-directory "semanticdb/")))))

(defun c-c++/init-cmake-mode ()
  (use-package cmake-mode
    :defer t
    :init
    (setq auto-mode-alist
          (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode))
                  auto-mode-alist))))

(defun c-c++/init-flycheck ()
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))

(defun c-c++/init-srefactor ()
  (use-package srefactor
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'c-mode
        "mr" 'srefactor-refactor-at-point)
      (evil-leader/set-key-for-mode 'c++-mode
        "mr" 'srefactor-refactor-at-point))))

(defun c-c++/init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (progn

      (defun spacemacs/lazy-load-stickyfunc-enhance ()
        "Lazy load the package."
        (require 'stickyfunc-enhance))

      (add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance
                    '(c-mode c++-mode)))))

