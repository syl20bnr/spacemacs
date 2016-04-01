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
    srefactor
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    (progn
      (require 'compile)
      (mapc (lambda (m)
              (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
              (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
              (semantic-mode 1)
              (c-toggle-auto-newline 1))
            '(c-mode c++-mode)))))

(defun c-c++/init-srefactor ()
  (use-package srefactor
    :if (not (version< emacs-version "24.4"))
    :init
    (progn
      (evil-leader/set-key-for-mode 'c-mode
        "mr" 'srefactor-refactor-at-point)
      (evil-leader/set-key-for-mode 'c++-mode
        "mr" 'srefactor-refactor-at-point))))

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
