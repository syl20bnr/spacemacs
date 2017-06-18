;;; packages.el --- vala layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst vala-packages
  '(
    vala-mode
    yasnippet
    (vala-snippets :toggle (configuration-layer/package-usedp 'yasnippet)))
  "The list of Lisp packages required by the vala layer.")

(defun vala/init-vala-mode ()
  (use-package vala-mode
    :defer t))

(defun vala/post-init-yasnippet ()
  (add-hook 'vala-mode-hook 'spacemacs/load-yasnippet))

(defun vala/init-vala-snippets ()
  (use-package vala-snippets
    :defer t))

;;; packages.el ends here
