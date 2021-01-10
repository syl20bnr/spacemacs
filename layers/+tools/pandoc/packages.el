;;; packages.el --- pandoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Christoph Paulik <cpaulik@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pandoc-packages
      '(pandoc-mode
        ox-pandoc
        ))

(defun pandoc/init-pandoc-mode ()
  "Initialize my package"
  (use-package pandoc-mode
    :defer t
    :commands spacemacs/run-pandoc
    :init
    (progn
      (spacemacs/declare-prefix "P" "pandoc")
      (spacemacs/set-leader-keys "P/" 'spacemacs/run-pandoc))
    :config
    (progn
      (setq pandoc-data-dir (concat spacemacs-cache-directory "pandoc/"))
      (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))))

(defun pandoc/init-ox-pandoc ()
  (use-package ox-pandoc
    :defer t
    :init
    (with-eval-after-load 'org (require 'ox-pandoc))))
