;;; packages.el --- pandoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    :config
    (progn
      (defun spacemacs/run-pandoc ()
        "Start pandoc for the buffer and open the menu"
        (interactive)
        (pandoc-mode)
        (pandoc-main-hydra/body))
      (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
    :init
    (progn
      (spacemacs/set-leader-keys "P/" 'spacemacs/run-pandoc))))

(defun pandoc/init-ox-pandoc ()
  (use-package ox-pandoc
    :defer t
    :init
    (with-eval-after-load 'org (require 'ox-pandoc))))
