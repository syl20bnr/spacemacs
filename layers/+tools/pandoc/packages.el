;;; packages.el --- pandoc Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Christoph Paulik <cpaulik@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
    :commands space-macs/run-pandoc
    :init
    (progn
      (space-macs/declare-prefix "P" "pandoc")
      (space-macs/set-leader-keys "P/" 'space-macs/run-pandoc))
    :config
    (progn
      (setq pandoc-data-dir (concat space-macs-cache-directory "pandoc/"))
      (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))))

(defun pandoc/init-ox-pandoc ()
  (use-package ox-pandoc
    :defer t
    :init
    (with-eval-after-load 'org (require 'ox-pandoc))))


