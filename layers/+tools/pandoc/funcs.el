;;; funcs.el --- pandoc Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Christoph Paulik <cpaulik@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/run-pandoc ()
  "Start pandoc for the buffer and open the menu"
  (interactive)
  ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
  (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
  (pandoc-main-hydra/body))
