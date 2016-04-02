;;; packages.el --- tmux Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq tmux-packages '((tmux :location local)))

(defun tmux/init-tmux ()
  "Initialize tmux"
  (use-package tmux))
