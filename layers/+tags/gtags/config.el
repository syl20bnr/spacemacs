;;; config.el --- gtags configuration File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defvar gtags-enable-by-default t
  "Whether or not to enable ggtags-mode.")

(defvar space-macs--counsel-gtags-dwim-success nil
  "Stores the return value of `counsel-gtags-dwim' so it can be
  passed to the jump handler. This is needed because `buffer' and
  `point' are not updated after jumping.")

(space-macs|define-jump-handlers tcl-mode)
(space-macs|define-jump-handlers vhdl-mode)
(space-macs|define-jump-handlers awk-mode)
(space-macs|define-jump-handlers dired-mode)
(space-macs|define-jump-handlers compilation-mode)
(space-macs|define-jump-handlers shell-mode)


