;;; config.el --- gtags configuration File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar gtags-enable-by-default t
  "Whether or not to enable ggtags-mode.")

(defvar spacemacs--counsel-gtags-dwim-success nil
  "Stores the return value of `counsel-gtags-dwim' so it can be
  passed to the jump handler. This is needed because `buffer' and
  `point' are not updated after jumping.")

(spacemacs|define-jump-handlers tcl-mode)
(spacemacs|define-jump-handlers vhdl-mode)
(spacemacs|define-jump-handlers awk-mode)
(spacemacs|define-jump-handlers dired-mode)
(spacemacs|define-jump-handlers compilation-mode)
(spacemacs|define-jump-handlers shell-mode)
