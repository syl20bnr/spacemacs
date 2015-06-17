;;; config.el --- Syntax Checking Layer configuration File for Spacemacs
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

;; Variables

(defvar syntax-checking-enable-tooltips t
  "If non nil some feedback are displayed in tooltips.")

(defvar syntax-checking-check-on-buffer-change nil
  "If non nil check syntax on idle and upon entering a newline, in addition to upon enabling flycheck-mode and upon saving the buffer.")

;; Command Prefixes

(spacemacs/declare-prefix "S" "spelling")
