;;; config.el --- Markdown Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|defvar-company-backends markdown-mode)

(defvar markdown-live-preview-engine 'eww
  "Possibe values are `eww' (built-in browser) or `vmd' (installed with `npm').")
