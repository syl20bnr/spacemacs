;;; config.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Kepi <kepi@igloonet.cz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers sql-mode)

(defvar sql-capitalize-keywords nil
  "Capitalize keywords in SQL mode.")

(defvar sql-capitalize-keywords-disable-interactive nil
  "Do not capitalize keywords in interactive session (e.g. psql).")

(defvar sql-capitalize-keywords-blacklist '("name")
  "List of keywords to ignore during capitalization.")

(defvar sql-auto-indent t
  "If non nil use sql-indent.")
