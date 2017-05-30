;;; config.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Kepi <kepi@igloonet.cz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar sql-capitalize-keywords nil
  "Capitalize keywords in SQL mode.")

(defvar sql-capitalize-keywords-disable-interactive nil
  "Do not capitalize keywords in interactive session (e.g. psql).")

(defvar sql-capitalize-keywords-blacklist '("name")
  "List of keywords to ignore during capitalization.")
