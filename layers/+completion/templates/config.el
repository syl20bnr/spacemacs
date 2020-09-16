;;; config.el --- Tempalte Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar templates-private-directory
  (concat spacemacs-private-directory "templates")
  "Configurable private templates directory.")

(defvar templates-use-default-templates t
  "If true, do not remove the default settings for
`auto-insert-mode'. If `nil', ONLY the ones specified by template
files will be used.")
