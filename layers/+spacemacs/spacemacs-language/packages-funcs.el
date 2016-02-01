;;; packages.el --- Spacemacs Language Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-language/set-google-translate-languages (source target)
  "Set source language for google translate.
For instance pass En as source for English."
  (interactive
   "sEnter source language (ie. en): \nsEnter target language (ie. fr): "
   source target)
  (message
   (format "Set google translate source language to %s and target to %s"
           source target))
  (setq google-translate-default-source-language (downcase source))
  (setq google-translate-default-target-language (downcase target)))
