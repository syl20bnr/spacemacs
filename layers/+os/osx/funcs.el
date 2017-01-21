;;; config.el --- OSX Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun osx/list-available-dictionaries ()
  "Get list of available dictionaries.

Useful when setting `osx-dictionary-dictionary-choice'."
  (interactive)
  (message (shell-command-to-string
     (format "%s -l" (osx-dictionary-cli-find-or-recompile)))))
