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

(defun osx/enable-separate-clipboards ()
  "Enable treating OSX pasteboard and spacemacs kill-ring as
separate clipboards."
  (interactive)
  (when (and (spacemacs/system-is-mac)
             (not (display-graphic-p))
             (fboundp 'turn-off-pbcopy))
    (turn-off-pbcopy))
  (setq select-enable-clipboard nil)
  (setq osx-use-separate-clipboards t)
  (message "Now using separate clipboards."))

(defun osx/disable-separate-clipboards ()
  "Disable treating OSX pasteboard and spacemacs kill-ring as
separate clipboards."
  (interactive)
  (when (and (spacemacs/system-is-mac)
             (not (display-graphic-p))
             (fboundp 'turn-on-pbcopy))
    (turn-on-pbcopy))
  (setq select-enable-clipboard t)
  (setq osx-use-separate-clipboards nil)
  (message "Now using a unified clipboard."))
