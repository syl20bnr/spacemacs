;;; funcs.el --- Spell Checking Layer functions File for Spacemacs
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

(defun spell-checking/add-flyspell-hook (mode &optional target)
  "Enable flyspell for the given MODE, if
`spell-checking-enable-by-default' is true."
  (when spell-checking-enable-by-default
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook 'flyspell-mode))))

(defun spell-checking/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'adict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))
