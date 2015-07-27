;;; extensions.el --- c-c++ layer extensions for Spacemacs
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

;; Post extensions are loaded *before* the packages
(setq c-c++-pre-extensions '())

;; Post extensions are loaded *after* the packages
(setq c-c++-post-extensions
  '(
    disaster
    ))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (evil-leader/set-key-for-mode 'c-mode
      "md" 'disaster)
    (evil-leader/set-key-for-mode 'c++-mode
      "md" 'disaster)
    ))
