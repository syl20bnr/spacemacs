;;; packages.el --- sml Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Keith Simmons & Contributors
;;
;; Author: Keith Simmons <keith@the-simmons.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; As a note, this only works because s is after c in the alphabet...
;; I am not a powerful enough elisp wizard to figure out a more robust
;; way to do this... Maybe somebody else knows more and can help...

(setq key-seq-packages
      '(
        key-seq
        key-chord
        ))

(defun key-seq/init-key-seq ()
  (use-package key-seq
    :defer))

(defun key-seq/init-key-chord ()
  (use-package key-chord
    :defer
    :init
    (key-chord-mode 1)))
