;;; config.el --- Git Layer configuration File for Spacemacs
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

(defvar git-enable-github-support nil
  "If non nil the Github packages and extensions are enabled.")

(setq git/key-binding-prefixes '(("gh" . "gutter-hunks/highlight")))
(setq magit-completing-read-function 'magit-ido-completing-read)

(when git-enable-github-support
  (push (cons "gf" "file") git/key-binding-prefixes)
  (push (cons "gg" "gist") git/key-binding-prefixes))
(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      git/key-binding-prefixes)
