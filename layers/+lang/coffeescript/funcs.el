;;; funcs.el --- CoffeeScript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//coffeescript-indent-hook ()
  (setq indent-line-function 'spacemacs//coffeescript-indent
        evil-shift-width coffee-tab-width))

(defun spacemacs//coffeescript-indent ()
  (if (coffee-line-wants-indent)
      ;; We need to insert an additional tab because
      ;; the last line was special.
      (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
    ;; otherwise keep at the same indentation level
    (coffee-insert-spaces (coffee-previous-indent))))
