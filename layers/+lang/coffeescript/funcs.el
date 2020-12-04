;;; funcs.el --- CoffeeScript Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//coffeescript-indent-hook ()
  (setq indent-line-function 'space-macs//coffeescript-indent
        evil-shift-width coffee-tab-width))

(defun space-macs//coffeescript-indent ()
  (if (coffee-line-wants-indent)
      ;; We need to insert an additional tab because
      ;; the last line was special.
      (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
    ;; otherwise keep at the same indentation level
    (coffee-insert-spaces (coffee-previous-indent))))


