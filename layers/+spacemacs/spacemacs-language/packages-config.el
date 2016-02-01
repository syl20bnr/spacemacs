;;; packages-config.el --- Spacemacs Language Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-language/init-define-word ()
  (use-package define-word
    :defer t
    :init
    (spacemacs/set-leader-keys
      "xwd" 'define-word-at-point)))

(defun spacemacs-language/init-google-translate ()
  (use-package google-translate
    :init
    (progn
      (spacemacs/set-leader-keys
        "xgl" 'spacemacs/set-google-translate-languages
        "xgQ" 'google-translate-query-translate-reverse
        "xgq" 'google-translate-query-translate
        "xgT" 'google-translate-at-point-reverse
        "xgt" 'google-translate-at-point))
    ))
