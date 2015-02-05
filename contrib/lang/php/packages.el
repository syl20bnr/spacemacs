;;; packages.el --- PHP Layer packages File for Spacemacs
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

(defvar php-packages '(php-mode)
  "A list of the pacakges to install for php-mode.")

(defun php/init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode)
    :config (progn (evil-leader/set-key-for-mode 'php-mode
      "msr" 'php-send-region
      "mal" 'php-show-arglist
      "msd" 'php-search-documentation
      "mbm" 'php-browse-manual
      "mcv" 'php-mode-version))
  )
)
