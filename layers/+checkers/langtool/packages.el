;;; packages.el --- langtool layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst langtool-packages
  '(langtool))

;; Author: Steve Shogren https://github.com/steveshogren
(defun langtool/init-langtool ()
    (use-package langtool
        :defer t
        :init
        (progn
          (global-set-key "\M-n" 'langtool-goto-next-error)
          (message "Langtool loaded!"))))
