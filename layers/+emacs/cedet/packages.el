;;; packages.el --- %LAYER_NAME% layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: %USER_FULL_NAME% <%USER_MAIL_ADDRESS%>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst cedet-packages '((cedet :location site)
                           (cedet-contrib :location site)))

(defun cedet/init-cedet()
  "Initialize my package"
  (use-package cedet-devel-load
    :defer t
    :commands cedet-version
    :init
    :config
    ))

(defun cedet/init-cedet-contrib()
  "Initialize my package"
  (use-package cedet-contrib-load
    :defer t
    :commands semantic-scala-cedet-support
    :init
    (progn
      (unless (featurep 'cedet-devel-load)
              (require 'cedet-devel-load)))
    :config
    ))
;;; packages.el ends here
