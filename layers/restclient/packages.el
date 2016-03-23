;;; packages.el --- restclient Layer Packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq restclient-packages
  '((ob-http :toggle restclient-use-org)
    (restclient :toggle (null restclient-use-org))))

(defun restclient/init-ob-http ()
  (use-package ob-http
    :mode ("\\.http\\'" . org-mode)
    :init (spacemacs|use-package-add-hook org
            :post-config (add-to-list 'org-babel-load-languages '(http . t)))))

(defun restclient/init-restclient ()
  (use-package restclient
    :mode ("\\.http\\'" . restclient-mode)
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
            "s" 'restclient-http-send-current-stay-in-window
            "S" 'restclient-http-send-current
            "r" 'restclient-http-send-current-raw-stay-in-window
            "R" 'restclient-http-send-current-raw
            "y" 'restclient-copy-curl-command)))
