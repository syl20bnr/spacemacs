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
      '(ob-http
        restclient))

(defun restclient/init-ob-http ()
  (use-package ob-http
    :init
    (progn
      (when restclient-use-org
        (add-to-list 'auto-mode-alist '("\\.http\\'" . org-mode)))
      (spacemacs|use-package-add-hook org
        :post-config (add-to-list 'org-babel-load-languages '(http . t))))))

(defun restclient/init-restclient ()
  (use-package restclient
    :defer t
    :init
    (progn
      (unless restclient-use-org
        (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
      (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
        "s" 'restclient-http-send-current-stay-in-window
        "S" 'restclient-http-send-current
        "r" 'restclient-http-send-current-raw-stay-in-window
        "R" 'restclient-http-send-current-raw
        "y" 'restclient-copy-curl-command))))
