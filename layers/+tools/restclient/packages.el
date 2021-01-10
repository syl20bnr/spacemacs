;;; packages.el --- restclient Layer Packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq restclient-packages
      '(
        (company-restclient :requires company)
        golden-ratio
        ob-http
        ob-restclient
        restclient
        (restclient-helm :requires helm)
        ))

(defun restclient/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config (add-to-list 'golden-ratio-exclude-modes "restclient-mode")))

(defun restclient/pre-init-ob-http ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-http
      :init (add-to-list 'org-babel-load-languages '(http . t)))))

(defun restclient/init-ob-http ()
  (when restclient-use-org
    (add-to-list 'auto-mode-alist '("\\.http\\'" . org-mode))))

(defun restclient/pre-init-ob-restclient ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-restclient
      :init (add-to-list 'org-babel-load-languages '(restclient . t)))))
(defun restclient/init-ob-restclient ())

(defun restclient/init-restclient ()
  (use-package restclient
    :defer t
    :init
    (progn
      (unless restclient-use-org
        (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
      (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
        "n" 'restclient-jump-next
        "p" 'restclient-jump-prev
        "s" 'restclient-http-send-current-stay-in-window
        "S" 'restclient-http-send-current
        "r" 'spacemacs/restclient-http-send-current-raw-stay-in-window
        "R" 'restclient-http-send-current-raw
        "y" 'restclient-copy-curl-command))))

(defun restclient/init-company-restclient ()
  (use-package company-restclient
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-restclient
            :modes restclient-mode)))

(defun restclient/init-restclient-helm ()
  (use-package restclient-helm
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
            "j" 'helm-restclient)))
