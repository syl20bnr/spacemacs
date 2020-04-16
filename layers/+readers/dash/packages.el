;;; packages.el --- Dash Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; see conditional package inclusion
(defconst dash-packages
  '(
    (dash-at-point :toggle (spacemacs/system-is-mac))
    (helm-dash :requires helm)
    (counsel-dash :requires ivy)
    (zeal-at-point :toggle (or (spacemacs/system-is-linux)
                               (spacemacs/system-is-mswindows)))))

(defun dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init (progn
            (spacemacs/declare-prefix "az" "zeal/dash docs")
            (spacemacs/set-leader-keys
              "azh" 'helm-dash-at-point
              "azH" 'helm-dash))
    :config (when dash-autoload-common-docsets
              (dash//activate-package-docsets dash-docs-docset-newpath))))

(defun dash/init-counsel-dash ()
  (use-package counsel-dash
    :defer t
    :init (progn
            (spacemacs/declare-prefix "az" "zeal/dash docs")
            (spacemacs/set-leader-keys
              "azh" 'counsel-dash-at-point
              "azH" 'counsel-dash))
    :config (when dash-autoload-common-docsets
              (dash//activate-package-docsets dash-docs-docset-newpath))))

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :defer t
    :init (progn
            (spacemacs/declare-prefix "az" "zeal/dash docs")
            (spacemacs/set-leader-keys
              "azd" 'dash-at-point
              "azD" 'dash-at-point-with-docset))))

(defun dash/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init (progn
            (spacemacs/declare-prefix "az" "zeal/dash docs")
            (spacemacs/set-leader-keys
              "azd" 'zeal-at-point
              "azD" 'zeal-at-point-set-docset))
    :config
    ;; This lets users search in multiple docsets
    (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html,css,javascript"))))
