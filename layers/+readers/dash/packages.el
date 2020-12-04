;;; packages.el --- Dash Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; see conditional package inclusion
(defconst dash-packages
  '(
    (dash-at-point :toggle (space-macs/system-is-mac))
    (helm-dash :requires helm)
    (counsel-dash :requires ivy)
    (zeal-at-point :toggle (or (space-macs/system-is-linux)
                               (space-macs/system-is-mswindows)))))

(defun dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init (progn
            (space-macs/declare-prefix "arz" "zeal/dash docs")
            (space-macs/set-leader-keys
              "azh" 'helm-dash-at-point
              "azH" 'helm-dash))
    :config (when dash-autoload-common-docsets
              (dash//activate-package-docsets dash-docs-docset-newpath))))

(defun dash/init-counsel-dash ()
  (use-package counsel-dash
    :defer t
    :init (progn
            (space-macs/declare-prefix "arz" "zeal/dash docs")
            (space-macs/set-leader-keys
              "arzh" 'counsel-dash-at-point
              "arzH" 'counsel-dash))
    :config (when dash-autoload-common-docsets
              (dash//activate-package-docsets dash-docs-docset-newpath))))

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :defer t
    :init (progn
            (space-macs/declare-prefix "arz" "zeal/dash docs")
            (space-macs/set-leader-keys
              "arzd" 'dash-at-point
              "arzD" 'dash-at-point-with-docset))))

(defun dash/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init (progn
            (space-macs/declare-prefix "arz" "zeal/dash docs")
            (space-macs/set-leader-keys
              "arzd" 'zeal-at-point
              "arzD" 'zeal-at-point-set-docset))
    :config
    ;; This lets users search in multiple docsets
    (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html,css,javascript"))))


