;;; packages.el --- Dash Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
    :init 
    (spacemacs/declare-prefix "arz" "zeal/dash docs")
    (spacemacs/set-leader-keys
      "azh" 'helm-dash-at-point
      "azH" 'helm-dash)
    :config (when dash-autoload-common-docsets
              (dash//activate-package-docsets dash-docs-docset-newpath))))

(defun dash/init-counsel-dash ()
  (use-package counsel-dash
    :defer t
    :init 
    (spacemacs/declare-prefix "arz" "zeal/dash docs")
    (spacemacs/set-leader-keys
      "arzh" 'counsel-dash-at-point
      "arzH" 'counsel-dash)
    :config (when dash-autoload-common-docsets
              (dash//activate-package-docsets dash-docs-docset-newpath))))

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :defer t
    :init 
    (spacemacs/declare-prefix "arz" "zeal/dash docs")
    (spacemacs/set-leader-keys
      "arzd" 'dash-at-point
      "arzD" 'dash-at-point-with-docset)))

(defun dash/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init 
    (spacemacs/declare-prefix "arz" "zeal/dash docs")
    (spacemacs/set-leader-keys
      "arzd" 'zeal-at-point
      "arzD" 'zeal-at-point-set-docset)
    :config
    ;; This lets users search in multiple docsets
    (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html,css,javascript"))))
