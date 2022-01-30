;;; packages.el --- systemd layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
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


(defconst systemd-packages
  '(
    flycheck
    systemd
    journalctl-mode)
  "The list of Lisp packages required by the systemd layer.")

(defun systemd/post-init-flycheck ()
  (spacemacs/enable-flycheck 'systemd-mode))

(defun systemd/init-systemd ()
  (use-package systemd
    :defer t
    :init (setq systemd-use-company-p
                (configuration-layer/package-used-p 'company))
    :config (spacemacs/set-leader-keys-for-major-mode 'systemd-mode
              "hd" 'systemd-doc-directives
              "ho" 'systemd-doc-open)))

(defun systemd/init-journalctl-mode ()
  (use-package journalctl-mode
    :ensure t
    :init (progn
            (spacemacs/declare-prefix "atj"  "journalctl")
            (spacemacs/set-leader-keys
              "atjj" 'journalctl
              "atjs" 'journalctl-unit
              "atju" 'journalctl-user-unit
              "atjb" 'journalctl-boot))))

;;; packages.el ends here
