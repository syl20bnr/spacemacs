;;; packages.el --- Salt Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Ben Hayden <hayden767@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; Salt mode URL: https://github.com/beardedprojamz/salt-mode
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

(setq salt-packages '(salt-mode
                      smartparens))

(defun salt/init-salt-mode ()
  (use-package salt-mode
    :defer t
    :config (spacemacs/set-leader-keys-for-major-mode 'salt-mode "pb" 'mmm-parse-buffer)))

(defun salt/pre-init-smartparens ()
  (add-hook 'salt-mode-hook #'spacemacs//activate-smartparens)
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (progn
      (sp-local-pair 'salt-mode "{{" " }}")
      (sp-local-pair 'salt-mode "{%" " %}")
      (sp-local-pair 'salt-mode "{#" " #}"))))
