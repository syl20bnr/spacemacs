;;; packages.el --- Puppet layer packages File for Spacemacs
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


(setq puppet-packages
  '(
    company
    flycheck
    puppet-mode
    ))

(defun puppet/init-puppet-mode ()
  (use-package puppet-mode
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'puppet-mode
      "{" 'beginning-of-defun
      "}" 'end-of-defun
      "$" 'puppet-interpolate
      "a" 'puppet-align-block
      "'" 'puppet-toggle-string-quotes
      ";" 'puppet-clear-string
      "j" 'imenu
      "c" 'puppet-apply
      "v" 'puppet-validate
      "l" 'puppet-lint
      )))

(defun puppet/post-init-company ()
  (spacemacs|add-company-backends :modes puppet-mode))

(defun puppet/post-init-flycheck ()
  (spacemacs/enable-flycheck 'puppet-mode))
