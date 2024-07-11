;;; packages.el --- sailfish-developer layer packages file for Spacemacs.
;;
;; Copyright (c) 2020-2024 Sylvain Benner & Contributors
;;
;; Author: Victor Polevoy <fx@thefx.co>
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


(defconst sailfish-developer-packages
  '(sailfish-scratchbox))

(defun sailfish-developer/init-sailfish-scratchbox ()
  (use-package sailfish-scratchbox
    :defer t
    :init
    (spacemacs/declare-prefix "cs" "sailfish os developer menu")
    (spacemacs/set-leader-keys
      "csb" 'sailfish-scratchbox-mb2-build
      "csd" 'sailfish-scratchbox-deploy-rpms
      "csi" 'sailfish-scratchbox-install-rpms)))
