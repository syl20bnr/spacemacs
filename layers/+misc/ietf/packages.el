;;; packages.el --- ietf layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Christian Hopps <chopps@gmail.com>
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


;;; Code:

(defconst ietf-packages '(ietf-docs
                          (irfc :location local)
                          (ox-rfc :requires org)))

(defun ietf/init-ietf-docs ()
  (use-package ietf-docs
    :commands ietf-docs-open-at-point
    :init
    (spacemacs/set-leader-keys "f I" 'ietf-docs-open-at-point)
    (setq ietf-docs-cache-directory ietf-docs-cache)))

(defun ietf/init-irfc ()
  (use-package irfc
    :commands irfc-mode
    :init
    (setq irfc-directory ietf-docs-cache)
    (setq irfc-assoc-mode t)
    (add-to-list 'auto-mode-alist
                 '("/draft-\\([a-z0-9_]+-\\)+[a-z0-9_]+.txt" . irfc-mode))
    (add-to-list 'auto-mode-alist
                 '("/rfc\\([a-z0-9_-]+\\).txt" . irfc-mode))))

(defun ietf/init-ox-rfc ()
  (use-package ox-rfc :after ox))


;;; packages.el ends here
