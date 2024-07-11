;;; packages.el --- epub layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Jeremy Dormitzer <jeremy.dormitzer@gmail.com>
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


(defconst epub-packages '(nov))

(defun epub/init-nov ()
  (use-package nov
    :defer t
    :mode ("\\.epub\\'" . nov-mode)
    :config
    (evilified-state-evilify-map nov-mode-map
      :mode nov-mode
      :bindings
      (kbd "H") 'nov-previous-document
      (kbd "L") 'nov-next-document
      (kbd "[") 'nov-previous-document
      (kbd "]") 'nov-next-document
      (kbd "d") 'nov-scroll-up
      (kbd "u") 'nov-scroll-down
      (kbd "J") 'nov-scroll-up
      (kbd "K") 'nov-scroll-down
      (kbd "gm") 'nov-display-metadata
      (kbd "gr") 'nov-render-document
      (kbd "gt") 'nov-goto-toc
      (kbd "gv") 'nov-view-source
      (kbd "gV") 'nov-view-content-source)))
