;;; packages.el --- dtrt-indent layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Kevin Doherty <kjd@csail.mit.edu>
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


(defconst dtrt-indent-packages '(dtrt-indent))

(defun dtrt-indent/init-dtrt-indent ()
  (use-package dtrt-indent
    :hook (prog-mode .
              (lambda ()
                (modify-syntax-entry ?_ "w")
                (dtrt-indent-mode)
                (dtrt-indent-adapt)))
    :config
    (progn
      (spacemacs|hide-lighter dtrt-indent-mode))))

;;; packages.el ends here
