;;; funcs.el -- Passwords Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
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


(defun spacemacs//pass-completing-read ()
  "Read a password entry in the minibuffer, with completion."
  (completing-read "Password entry: " (password-store-list)))

(defun spacemacs/pass-copy-and-describe (entry)
  "Copy the password to the clipboard, and show the multiline description for ENTRY"
  (interactive (list (spacemacs//pass-completing-read)))
  (password-store-copy entry)
  (spacemacs/pass-describe entry))

(defun spacemacs/pass-describe (entry)
  "Show the multiline description for ENTRY"
  (interactive (list (spacemacs//pass-completing-read)))
  (let ((description (s-join "\n" (cdr (s-lines (password-store--run-show entry))))))
    (message "%s" description)))
