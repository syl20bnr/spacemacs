;;; funcs.el --- mu4e Layer functions File for Spacemacs
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


(defun mu4e/load-signature-from-file (file)
  "Load signature from FILE and strip separator if needed."
  (setq mu4e-compose-signature
        (with-temp-buffer
          (insert-file-contents file)
          (flush-lines message-signature-separator)
          (buffer-string))))

(defun mu4e/message-is-for-p (msg rx)
  "Check if to, cc or bcc field in MSG has any address in RX."
  (when (and msg rx)
    (or (mu4e-message-contact-field-matches msg :to rx)
        (mu4e-message-contact-field-matches msg :cc rx)
        (mu4e-message-contact-field-matches msg :bcc rx))))
