;;; funcs.el --- Jabber layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defun spacemacs/jabber-connect-hook (jc)
  (jabber-send-presence "" "Online" 10)
  (jabber-whitespace-ping-start)
  ;; Disable the minibuffer getting jabber messages when active
  ;; See http://www.emacswiki.org/JabberEl
  (define-jabber-alert echo "Show a message in the echo area"
    (lambda (msg &optional title)
      (unless (minibuffer-prompt)
        (message "%s" (or title msg))))))
