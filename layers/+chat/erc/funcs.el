;;; funcs.el --- Spacemacs ERC Layer functions File
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


(defun erc//servers (server-list)
  (dolist (s server-list)
    (setq s (cl-copy-list s))
    (apply (if
               (plist-get (cdr s) :ssl)
               (progn
                 (cl-remf (cdr s) :ssl)
                 'erc-tls)
             'erc)
           :server s)))

(defun erc/default-servers ()
  (interactive)
  (if erc-server-list
      (erc//servers erc-server-list)
    (message "You must define erc-server-list")))


;; persp

(defun spacemacs//erc-persp-filter-save-buffers-function (buffer)
  "Filter for erc layout."
  (with-current-buffer buffer
    (eq major-mode 'erc-mode)))

(defun spacemacs//erc-buffer-to-persp ()
  "Add buffer to erc layout."
  (persp-add-buffer (current-buffer) (persp-get-by-name
                                      erc-spacemacs-layout-name)))
