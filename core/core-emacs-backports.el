;;; core-emacs-backports.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


;; nothing for now

(provide 'core-emacs-backports)

(when (version< emacs-version "26.3")
  ;; Hack to prevent TLS error with Emacs 26.1 and 26.2 and gnutls 3.6.4 and
  ;; above see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
  (message (concat "Testing if your Emacs version %s and GnuTLS version "
                   "need the TLS work-around...")
           emacs-version)
  (message "More info at https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341")
  (unless (<= libgnutls-version 30603)
    (message (concat "Your Emacs version %s and GnutTLS version %s "
                     "need the work-around, applying it...")
             emacs-version libgnutls-version)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))
