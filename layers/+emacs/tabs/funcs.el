;;; funcs.el --- tabs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018, 2020 Sylvain Benner & Contributors
;;
;; Author: Deepu Puthrote <git@deepumohan.com>
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

(defun spacemacs//tabs-timer-initialize (secs)
  (setq spacemacs-tabs-timer (run-with-timer secs nil (lambda () (centaur-tabs-local-mode 1)))))

(defun spacemacs//tabs-timer-hide ()
  (spacemacs//tabs-timer-initialize tabs-auto-hide-delay))

(defun spacemacs//tabs-switch-and-hide (arg)
  (cancel-timer spacemacs-tabs-timer)
  (centaur-tabs-local-mode 1)
  ;; (if arg
  ;;     (centaur-tabs-backward)
  ;;   (centaur-tabs-forward))
  (pcase arg
    ('backward (centaur-tabs-backward))
    ('forward (centaur-tabs-forward))
    ('backward-group (centaur-tabs-backward-group))
    ('forward-group (centaur-tabs-forward-group)))
  (centaur-tabs-local-mode 0)
  (spacemacs//tabs-timer-hide))

(defun spacemacs//centaur-tabs-forward-and-hide ()
  (spacemacs//tabs-switch-and-hide 'forward))

(defun spacemacs//centaur-tabs-backward-and-hide ()
  (spacemacs//tabs-switch-and-hide 'backward))

(defun spacemacs/tabs-forward ()
  (interactive)
  (if tabs-auto-hide
      (spacemacs//centaur-tabs-forward-and-hide)
    (centaur-tabs-forward)))

(defun spacemacs/tabs-backward ()
  (interactive)
  (if tabs-auto-hide
      (spacemacs//centaur-tabs-backward-and-hide)
    (centaur-tabs-backward)))

(defun spacemacs/tabs-forward-group-and-hide ()
  (interactive)
  (spacemacs//tabs-switch-and-hide 'forward-group))

(defun spacemacs/tabs-backward-group-and-hide ()
  (interactive)
  (spacemacs//tabs-switch-and-hide 'backward-group))
