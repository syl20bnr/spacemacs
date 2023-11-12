;;; config.el --- Helm Configuration File for Spacemacs
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


;; Dumper

(defun helm/pre-dump ()
  (spacemacs/dump-modes '(helm-mode)))

;; Make sure we do not try to split the action window
;; from the main view, see #16184 for details.
;; This can still be overwritten in the dotfile.
(defvar helm-show-action-window-other-window nil)

;; variables

;; TODO: remove dotspacemacs variables backward compatbility in version
;;       0.400 or later
(defvar helm-no-header (spacemacs|dotspacemacs-backward-compatibility
                        dotspacemacs-helm-no-header nil)
  "if non nil, the helm header is hidden when there is only one source.")

(defvar helm-position (spacemacs|dotspacemacs-backward-compatibility
                       dotspacemacs-helm-position bottom)
  "Position in which to show the `helm' mini-buffer.")

(defvar spacemacs-helm-rg-max-column-number 512
  "Controls the maximum number of columns to display with ripgrep (otherwise
  omits a line)")

;; internals

;; for Helm Window position
(defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar spacemacs-helm-display-buffer-regexp
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . ,helm-position)
    (window-width . 0.6)
    (window-height . 0.4)))
(defvar spacemacs-display-buffer-alist nil)
