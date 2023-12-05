;;; config.el --- mu4e Layer configuration File for Spacemacs
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


(defvar mu4e-installation-path nil
  "Installation path for mu4e.")

(defvar mu4e-spacemacs-layout-name "@Mu4e"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar mu4e-spacemacs-layout-binding "m"
  "Binding used in the setup for `spacemacs-layouts' micro-state")

(defvar mu4e-spacemacs-kill-layout-on-exit t
  "When `t` exiting mu4e app will automatically kill its layout")

(defvar mu4e-enable-async-operations nil
  "Prefer async operations when sending emails.")

(defvar mu4e-enable-notifications nil
  "If non-nil, enable desktop notifications for unread emails.")

(defvar mu4e-enable-mode-line nil
  "If non-nil, enable display of unread emails in mode-line.")

(defvar mu4e-list-modes
  '(mu4e-main-mode mu4e-headers-mode)
  "Modes that are associated with mu4e's listing buffers.")

(defvar mu4e-view-modes
  '(mu4e-view-mode mu4e-compose-mode mu4e-loading-mode)
  "Modes that are associated with mu4e's view/compose buffers.")

(defvar mu4e-modes
  '(append 'mu4e-list-modes 'mu4e-view-modes)
  "Modes that are associated with mu4e buffers.")

(defvar mu4e-org-link-support t
  "If non-nil mu4e-org is configured.")

(defvar mu4e-org-compose-support nil
  "If non-nil org-mu4e is configured.")

(defvar mu4e-autorun-background-at-startup nil
  "If non-nil, mu4e will automatically run in background at emacs startup.")

(when mu4e-installation-path
  (add-to-list 'load-path mu4e-installation-path))
