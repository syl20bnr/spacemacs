;;; core-early-funcs.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; This file is sourced by emacs early-init.el file.
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

(defun spacemacs//toggle-gui-elements (&optional on-off)
  "Toggle menu bar, tool bar, scroll bars, and tool tip modes. If
optional ON-OFF is not specified, then toggle on/off state. If
ON-OFF is 0 or 1, then turn gui elements OFF or ON respectively."
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode (or on-off (not scroll-bar-mode))))
  (when  (fboundp 'tool-bar-mode)
    (tool-bar-mode (or on-off (not tool-bar-mode))))
  (unless (memq (window-system) '(mac ns))
    (when (fboundp 'menu-bar-mode)
      (menu-bar-mode (or on-off (not menu-bar-mode)))))
  ;; tooltips in echo-aera
  (when (fboundp 'tooltip-mode)
    (tooltip-mode (or on-off (not tooltip-mode)))))

(provide 'core-early-funcs)
