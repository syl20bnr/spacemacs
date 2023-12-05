;;; config.el --- EXWM Layer packages File for Spacemacs
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

(defvar exwm-terminal-command "xterm"
  "Terminal command to run.")

(defvar exwm-locking-command nil
  "Command to run when locking session")

(defvar exwm-hide-tiling-modeline nil
  "Whether to hide modeline.")

;; (defvar exwm-leader-key nil
;;   "Key to use for EXWM global commands")

(defvar exwm-enable-systray nil
  "When non-nil, enable system tray integration for EXWM.")

(defvar exwm-autostart-xdg-applications nil
  "When non-nil, autostart applications in  $XDG_USER_HOME/autostart directory.")

(defvar exwm-workspace-switch-wrap t
  "When non-nil, `exwm/exwm-workspace-next' and `exwm/exwm-workspace-prev' should wrap.")

(defvar exwm--randr-displays (thread-first
                                 "xrandr --listmonitors | grep \"[0-9]\\+:\" | cut -d' ' -f2,6 "
                               (shell-command-to-string)
                               (string-trim)
                               (split-string ": "))
  "List of defined monitors.")

(defvar exwm-randr-command nil
  "`xrandr' command to set up displays prior to EXWM init.
For example,
  \"xrandr --output VGA1 --left-of LVDS1 --auto\".")
