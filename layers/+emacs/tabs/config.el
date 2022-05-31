;;; config.el --- tabs configuration File for Spacemacs
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

(spacemacs|defc tabs-selected-tab-bar 'left
  "When non-nil, display a bar next to the current selected tab in the given direction.

If Emacs is running in daemon mode, this is turned off regardless of the setting."
  '(choice (const left) (const over) (const under)))

(spacemacs|defc tabs-group-by-project t
  "When non-nil, group tabs by projectile project.
Default t. If non-nil calls (tabs-group-by-projectile-project)
Otherwise calls (tabs-group-buffer-groups)"
  '(boolean))

(spacemacs|defc tabs-headline-match t
  "When non-nil, make headline use tabs-default-face. Default t.
Calls (tabs-headline-match)"
  '(boolean))

(spacemacs|defc tabs-auto-hide nil
  "If non-nil hide tabs automatically after TABS-AUTO-HIDE-DELAY seconds."
  '(boolean))

(spacemacs|defc tabs-auto-hide-delay 2
  "Tabs auto hide delay in seconds."
  '(float))
