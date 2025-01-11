;;; early-init.el --- Spacemacs Early Init File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2020-2024 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
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

;;; Commentary:
;;
;; Early init file for Spacemacs to prevent Emacs from automatically
;; initializing the package manager before loading the init file.
;; This ensures that Spacemacs retains control over package initialization.

;;; Code:

;; Disable automatic package initialization at startup
(setq package-enable-at-startup nil)

;; Load early core functions for Spacemacs
(load (expand-file-name "core/core-early-funcs" (file-name-directory load-file-name))
      nil (not init-file-debug))

;; Note: The hooks below for disabling GUI elements are commented out
;; as they can interfere with user configurations in `dotspacemacs/user-config`.
;; Spacemacs disables GUI elements in `spacemacs/init`, so these hooks
;; are not typically necessary. Original comments retained for context.

;; ;; Disable GUI elements to prevent graphical glitches
;; (add-hook 'window-setup-hook 'spacemacs/toggle-gui-elements-off)
;; (add-hook 'tty-setup-hook 'spacemacs/toggle-gui-elements-off)

;;; early-init.el ends here
