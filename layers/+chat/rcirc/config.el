;;; config.el --- rcirc Layer configuration File for Spacemacs
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


;; Variables

(defvar rcirc-enable-authinfo-support nil
  "if non nil then authentication uses authinfo.")

(defvar rcirc-enable-emojify nil
  "if non nil then automatically enable emojify-mode in rcirc buffers.")

(defvar rcirc-enable-erc-image nil
  "if non nil then enable erc-image to display images in rcirc.")

(defvar rcirc-enable-erc-tweet nil
  "if non nil then enable erc-tweet to display tweets in rcirc.")

(defvar rcirc-enable-erc-yt nil
  "if non nil then enable erc-yt to display YouTube previews in rcirc.")

(defvar rcirc-enable-late-fix nil
  "if non nil then enable rcirc-late-fix to show s/// fixes in rcirc buffers.")

(defvar rcirc-enable-styles nil
  "if non nil then enable rcirc-styles to parse style markup codes in rcirc.")

(defvar rcirc-enable-znc-support nil
  "if non nil then znc is enabled.")

(defvar rcirc-spacemacs-layout-name "@RCIRC"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar rcirc-spacemacs-layout-binding "i"
  "Binding used in the setup for `spacemacs-layouts' micro-state")
