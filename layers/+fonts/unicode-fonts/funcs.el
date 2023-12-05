;;; funcs.el --- unicode-fonts layer funcs file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Lucius Hu
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

;;; Code:

(defun unicode-fonts//setup-fonts (frame)
  "Setup `unicode-fonts' package for FRAME.

This functions setups `unicode-fonts' right away when starting a GUI Emacs.
But if Emacs is running in a daemon, it postpone the setup until a GUI frame
is opened."
  (if (and frame (display-graphic-p frame))
      (with-selected-frame frame
        (require 'unicode-fonts)
        (unicode-fonts-setup)
        (remove-hook 'after-make-frame-functions #'unicode-fonts//setup-fonts))
    (add-hook 'after-make-frame-functions #'unicode-fonts//setup-fonts)))

;;; funcs.el ends here
