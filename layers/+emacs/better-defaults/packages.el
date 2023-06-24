;;; packages.el --- Better Emacs Defaults Layer functions File
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
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


(defconst better-defaults-packages
  '(
    mwim
    unfill
    )
  "The list of Lisp packages required by the mwim layer.")

(defun better-defaults/init-mwim ()
  (use-package mwim
    :defer t
    :init
    (if better-defaults-move-to-beginning-of-code-first
        (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
      (global-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code))

    (if better-defaults-move-to-end-of-code-first
        (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
      (global-set-key (kbd "C-e") 'mwim-end-of-line-or-code))))

(defun better-defaults/init-unfill ()
  (use-package unfill
    :defer t
    :commands (unfill-region unfill-paragraph unfill-toggle)
    :init
    (global-set-key [remap fill-paragraph] #'unfill-toggle)))
