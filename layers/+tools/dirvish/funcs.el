;;; funcs.el --- ranger Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thanh <thanhvg@gmail.com>
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


(defun spacemacs/dirvish-subtree-beginning ()
  "Jump to beginning of subtree."
  (interactive)
  (when-let ((ov (dirvish-subtree--parent)))
    (goto-char (overlay-start ov))
    (dired-previous-line 1)))

;; h binding
(defun spacemacs/dirvish-collapse-or-up ()
  "Collapse or go up node."
  (interactive)
  (if (and (file-directory-p (dired-get-filename))
           (dirvish-subtree--expanded-p))
          (progn
            (dired-next-line 1)
            (dirvish-subtree-remove))
    (spacemacs/dirvish-subtree-beginning)))

;; l key
(defun spacemacs/dirvish-expand-or-open ()
  "Expand or open node."
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (if (dirvish-subtree--expanded-p)
          (dired-next-line 1)
        (dirvish-subtree--insert))
    (dired-find-file)))
