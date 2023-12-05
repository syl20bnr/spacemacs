;;; packages.el --- xclipboard layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Authors: Charles Weill <weill@google.com>
;;          Google LLC.
;;          Hong Xu <hong@topbug.net>
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


(defconst xclipboard-packages
  '(
    (spacemacs-xclipboard :location local)
    (cliphist :requires ivy
              :toggle xclipboard-enable-cliphist)
    ))

(defun xclipboard/init-cliphist ()
  (use-package cliphist
    :init (spacemacs/set-leader-keys
            "xP" 'cliphist-paste-item
            "xR" 'spacemacs/xclipboard-cliphist-paste-item-rectangle
            "xs" 'cliphist-select-item)
    :config (setq cliphist-cc-kill-ring t)))

(defun xclipboard/init-spacemacs-xclipboard ()
  (use-package spacemacs-xclipboard
    :init (spacemacs/set-leader-keys
            "xp" 'spacemacs/xclipboard-paste
            "xy" 'spacemacs/xclipboard-copy)))
