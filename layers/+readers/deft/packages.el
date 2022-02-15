;;; packages.el --- deft Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;         Bruno Morais <brunosmmm@gmail.com>
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


(defconst deft-packages
  '(
    deft
    (zetteldeft :toggle (eq deft-zetteldeft t))))

(defun deft/init-zetteldeft ()
  (use-package zetteldeft
    :if (eq deft-zetteldeft t)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'deft-mode "mz" "zetteldeft")
      (spacemacs/declare-prefix-for-mode 'org-mode "mz" "zetteldeft")
      (spacemacs/declare-prefix "ardz" "zetteldeft")
      ;; zetteldeft actions in deft mode
      (spacemacs/set-leader-keys-for-major-mode 'deft-mode
        "zT" 'zetteldeft-tag-buffer
        "zn" 'zetteldeft-new-file)
      ;; zetteldeft actions in org mode
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "zc" 'zetteldeft-search-current-id
        "zf" 'zetteldeft-follow-link
        "zt" 'zetteldeft-avy-tag-search
        "zN" 'zetteldeft-new-file-and-link
        "zr" 'zetteldeft-file-rename
        "zi" 'zetteldeft-find-file-id-insert
        "zI" 'zetteldeft-find-file-full-title-insert
        "zs" 'zetteldeft-search-at-point
        "zl" 'zetteldeft-avy-link-search
        "zF" 'zetteldeft-avy-file-search-ace-window
        "zo" 'zetteldeft-find-file)
      ;; new zetteldeft file under capture
      (spacemacs/set-leader-keys "Cz" 'zetteldeft-new-file)
      ;; actions under applications/deft/zetteldeft
      (spacemacs/set-leader-keys "ardzn" 'zetteldeft-new-file)
      (spacemacs/set-leader-keys "ardzT" 'zetteldeft-tag-buffer)
      (spacemacs/set-leader-keys "ardzs" 'zetteldeft-search-at-point)
      (spacemacs/set-leader-keys "ardzo" 'zetteldeft-find-file))))

(defun deft/init-deft ()
  (use-package deft
    :defer t
    :init
    (progn
      (setq deft-extensions '("org" "md" "txt")
            deft-text-mode 'org-mode
            deft-use-filename-as-title t
            deft-use-filter-string-for-filename t)
      ;; in applications prefix, NOTE: backward incompatible keybindings
      (if deft-zetteldeft
          (progn
            (spacemacs/declare-prefix "ard" "deft")
            (spacemacs/set-leader-keys "ardn" 'spacemacs/deft))
        (spacemacs/set-leader-keys "ard" 'spacemacs/deft))
      ;; put in capture prefix
      (spacemacs/set-leader-keys "Cd" 'deft-new-file))
    :config (spacemacs/set-leader-keys-for-major-mode 'deft-mode
             "c" 'deft-filter-clear
             "d" 'deft-delete-file
             "i" 'deft-toggle-incremental-search
             "n" 'deft-new-file
             "N" 'deft-new-file-named
             "q" 'quit-window
             "o" 'deft-open-file-other-window
             "r" 'deft-rename-file)))
