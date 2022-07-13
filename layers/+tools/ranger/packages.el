;;; packages.el --- ranger Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Rich Alesi
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


(defconst ranger-packages
  '(
    (dired :location built-in)
    golden-ratio
    ranger))

(defun ranger//set-leader-keys ()
  (spacemacs/declare-prefix "atr" "ranger/deer")
  (spacemacs/set-leader-keys
    "atrr" 'ranger
    "atrd" 'deer
    "jD" 'deer-jump-other-window
    "jd" 'deer))

(defun ranger/init-ranger ()
  (use-package ranger
    :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
    :init
    (progn
      (ranger//set-leader-keys)

      (when (memq ranger-enter-with-minus '(deer ranger))
        (define-key evil-motion-state-map (kbd "-") ranger-enter-with-minus))

      ;; set up image-dired to allow picture resize
      (setq image-dired-dir (concat spacemacs-cache-directory "image-dir"))
      (unless (file-directory-p image-dired-dir)
        (make-directory image-dired-dir)))
    :config
    (progn
      (when (memq 'helm dotspacemacs-configuration-layers)
        (require 'helm))
      (define-key ranger-mode-map (kbd "-") 'ranger-up-directory))))

(defun ranger/post-init-dired ()
  ;; Be sure to override dired bindings
  (ranger//set-leader-keys))

(defun ranger/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-exclude-modes "ranger-mode")))
