;;; packages.el --- parinfer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: DogLooksGood <DogLooksGood@rMBP>
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


(defconst parinfer-packages
  '(parinfer-rust-mode))

(defun parinfer/init-parinfer-rust-mode ()
  (use-package parinfer-rust-mode
    :defer t
    :diminish parinfer-rust-mode
    :hook emacs-lisp-mode clojure-mode scheme-mode common-lisp-mode
    :init
    (progn
      (setq parinfer-rust-auto-download parinfer-auto-download)
      (if parinfer-rust-auto-download
          (setq parinfer-rust-library-directory
                (concat spacemacs-cache-directory
                        "parinfer-rust/"))
        (when parinfer-library
          (setq parinfer-rust-library parinfer-library)))
      (spacemacs|add-toggle parinfer-smart-indent
        :evil-leader "tP"
        :documentation "Enable Parinfer Smart Indent Mode."
        :if (bound-and-true-p parinfer-rust-mode)
        :status (eq parinfer-rust--mode 'smart)
        :on (parinfer-rust-toggle-paren-mode)
        :off (parinfer-rust-toggle-paren-mode)))))
