;;; packages.el --- languagetool layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
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


(defconst languagetool-packages
  '((langtool :toggle (spacemacs//languagetool-detect))))

(defun languagetool/init-langtool ()
  (use-package langtool
    :defer t
    :commands 'langtool-correct-buffer
    :init
    (progn
      ;; The whitespace rules give a lot of false positives when linting rich
      ;; text.
      (setq-default langtool-disabled-rules '("WHITESPACE_RULE"))
      (spacemacs/set-leader-keys
        "Sl" 'spacemacs/languagetool-toggle
        "SL" 'langtool-correct-buffer)
      (define-key evil-normal-state-map (kbd "[ a")
        'spacemacs/languagetool-previous-error)
      (define-key evil-normal-state-map (kbd "] a")
        'spacemacs/languagetool-next-error))))
