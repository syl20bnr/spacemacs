;;; packages.el --- forth layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Tim Jaeger <jger.tm@gmail.com>
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


(defconst forth-packages '(forth-mode))

(defun forth/init-forth-mode ()
  (use-package forth-mode
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'forth-mode
            "ds" 'forth-see
            "eE" 'forth-eval
            "ee" 'forth-eval-last-expression
            "er" 'forth-eval-region
            "sb" 'forth-load-file
            "si" 'run-forth
            "sk" 'forth-kill)))
