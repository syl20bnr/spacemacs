;;; core-spacemacs-ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Alberto Zaccagni <me@lazywithclass.com>
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

(require 'mocker)
(require 'core-spacemacs)

(ert-deftest assertion-library-should-work ()
  "the assertion library should work"
  (mocker-let ((foo (n)
                    ((:input '(1) :output 1)))
               (bar (n m)
                    ((:input '(2 2) :output 2))))
              (should (equal (foo 1) 1))
              (should (equal (bar 2 2) 2))))
