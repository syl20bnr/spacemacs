;;; core-release-management-ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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

(require 'core-spacemacs)

;; ---------------------------------------------------------------------------
;; spacemacs//git-has-remote
;; ---------------------------------------------------------------------------

(ert-deftest test-git-has-remote--command-is-working ()
  (should (numberp (spacemacs//git-has-remote "origin"))))

(ert-deftest test-git-has-remote--input-is-not-a-remote ()
  (should (equal (spacemacs//git-has-remote "clearly-not-a-R3M0T3!") nil)))

;; ---------------------------------------------------------------------------
;; spacemacs//git-fetch-tags
;; ---------------------------------------------------------------------------

(ert-deftest test-git-fetch-tags--command-is-working ()
  (should (equal t (spacemacs//git-fetch-tags "origin" "master"))))

(ert-deftest test-git-fetch-tags--input-is-not-a-remote ()
  (should (equal nil (spacemacs//git-fetch-tags "qwerty" "master"))))
