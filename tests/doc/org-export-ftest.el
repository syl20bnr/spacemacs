;;; org-export-ftest.el --- Spacemacs Org Export Functional Test File  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: smile13241324 <smile13241324@gmail.com>
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
(require 'org)
(require 'core-documentation)

;; -----------------------------------------------------------------------------
;; Spacemacs Documentation HTML Export Test
;; Currently checks whether all org documentation files can be converted to html
;; -----------------------------------------------------------------------------
(ert-deftest test-spacemacs-html-export ()
  (unwind-protect (spacemacs/publish-doc)
    (delete-directory (concat spacemacs-start-directory
                              "export/")
                      t)))
