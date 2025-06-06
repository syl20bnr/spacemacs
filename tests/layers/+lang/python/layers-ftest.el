;;; layers-ftest.el --- Spacemacs Python Layer Test File  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Lin Sun <sunlin7 AT hotmail.com>
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

(require 'ert-x)

(ert-deftest python-shell-interpreter-venv-first ()
  "Test the python-shell-interpreter should check .venv first."
  (ert-with-temp-directory dir
    (let ((vbin (expand-file-name
                 (if (eq system-type 'windows-nt) ".venv/Scripts" ".venv/bin")
                 dir)))
      (mkdir vbin 'recursive)
      (let* ((default-directory vbin)
             (tmpfile (make-temp-file "testing-"))
             (pyshell (concat "ipython" (car exec-suffixes))))
        (copy-file tmpfile pyshell)
        (set-file-modes pyshell #o755))
      (with-current-buffer (find-file-noselect (expand-file-name "t.py" dir))
        (should (string-match-p ".venv/" python-shell-interpreter))))))
