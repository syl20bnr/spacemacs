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

(defmacro pytest-in-temp-directory (pyname dir pypath &rest body)
  "Bind DIR to the name of a new temporary directory and bind PYPATH to
the path of python interpreter, then evaluate BODY.
The PYNAME is the basename of python interpreter place holder.
Delete the temporary directory after BODY exits normally or
non-locally. It extents the `ert-with-temp-directory' with the PYPATH.
This function will change the `default-directory' to the target directory."
  (declare (indent 3) (debug (symbolp body)))
  `(ert-with-temp-directory ,dir
     (let* ((_bin (if (eq system-type 'windows-nt) "Scripts" "bin"))
            (_vbin (file-name-concat ,dir ".venv" _bin))
            (,pypath (expand-file-name (concat ,pyname (car exec-suffixes))
                                        _vbin)))
       (mkdir _vbin 'recursive)
       (let ((default-directory _vbin)
             (tmpfile (make-temp-file "testing-")))
         (copy-file tmpfile (file-name-nondirectory ,pypath))
         (set-file-modes ,pypath #o755))
       ,@body)))

(ert-deftest python-shell-interpreter-venv-first ()
  "Test the python-shell-interpreter should check .venv first."
  (pytest-in-temp-directory "ipython" dir pypath
    (with-current-buffer (find-file-noselect (expand-file-name "t.py" dir))
      (should (string-match-p "ipython" python-shell-interpreter)))))

(ert-deftest python-shell-interpreter-custom-value ()
  "Test the python-shell-interpreter should follow custom value"
  (pytest-in-temp-directory "python" dir pypath
    ;; for user manual value
    (let ((python-shell-interpreter "xpython"))
      (with-current-buffer (find-file-noselect (expand-file-name "t0.py" dir))
        (should (string-match-p "xpython" python-shell-interpreter))))
    ;; from the .dir-locals
    (let ((enable-local-variables :all))
      (with-temp-file (expand-file-name ".dir-locals.el" dir)
        (insert "((python-mode . ((python-shell-interpreter . \"xpython\"))))"))
      (with-current-buffer (find-file-noselect (expand-file-name "t1.py" dir))
        (should (string-match-p "xpython" python-shell-interpreter))))))

(ert-deftest python-shell-interpreter-breadth-first ()
  "Test python layer should search interpreter in Breadth-First"
  (pytest-in-temp-directory "python" dir pypath
    (let* ((pdir (file-name-concat dir "pdir"))
           (ipy (string-replace "python" "ipython"
                                (file-name-nondirectory pypath)))
           (ipython0 (file-name-concat (file-name-directory pypath) ipy))
           (ipython1 (file-name-concat pdir ipy)))
      ;; Should "ipython" first if both "python" & "ipython" in same dir
      (message "ipython0 %s ipython1 %s" ipython0 ipython1)
      (copy-file pypath ipython0)  ; now the DIR has both "python" and "ipython"
      (with-current-buffer (find-file-noselect (expand-file-name "t0.py" dir))
        (should (string-match-p "ipython" python-shell-interpreter)))
      (delete-file ipython0)
      (mkdir pdir)
      (copy-file pypath ipython1)
      (add-to-list 'exec-path pdir)
      (with-current-buffer (find-file-noselect (expand-file-name "t1.py" dir))
        ;; .venv/ on top of exec-path with "python", pdir with "ipython" on 2nd
        (should (string= "python" (file-name-base python-shell-interpreter)))))))
