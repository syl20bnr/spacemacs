;;; packages.el --- Dotnet Layer packages File for Spacemacs
;;
;; Author: Jordan Kaye <jordan.kaye2@gmail.com>
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


(defconst dotnet-packages
  '(dotnet))

(defun dotnet/init-dotnet ()
  (use-package dotnet
    :defer t
    :init
    (dolist (mode '(fsharp-mode csharp-mode))
      (spacemacs/declare-prefix-for-mode mode "mp" "project")
      (spacemacs/declare-prefix-for-mode mode "mpa" "add")
      (spacemacs/declare-prefix-for-mode mode "mpr" "run")
      (spacemacs/declare-prefix-for-mode mode "mps" "sln")
      (spacemacs/set-leader-keys-for-major-mode mode
        "pap" 'dotnet-add-package
        "par" 'dotnet-add-reference
        "pb"  'dotnet-build
        "pc"  'dotnet-clean
        "pn"  'dotnet-new
        "pp"  'dotnet-publish
        "pra" 'dotnet-run-with-args
        "prr" 'dotnet-run
        "prs" 'dotnet-restore
        "psa" 'dotnet-sln-add
        "psl" 'dotnet-sln-list
        "psn" 'dotnet-sln-new
        "psr" 'dotnet-sln-remove
        "pt"  'dotnet-test))))
