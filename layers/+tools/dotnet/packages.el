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


(setq dotnet-packages
      '(
        dotnet
        ))

(defun dotnet/init-dotnet ()
  (use-package dotnet
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'fsharp-mode
      "dap" 'dotnet-add-package
      "dar" 'dotnet-add-reference
      "db"  'dotnet-build
      "dc"  'dotnet-clean
      "dn"  'dotnet-new
      "dp"  'dotnet-publish
      "dra" 'dotnet-run-with-args
      "drr" 'dotnet-run
      "drs" 'dotnet-restore
      "dsa" 'dotnet-sln-add
      "dsl" 'dotnet-sln-list
      "dsn" 'dotnet-sln-new
      "dsr" 'dotnet-sln-remove
      "dt"  'dotnet-test)))
