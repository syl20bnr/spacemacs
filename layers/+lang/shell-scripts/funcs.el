;;; funcs.el --- Shell Scripts Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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



;; lsp
(defun spacemacs//shell-scripts-setup-backend ()
  "Conditionally setup shell-scripts backend."
  (when (eq shell-scripts-backend 'lsp)
    (lsp-deferred)))

(defun spacemacs//shell-scripts-setup-company ()
  "Conditionally setup company based on backend."
  ;; Activate lsp company explicitly to activate
  ;; standard backends as well
  (if (eq shell-scripts-backend 'lsp)
      (spacemacs|add-company-backends
        :backends company-capf
        :modes sh-mode)
    (spacemacs|add-company-backends
      :backends (company-shell company-shell-env)
      :modes sh-mode)))


;; shebang

(defun spacemacs/insert-shebang ()
  "Insert shebang line at the top of the file."
  (interactive)
  (require 'insert-shebang)
  (insert-shebang-get-extension-and-insert
   (file-name-nondirectory (buffer-file-name))))
