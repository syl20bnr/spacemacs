;;; funcs.el --- Crystal Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
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


(defun spacemacs//crystal-auto-format-setup ()
  (if crystal-enable-auto-format
      (add-hook 'before-save-hook 'crystal-tool-format nil 'local)
    (remove-hook 'before-save-hook 'crystal-tool-format 'local)))

(defun spacemacs/crystal-run-main ()
  (interactive)
  (let ((default-directory (crystal-find-project-root)))
    (shell-command
     (format "crystal run %s"
             (shell-quote-argument (buffer-file-name))))))

(defun spacemacs//crystal-setup-company ()
  "Conditionally setup company based on backend."
  (pcase crystal-backend
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    ('lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes crystal-mode))
    ('company-crystal (spacemacs|add-company-backends
                        :backends company-capf
                        :modes crystal-mode
                        :variables company-tooltip-align-annotations t))))

(defun spacemacs//crystal-setup-backend ()
  "Conditionally setup crystal backend."
  (when (eq crystal-backend 'lsp) (lsp)))
