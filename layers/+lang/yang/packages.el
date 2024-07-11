;;; packages.el --- YANG Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Christian Hopps <chopps@gmail.com>
;; Originally started with checker definition from flycheck-yang project.
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


(setq yang-packages '(company
                      flycheck
                      yang-mode))

(defun yang/post-init-company ()
  (spacemacs|add-company-backends :modes yang-mode))

(defun yang/post-init-flycheck ()
  (flycheck-define-command-checker 'yang-pyang
      "A YANG syntax checker using the pyang parser."
      :command '("pyang"
                 (eval (concat "--" yang-pyang-rules))
                 (eval (or yang-pyang-extra-args nil))
                 source)
      :error-patterns '((error line-start (file-name) ":"
                               line ": " "error: " (message) line-end)
                        (warning line-start (file-name) ":"
                                 line ": " "warning: " (message) line-end))
      :modes 'yang-mode
      :error-filter (lambda (errors)
                       (-> errors
                           flycheck-dedent-error-messages
                           flycheck-sanitize-errors))
    (add-to-list 'flycheck-checkers 'yang-pyang)
    (spacemacs/enable-flycheck 'yang-mode)))

(defun yang/init-yang-mode ()
  "Initialize YANG mode"
  (use-package yang-mode))
