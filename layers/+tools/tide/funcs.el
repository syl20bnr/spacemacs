;;; funcs.el --- Tide  Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
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


(defun spacemacs//tide-setup-bindings ()
  "Define keys bindings for `tide-mode'"
  (spacemacs/set-leader-keys-for-minor-mode 'tide-mode
    "E" "errors"
    "Ee" #'tide-fix
    "Ed" #'tide-add-tslint-disable-next-line
    "Ep" #'tide-project-errors
    "g" "goto"
    "ge" #'tide-project-errors
    "gb" #'tide-jump-back
    "gg" #'tide-jump-to-definition
    "gt" #'spacemacs/typescript-jump-to-type-def
    "gr" #'tide-references
    "h" "help"
    "hh" #'tide-documentation-at-point
    "r" "refactor"
    "ri" #'tide-organize-imports
    "rr" #'tide-rename-symbol
    "rf" #'tide-rename-file
    "rR" #'tide-refactor
    "S" "server"
    "Sr" #'tide-restart-server
    "Sj" #'spacemacs//tide-create-jsconfig-file))

(defun spacemacs//tide-setup ()
  "Setup tide backend.
Must be called by a layer using tide."
  (evilified-state-evilify-map tide-references-mode-map
    :mode tide-references-mode
    :bindings
    (kbd "C-k") 'tide-find-previous-reference
    (kbd "C-j") 'tide-find-next-reference
    (kbd "C-l") 'tide-goto-reference)
  (tide-hl-identifier-mode +1)
  (tide-setup))

(defun spacemacs//tide--list-to-string (list)
  "Convert LIST to string."
  (cl-reduce (lambda (x y) (concat x " " (symbol-name y)))
             (cdr list)
             :initial-value (format "%s" (car list) )))

(defun spacemacs//tide-setup-company (&rest modes)
  "Setup tide company for MODES.
Must be called by a layer using tide."
  (eval `(spacemacs|add-company-backends
           :backends company-tide
           :modes ,@modes
           :append-hooks nil
           :call-hooks t))
  (company-mode))

(defun spacemacs//tide-setup-eldoc ()
  "Setup eldoc for tide."
  (eldoc-mode))

(defun spacemacs//tide-setup-jump-handle ()
  "Set jump handlers."
  (add-to-list 'spacemacs-jump-handlers '(tide-jump-to-definition :async t)))

(defun spacemacs//tide-create-jsconfig-file ()
  "Create a jsconfig file at project root."
  (interactive)
  (let ((jsconfig (cdr (project-current))))
    (if jsconfig
        (let ((jsconfig-file (concat jsconfig "jsconfig.json")))
          (if (file-exists-p jsconfig-file)
              (message "File exists")
            (with-temp-file jsconfig-file
              (insert tide-jsconfig-content))))
      (message "Project not found"))))
