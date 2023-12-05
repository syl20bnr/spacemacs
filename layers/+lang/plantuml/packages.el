;;; packages.el --- plantuml layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Robert O'Connor <robby.oconnor@gmail.com>
;; Contributor: Carlo Sciolla <carlo.sciolla@gmail.com>
;; URL: https://github.com/robbyoconnor
;;
;;; Commentary:
;;
;; Adds PlantUML support to Spacemacs using plantuml-mode.
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


;;; Code:
(defconst plantuml-packages
  '(
    org
    plantuml-mode
    ))

(defun plantuml/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(plantuml . t))))

(defun plantuml/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :mode ("\\.\\(pum\\|puml\\)\\'" . plantuml-mode)
    :config
    ;; Our default is jar execution, not server as server is not working reliable see #13574
    (setq plantuml-default-exec-mode 'jar)
    ;; for now plantuml electric indentation is buggy and does not
    ;; really work, let's disable auto-indentation on paste for
    ;; this mode
    (add-to-list 'spacemacs-indent-sensitive-modes 'plantuml-mode)
    (spacemacs/declare-prefix-for-mode 'plantuml-mode
      "mc" "compile")
    (spacemacs/set-leader-keys-for-major-mode 'plantuml-mode
      "cc" 'plantuml-preview
      "co" 'plantuml-set-output-type)))
