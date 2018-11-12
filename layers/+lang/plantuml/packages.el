;;; packages.el --- plantuml layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
;;; License: GPLv3


;;; Code:
(defconst plantuml-packages
  '(org
    plantuml-mode))

(defun plantuml/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(plantuml . t))))

(defun plantuml/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :mode ("\\.\\(pum\\|puml\\)\\'" . plantuml-mode)
    :config
    (progn
      (when (boundp 'spacemacs-indent-sensitive-modes)
        ;; for now plantuml electric indentation is buggy and does not
        ;; really work, let's disable auto-indentation on paste for
        ;; this mode
        (add-to-list 'spacemacs-indent-sensitive-modes 'plantuml-mode))
      (spacemacs/set-leader-keys-for-major-mode 'plantuml-mode
        "cc" 'plantuml-preview
        "co" 'plantuml-set-output-type))))
