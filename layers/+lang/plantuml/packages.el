;;; packages.el --- plantuml layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Robert O'Connor <robby.oconnor@gmail.com>
;; Contributor: Carlo Sciolla <carlo.sciolla@gmail.com>
;; URL: https://github.com/robbyoconnor
;;
;;; Commentary:
;;
;; Adds PlantUML support to Space-macs using plantuml-mode.
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;;; Code:
(defconst plantuml-packages
  '(
    org
    plantuml-mode
    ))

(defun plantuml/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(plantuml . t))))

(defun plantuml/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :mode ("\\.\\(pum\\|puml\\)\\'" . plantuml-mode)
    :config
    (progn
      ;; Our default is jar execution, not server as server is not working reliable see #13574
      (setq plantuml-default-exec-mode 'jar)
      (when (boundp 'space-macs-indent-sensitive-modes)
        ;; for now plantuml electric indentation is buggy and does not
        ;; really work, let's disable auto-indentation on paste for
        ;; this mode
        (add-to-list 'space-macs-indent-sensitive-modes 'plantuml-mode))
      (space-macs/declare-prefix-for-mode 'plantuml-mode
        "mc" "compile")
      (space-macs/set-leader-keys-for-major-mode 'plantuml-mode
        "cc" 'plantuml-preview
        "co" 'plantuml-set-output-type))))


