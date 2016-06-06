;;; packages.el --- plantuml layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Robert O'Connor <robby.oconnor@gmail.com>
;; URL: https://github.com/robbyoconnor
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst plantuml-packages
  '(org
    puml-mode))

(defun plantuml/post-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(plantuml . t))))

(defun plantuml/init-puml-mode ()
  (use-package puml-mode
    :defer t
    :mode ("\\.pum\\'" . puml-mode)
    :config (spacemacs/set-leader-keys-for-major-mode 'puml-mode
              "cc" 'puml-preview
              "co" 'puml-set-output-type)))

