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

;;; Commentary:

;;; Code:

(defun plantuml/init-puml-mode ()
  (use-package puml-mode
    :defer t
    :mode ("\\.pum$" . puml-mode)

    :config (spacemacs/set-leader-keys-for-major-mode 'puml-mode
              "cc" 'puml-preview
              "co" 'puml-set-output-type)))

(defun plantuml/post-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (when (boundp 'org-plantuml-jar-path)
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((plantuml . t))))))

;;; packages.el ends here
