;;; packages.el --- Groovy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq groovy-packages
      '(
        company
        groovy-imports
        groovy-mode
        org
        ))

(defun groovy/post-init-company ()
  (spacemacs|add-company-backends :modes groovy-mode))

(defun groovy/init-groovy-imports ()
  (use-package groovy-imports
    :defer t
    :init
    (progn
      (add-hook 'groovy-mode-hook 'groovy-imports-scan-file)
      (spacemacs/set-leader-keys-for-major-mode 'groovy-mode
        "ri" 'groovy-imports-add-import-dwim))))

(defun groovy/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'groovy-mode "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'groovy-mode
        "'"  'run-groovy
        "sB" 'spacemacs/groovy-load-file-switch
        "sb" 'spacemacs/groovy-load-file
        "sF" 'spacemacs/groovy-send-definition-switch
        "sf" 'groovy-send-definition
        "si" 'run-groovy
        "sR" 'spacemacs/groovy-send-region-switch
        "sr" 'groovy-send-region))))

(defun groovy/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(groovy . t))))
