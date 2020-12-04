;;; packages.el --- Groovy Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst groovy-packages
  '(
    company
    groovy-imports
    groovy-mode
    org))

(defun groovy/post-init-company ()
  (space-macs//groovy-setup-company))

(defun groovy/post-init-flycheck ()
  (space-macs/enable-flycheck 'groovy-mode))

(defun groovy/init-groovy-imports ()
  (use-package groovy-imports
    :defer t
    :init
    (progn
      (add-hook 'groovy-mode-hook 'groovy-imports-scan-file)
      (space-macs/set-leader-keys-for-major-mode 'groovy-mode
        "ri" 'groovy-imports-add-import-dwim))))

(defun groovy/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    :init
    (progn
      (setq lsp-groovy-server-file groovy-lsp-jar-path)
      (space-macs/declare-prefix-for-mode 'groovy-mode "ms" "REPL")
      (space-macs/set-leader-keys-for-major-mode 'groovy-mode
        "'"  'run-groovy
        "sB" 'space-macs/groovy-load-file-switch
        "sb" 'space-macs/groovy-load-file
        "sF" 'space-macs/groovy-send-definition-switch
        "sf" 'groovy-send-definition
        "si" 'run-groovy
        "sR" 'space-macs/groovy-send-region-switch
        "sr" 'groovy-send-region)
      (add-hook 'groovy-mode-hook #'space-macs//groovy-setup-backend))))

(defun groovy/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(groovy . t))))


