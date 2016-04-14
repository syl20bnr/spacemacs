;;; packages.el --- cfengine layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Nick Anderson <nick@cmdln.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `cfengine-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cfengine/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cfengine/pre-init-PACKAGE' and/or
;;   `cfengine/post-init-PACKAGE' to customize the package as it is loaded.

(setq cfengine-packages
      '(
	flycheck
	company))
	;; cfengine3-mode is included in emacs itself.

(defun cfengine/init-cfengine()
  (use-package cfengine3-mode
    :init
      (progn
        (spacemacs/set-leader-keys-for-major-mode 'cfengine3-mode
          "j" 'cfengine3-reformat-json-string))))

(defun cfengine/post-init-company ()
  (spacemacs|add-company-hook cfengine3-mode))

(defun cfengine/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'cfengine3-mode-hook))
