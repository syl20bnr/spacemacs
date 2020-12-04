;;; packages.el --- cfengine layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Nick Anderson <nick@cmdln.org>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst cfengine-packages
  '(
    (cfengine3-mode :location built-in)
    company
    eldoc
    flycheck
    (ob-cfengine3 :requires org)
    org
    mustache-mode
    ))

(defun cfengine/init-cfengine3-mode ()
  (use-package cfengine3-mode
    :defer t
    :mode ("\\.cf\\'" . cfengine3-mode)
    :init (space-macs/set-leader-keys-for-major-mode 'cfengine3-mode
            "j" 'cfengine3-reformat-json-string)))

(defun cfengine/post-init-company ()
  (space-macs|add-company-backends :modes cfengine3-mode))

(defun cfengine/post-init-eldoc ()
  (add-hook 'cfengine3-mode-hook 'eldoc-mode))

(defun cfengine/post-init-flycheck ()
  (space-macs/enable-flycheck 'cfengine3-mode))

(defun cfengine/init-ob-cfengine3 ()
  (use-package ob-cfengine3
    :defer t))

(defun cfengine/pre-init-org ()
  (when (configuration-layer/layer-used-p 'org)
    (space-macs|use-package-add-hook org
      :post-config (add-to-list 'org-babel-load-languages '(cfengine3 . t)))))

(defun cfengine/init-mustache-mode ()
  (use-package mustache-mode
    :init (add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode))
    :defer t))


