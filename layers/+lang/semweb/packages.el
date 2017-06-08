;;; packages.el --- semweb layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andreas Textor <mail@atextor.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq semweb-packages
  '(
    company
    ttl-mode
    sparql-mode
    ))

(defun semweb/init-ttl-mode ()
  (use-package ttl-mode
    :mode ("\\.\\(ttl\\|n3\\)\\'" . ttl-mode)))

(defun semweb/post-init-company ()
  (add-hook 'sparql-mode-hook 'company-mode))

(defun semweb/init-sparql-mode ()
  (use-package sparql-mode
    :mode ("\\.\\(sparql\\|rq\\)\\'" . sparql-mode)
    :init (spacemacs/set-leader-keys-for-major-mode 'sparql-mode "q" 'sparql-query-region)))

(defun semweb/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sparql . t))))
