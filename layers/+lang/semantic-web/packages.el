;;; packages.el --- semweb layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Andreas Textor <mail@atextor.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq semantic-web-packages
  '(
    ttl-mode
    sparql-mode
    org
    ))

(defun semantic-web/init-ttl-mode ()
  (use-package ttl-mode
    :mode ("\\.\\(ttl\\|n3\\)\\'" . ttl-mode)))

(defun semantic-web/init-sparql-mode ()
  (use-package sparql-mode
    :mode ("\\.\\(sparql\\|rq\\)\\'" . sparql-mode)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'sparql-mode "q" 'sparql-query-region)
      (when (configuration-layer/package-used-p 'company)
        (spacemacs|add-company-backends
          :backends company-sparql
          :modes sparql-mode)))))

(defun semantic-web/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sparql . t))))
