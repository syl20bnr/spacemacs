;;; packages.el --- Semantic Web layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Andreas Textor <mail@atextor.de>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
      (space-macs/set-leader-keys-for-major-mode 'sparql-mode "q" 'sparql-query-region)
      (when (configuration-layer/package-used-p 'company)
        (space-macs|add-company-backends
          :backends company-sparql
          :modes sparql-mode)))))

(defun semantic-web/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sparql . t))))


