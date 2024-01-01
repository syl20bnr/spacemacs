;;; packages.el --- Semantic Web layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Andreas Textor <mail@atextor.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst semantic-web-packages
  '((ttl-mode :location (recipe
                         :fetcher github
                         :repo "jeeger/ttl-mode"))
    sparql-mode
    org))

(defun semantic-web/init-ttl-mode ()
  (use-package ttl-mode
    :mode ("\\.\\(ttl\\|n3\\)\\'" . ttl-mode)))

(defun semantic-web/init-sparql-mode ()
  (use-package sparql-mode
    :mode ("\\.\\(sparql\\|rq\\)\\'" . sparql-mode)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'sparql-mode "q" 'sparql-query-region)
    (when (configuration-layer/package-used-p 'company)
      (spacemacs|add-company-backends
       :backends company-sparql
       :modes sparql-mode))))

(defun semantic-web/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sparql . t))))
