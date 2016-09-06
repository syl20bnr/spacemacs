;;; packages.el --- BibTeX Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Joshua Ellis <josh@jpellis.me>
;; URL: https://github.com/JP-Ellis
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/org-ref-insert-cite-link ()
  (interactive)
  ;; Workaround for #7040
  (require 'org-ref-core)
  (call-interactively 'org-ref-helm-insert-cite-link))
