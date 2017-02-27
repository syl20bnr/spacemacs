;;; funcs.el --- Orgwiki Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Swaroop C H <swaroop@swaroopch.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Modified version of `org-wiki-asset-insert-file'
(defun spacemacs/orgwiki-insert-asset-link ()
  "Insert link [[file:<page>/<file>]] to asset file of current page at point.
Insert an asset file of current page at point providing a Helm completion.
Example: [[Linux/LinuxManual.pdf]]"
  (interactive)

  (let ((pagename (file-name-base (buffer-file-name))))
    (org-wiki--asset-helm-selection
     pagename
     (lambda (file)
       (insert (format "[[file:%s/%s]]"
                       pagename
                       file
                       )))))
  (when org-startup-with-inline-images
    (org-display-inline-images)))
