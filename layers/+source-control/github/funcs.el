;;; funcs.el --- GitHub layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; Forge

(defun spacemacs/forge-get-info-from-fetched-notification-error (err)
  "Return info for given s-exp error return by `forge-pull-notifications'.

Call this function interactively and paste the s-exp from the error returned by
the `forge-pull-notifications' function.

Example of error:

error in process filter: ghub--signal-error: peculiar error:
((path \"query\" \"_Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5MDM3NDE6NTI0NzY1\")
 (extensions (code . \"undefinedField\")
 (typeName . \"Query\")
 (fieldName . \"nil\"))
 (locations ((line . 2) (column . 1)))
 (message . \"Field 'nil' doesn't exist on type 'Query'\"))

Function adapted from issue:
https://github.com/magit/forge/issues/80#issuecomment-456103195
"
  (interactive "xs-exp: ")
  (message "%s" err)
  (let* ((query_value (third (car err)))
         (result (car (forge-sql
                       [:select [owner name]
                                :from repository
                                :where (= id $s1)]
                       (base64-encode-string
                        (mapconcat
                         #'identity
                         (butlast
                          (split-string
                           (base64-decode-string (substring query_value 1))
                           ":"))
                         ":")
                        t)))))
    (message "repository: %s/%s" (car result) (cadr result))))
