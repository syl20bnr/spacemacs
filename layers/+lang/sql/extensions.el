;;; extensions.el --- sql Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar sql-pre-extensions
  '(
    ;; pre extension sqls go here
    )
  "List of all extensions to load before the packages.")

(defvar sql-post-extensions
  '(
    ;; post extension sqls go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function sql/init-<extension-sql>
;;
;; (defun sql/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
