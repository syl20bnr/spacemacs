;;; funcs.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Brian Hicks & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defmacro sql-client (name)
  (let
      ((funcname (intern (concat "sql/" name "-client")))
       (highlight (intern (concat "sql-highlight-" name "-keywords")))
       (client (intern (concat "sql-" name))))
    `(defun ,funcname ()
       (,highlight)
       (interactive)
       (,client))))

(sql-client "db2")
(sql-client "mysql")
(sql-client "postgres")
(sql-client "sqlite")
(sql-client "ms")
(sql-client "informix")
(sql-client "ingrex")
(sql-client "interbase")
(sql-client "linter")
(sql-client "oracle")
(sql-client "solid")
(sql-client "sybase")
(sql-client "vertica")
