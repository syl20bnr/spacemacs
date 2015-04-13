;;; packages.el --- sql Layer packages File for Spacemacs
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

(defvar sql-packages '(sql sql-indent))

(defvar sql-excluded-packages '())

(defun sql/init-sql ()
  (use-package sql
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'sql-mode
        ;; sqli buffer
        "mbb" 'sql-show-sqli-buffer
        "mbs" 'sql-set-sqli-buffer
        "mbp" 'sql-toggle-pop-to-buffer-after-send-region

        ;; clients
        "mcb" 'sql/sybase-client
        "mcd" 'sql/db2-client
        "mci" 'sql/informix-client
        "mcl" 'sql/linter-client
        "mcm" 'sql/mysql-client
        "mcn" 'sql/ingrex-client
        "mco" 'sql/oracle-client
        "mcp" 'sql/postgres-client
        "mcq" 'sql/sqlite-client
        "mcs" 'sql/ms-client
        "mcS" 'sql/solid-client
        "mct" 'sql/interbase-client
        "mcv" 'sql/vertica-client

        ;; dialects
        "mda" 'sql-highlight-ansi-keywords
        "mdb" 'sql-highlight-sybase-keywords
        "mdd" 'sql-highlight-db2-keywords
        "mdi" 'sql-highlight-informix-keywords
        "mdl" 'sql-highlight-linter-keywords
        "mdm" 'sql-highlight-mysql-keywords
        "mdn" 'sql-highlight-ingrex-keywords
        "mdo" 'sql-highlight-oracle-keywords
        "mdp" 'sql-highlight-postgres-keywords
        "mdq" 'sql-highlight-sqlite-keywords
        "mds" 'sql-highlight-ms-keywords
        "mdS" 'sql-highlight-solid-keywords
        "mdt" 'sql-highlight-interbase-keywords
        "mdv" 'sql-highlight-vertica-keywords

        ;; interactivity
        "msb" 'sql-send-buffer
        "msr" 'sql-send-region
        ; paragraph gets "s" here because it's probably the most handy. If you
        ; separate your commands in a SQL file, double-tapping the key will
        ; send the command under the point, which is what you probably want.
        "mss" 'sql-send-paragraph
        "mst" 'sql-send-string

        ;; listing
        "mla" 'sql-list-all
        "mlt" 'sql-list-table)

      (evil-leader/set-key-for-mode 'sql-interactive-mode
        ;; sqli buffer
        "mbr" 'sql-rename-buffer
        "mbS" 'sql-save-connection)

      (add-hook 'sql-interactive-mode-hook
                (lambda () (toggle-truncate-lines t))))))

(defun sql/init-sql-indent ()
  (use-package sql-indent
    :defer t))
