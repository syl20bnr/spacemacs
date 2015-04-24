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
      ;; should not set this to anything else than nil
      ;; the focus of SQLi is handled by spacemacs conventions
      (setq sql-pop-to-buffer-after-send-region nil)

      (defmacro sql-client (name)
        "Define a function to set syntax highlighting and start a SQLi buffer."
        (let
            ((funcname (intern (concat "sql/" name "-client")))
             (highlight (intern (concat "sql-highlight-" name "-keywords")))
             (client (intern (concat "sql-" name))))
          `(defun ,funcname ()
             (interactive)
             (,highlight)
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

      (defun spacemacs/sql-send-string-and-focus ()
        "Send a string to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (call-interactively 'sql-send-string)
          (evil-insert-state)))

      (defun spacemacs/sql-send-buffer-and-focus ()
        "Send the buffer to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-buffer)
          (evil-insert-state)))

      (defun spacemacs/sql-send-paragraph-and-focus ()
        "Send the paragraph to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-paragraph)
          (evil-insert-state)))

      (defun spacemacs/sql-send-region-and-focus (start end)
        "Send region to SQLi and switch to SQLi in `insert state'."
        (interactive "r")
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-region start end)
          (evil-insert-state)))

      (evil-leader/set-key-for-mode 'sql-mode
        ;; sqli buffer
        "mbb" 'sql-show-sqli-buffer
        "mbs" 'sql-set-sqli-buffer

        ;; clients
        ;; TODO: change the key binding to `msi' and use a helm buffer
        ;; to choose the client type
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
        ;; TODO: use `mhk' and use a helm buffer to choose the dialect
        "mhka" 'sql-highlight-ansi-keywords
        "mhkb" 'sql-highlight-sybase-keywords
        "mhkd" 'sql-highlight-db2-keywords
        "mhki" 'sql-highlight-informix-keywords
        "mhkl" 'sql-highlight-linter-keywords
        "mhkm" 'sql-highlight-mysql-keywords
        "mhkn" 'sql-highlight-ingrex-keywords
        "mhko" 'sql-highlight-oracle-keywords
        "mhkp" 'sql-highlight-postgres-keywords
        "mhkq" 'sql-highlight-sqlite-keywords
        "mhks" 'sql-highlight-ms-keywords
        "mhkS" 'sql-highlight-solid-keywords
        "mhkt" 'sql-highlight-interbase-keywords
        "mhkv" 'sql-highlight-vertica-keywords

        ;; interactivity
        "msb" 'sql-send-buffer
        "msB" 'spacemacs/sql-send-buffer-and-focus
        ;; paragraph gets "f" here because they can be assimilated to functions.
        ;; If you separate your commands in a SQL file, double-tapping the key
        ;; will send the command under the point, which is what you probably want.
        "msf" 'sql-send-paragraph
        "msF" 'spacemacs/sql-send-paragraph-and-focus
        "msq" 'sql-send-string
        "msQ" 'spacemacs/sql-send-string-and-focus
        "msr" 'sql-send-region
        "msR" 'spacemacs/sql-send-region-and-focus

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
