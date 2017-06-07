;;; packages.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq sql-packages
      '(
        company
        sql
        sql-indent
        (sqlup-mode :toggle sql-capitalize-keywords)
        ))

(defun sql/init-sql ()
  (use-package sql
    :defer t
    :init (spacemacs/register-repl 'sql 'spacemacs/sql-start "sql")
    :config
    (progn
      (setq spacemacs-sql-highlightable sql-product-alist
            spacemacs-sql-startable (remove-if-not
                                (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                                sql-product-alist)

            ;; should not set this to anything else than nil
            ;; the focus of SQLi is handled by spacemacs conventions
            sql-pop-to-buffer-after-send-region nil)

      (defun spacemacs//sql-source (products)
        "return a source for helm selection"
        `((name . "SQL Products")
          (candidates . ,(mapcar (lambda (product)
                                   (cons (sql-get-product-feature (car product) :name)
                                         (car product)))
                                 products))
          (action . (lambda (candidate) (helm-marked-candidates)))))

      (defun spacemacs/sql-highlight ()
        "set SQL dialect-specific highlighting"
        (interactive)
        (let ((product (car (helm
                             :sources (list (spacemacs//sql-source spacemacs-sql-highlightable))))))
          (sql-set-product product)))

      (defun spacemacs/sql-start ()
        "set SQL dialect-specific highlighting and start inferior SQLi process"
        (interactive)
        (let ((product (car (helm
                             :sources (list (spacemacs//sql-source spacemacs-sql-startable))))))
          (sql-set-product product)
          (sql-product-interactive product)))

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

      (spacemacs/set-leader-keys-for-major-mode 'sql-mode
        "'" 'spacemacs/sql-start

        ;; sqli buffer
        "bb" 'sql-show-sqli-buffer
        "bs" 'sql-set-sqli-buffer

        ;; dialects
        "hk" 'spacemacs/sql-highlight

        ;; interactivity
        "sb" 'sql-send-buffer
        "sB" 'spacemacs/sql-send-buffer-and-focus
        "si" 'spacemacs/sql-start
        ;; paragraph gets "f" here because they can be assimilated to functions.
        ;; If you separate your commands in a SQL file, this key will send the
        ;; command around point, which is what you probably want.
        "sf" 'sql-send-paragraph
        "sF" 'spacemacs/sql-send-paragraph-and-focus
        "sq" 'sql-send-string
        "sQ" 'spacemacs/sql-send-string-and-focus
        "sr" 'sql-send-region
        "sR" 'spacemacs/sql-send-region-and-focus

        ;; listing
        "la" 'sql-list-all
        "lt" 'sql-list-table)

      (spacemacs/set-leader-keys-for-major-mode 'sql-interactive-mode
        ;; sqli buffer
        "br" 'sql-rename-buffer
        "bS" 'sql-save-connection)

      (add-hook 'sql-interactive-mode-hook
                (lambda () (toggle-truncate-lines t))))))

(defun sql/init-sql-indent ()
  (use-package sql-indent
    :defer t))

(defun sql/init-sqlup-mode ()
  (use-package sqlup-mode
    :defer t
    :init
    (progn
      (add-hook 'sql-mode-hook 'sqlup-mode)
      (unless sql-capitalize-keywords-disable-interactive
        (add-hook 'sql-interactive-mode-hook 'sqlup-mode))
      (spacemacs/set-leader-keys-for-major-mode 'sql-mode
        "=c" 'sqlup-capitalize-keywords-in-region))
    :config
    (progn
      (spacemacs|diminish sqlup-mode)
      (setq sqlup-blacklist (append sqlup-blacklist
                                    sql-capitalize-keywords-blacklist)))))

(defun sql/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes sql-mode))
