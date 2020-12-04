;;; packages.el --- sql Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq sql-packages
      '(
        company
        org
        sql
        ;; This mode is more up-to-date than the MELPA one.
        ;; Turns out that it is available in GNU ELPA but we cannot
        ;; force Space-macs to fetch from it for now, it will always
        ;; pickup the MELPA version. So for now we use an explicit
        ;; recip to fetch from GitHUb the package.
        (sql-indent :location (recipe
                               :fetcher github
                               :repo "alex-hhh/e-macs-sql-indent"
                               :files ("sql-indent.el")))
        (sqlfmt :location local)
        (sqlup-mode :toggle sql-capitalize-keywords)
        ))

(defun sql/init-sql ()
  (use-package sql
    :defer t
    :init
    (progn
      (space-macs/register-repl 'sql 'space-macs/sql-start "sql")
      (add-hook 'sql-mode-hook
                'space-macs//sql-setup-backend))
    :config
    (progn
      (setq
       ;; should not set this to anything else than nil
       ;; the focus of SQLi is handled by space-macs conventions
       sql-pop-to-buffer-after-send-region nil)
      (advice-add 'sql-add-product :after #'space-macs/sql-populate-products-list)
      (advice-add 'sql-del-product :after #'space-macs/sql-populate-products-list)
      (space-macs/sql-populate-products-list)
      (defun space-macs//sql-source (products)
        "return a source for helm selection"
        `((name . "SQL Products")
          (candidates . ,(mapcar (lambda (product)
                                   (cons (sql-get-product-feature (car product) :name)
                                         (car product)))
                                 products))
          (action . (lambda (candidate) (helm-marked-candidates)))))

      (defun space-macs/sql-highlight ()
        "set SQL dialect-specific highlighting"
        (interactive)
        (let ((product (car (helm
                             :sources (list (space-macs//sql-source space-macs-sql-highlightable))))))
          (sql-set-product product)))

      (defun space-macs/sql-start ()
        "set SQL dialect-specific highlighting and start inferior SQLi process"
        (interactive)
        (let ((product (car (helm
                             :sources (list (space-macs//sql-source space-macs-sql-startable))))))
          (sql-set-product product)
          (sql-product-interactive product)))

      (defun space-macs/sql-send-string-and-focus ()
        "Send a string to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (call-interactively 'sql-send-string)
          (evil-insert-state)))

      (defun space-macs/sql-send-buffer-and-focus ()
        "Send the buffer to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-buffer)
          (evil-insert-state)))

      (defun space-macs/sql-send-paragraph-and-focus ()
        "Send the paragraph to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-paragraph)
          (evil-insert-state)))

      (defun space-macs/sql-send-region-and-focus (start end)
        "Send region to SQLi and switch to SQLi in `insert state'."
        (interactive "r")
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-region start end)
          (evil-insert-state)))

      (defun space-macs/sql-send-line-and-next-and-focus ()
        "Send the current line to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-line-and-next)))

      (defun space-macs/sql-send-string ()
        "Send a string to SQLi and stays in the same region."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region nil))
          (call-interactively 'sql-send-string)))

      (defun space-macs/sql-send-buffer ()
        "Send the buffer to SQLi and stays in the same region."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region nil))
          (sql-send-buffer)))

      (defun space-macs/sql-send-paragraph ()
        "Send the paragraph to SQLi and stays in the same region."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region nil))
          (sql-send-paragraph)))

      (defun space-macs/sql-send-region (start end)
        "Send region to SQLi and stays in the same region."
        (interactive "r")
        (let ((sql-pop-to-buffer-after-send-region nil))
          (sql-send-region start end)))

      (defun space-macs/sql-send-line-and-next ()
        "Send the current line to SQLi and stays in the same region."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region nil))
          (sql-send-line-and-next)))

      (space-macs/declare-prefix-for-mode 'sql-mode "mb" "buffer")
      (space-macs/declare-prefix-for-mode 'sql-mode "mg" "goto")
      (space-macs/declare-prefix-for-mode 'sql-mode "mh" "dialects")
      (space-macs/declare-prefix-for-mode 'sql-mode "ml" "listing")
      (space-macs/declare-prefix-for-mode 'sql-mode "ms" "REPL")
      (space-macs/set-leader-keys-for-major-mode 'sql-mode
        "'" 'space-macs/sql-start

        ;; sqli buffer
        "bb" 'sql-show-sqli-buffer
        "bc" 'sql-connect
        "bs" 'sql-set-sqli-buffer

        ;; dialects
        "hk" 'space-macs/sql-highlight

        ;; repl
        "sb" 'sql-send-buffer
        "sB" 'space-macs/sql-send-buffer-and-focus
        "si" 'space-macs/sql-start
        ;; paragraph gets "f" here because they can be assimilated to functions.
        ;; If you separate your commands in a SQL file, this key will send the
        ;; command around point, which is what you probably want.
        "sf" 'space-macs/sql-send-paragraph
        "sF" 'space-macs/sql-send-paragraph-and-focus
        "sl" 'space-macs/sql-send-line-and-next
        "sL" 'space-macs/sql-send-line-and-next-and-focus
        "sq" 'space-macs/sql-send-string
        "sQ" 'space-macs/sql-send-string-and-focus
        "sr" 'space-macs/sql-send-region
        "sR" 'space-macs/sql-send-region-and-focus

        ;; listing
        "la" 'sql-list-all
        "lt" 'sql-list-table)

      (space-macs/declare-prefix-for-mode 'sql-interactive-mode "mb" "buffer")
      (space-macs/set-leader-keys-for-major-mode 'sql-interactive-mode
        ;; sqli buffer
        "br" 'sql-rename-buffer
        "bS" 'sql-save-connection)

      (add-hook 'sql-interactive-mode-hook
                (lambda () (toggle-truncate-lines t)))

      ;; lsp-sqls
      (let ((path-config (cond
                          ((equal sql-lsp-sqls-workspace-config-path 'workspace) "workspace")
                          ((equal sql-lsp-sqls-workspace-config-path 'root) "root")
                          (t nil))))
        (setq lsp-sqls-workspace-config-path path-config)))))

(defun sql/init-sql-indent ()
  (use-package sql-indent
    :if sql-auto-indent
    :defer t
    :init (add-hook 'sql-mode-hook 'sqlind-minor-mode)
    :config (space-macs|hide-lighter sqlind-minor-mode)))

(defun sql/init-sqlfmt ()
  (use-package sqlfmt
    :commands sqlfmt-buffer
    :init
    (space-macs/declare-prefix-for-mode 'sql-mode "m=" "formatting")
    (space-macs/set-leader-keys-for-major-mode 'sql-mode
      "=r" 'sqlfmt-region
      "==" 'sqlfmt-buffer)))

(defun sql/init-sqlup-mode ()
  (use-package sqlup-mode
    :defer t
    :init
    (progn
      (add-hook 'sql-mode-hook 'sqlup-mode)
      (unless sql-capitalize-keywords-disable-interactive
        (add-hook 'sql-interactive-mode-hook 'sqlup-mode))
      (space-macs/set-leader-keys-for-major-mode 'sql-mode
        "c" 'sqlup-capitalize-keywords-in-region))
    :config
    (progn
      (space-macs|hide-lighter sqlup-mode)
      (setq sqlup-blacklist (append sqlup-blacklist
                                    sql-capitalize-keywords-blacklist)))))

(defun sql/post-init-company ()
  (space-macs//sql-setup-company))

(defun sql/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sql . t))))


