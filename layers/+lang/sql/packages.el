;;; packages.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
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


(setq sql-packages
      '(
        company
        org
        sql
        ;; This mode is more up-to-date than the MELPA one.
        ;; Turns out that it is available in GNU ELPA but we cannot
        ;; force Spacemacs to fetch from it for now, it will always
        ;; pickup the MELPA version. So for now we use an explicit
        ;; recip to fetch from GitHUb the package.
        (sql-indent :location (recipe
                               :fetcher github
                               :repo "alex-hhh/emacs-sql-indent"
                               :files ("sql-indent.el")))
        (sqlfmt :location local)
        (sqlup-mode :toggle sql-capitalize-keywords)
        ))

(defun sql/init-sql ()
  (use-package sql
    :defer t
    :init
    (spacemacs/register-repl 'sql 'spacemacs/sql-start "sql")
    (add-hook 'sql-mode-hook
              'spacemacs//sql-setup-backend)
    :config
    (setq
     ;; should not set this to anything else than nil
     ;; the focus of SQLi is handled by spacemacs conventions
     sql-pop-to-buffer-after-send-region nil)
    (advice-add 'sql-add-product :after #'spacemacs/sql-populate-products-list)
    (advice-add 'sql-del-product :after #'spacemacs/sql-populate-products-list)
    (spacemacs/sql-populate-products-list)
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

    (defun spacemacs/sql-send-line-and-next-and-focus ()
      "Send the current line to SQLi and switch to SQLi in `insert state'."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region t))
        (sql-send-line-and-next)))

    (defun spacemacs/sql-send-string ()
      "Send a string to SQLi and stays in the same region."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region nil))
        (call-interactively 'sql-send-string)))

    (defun spacemacs/sql-send-buffer ()
      "Send the buffer to SQLi and stays in the same region."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region nil))
        (sql-send-buffer)))

    (defun spacemacs/sql-send-paragraph ()
      "Send the paragraph to SQLi and stays in the same region."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region nil))
        (sql-send-paragraph)))

    (defun spacemacs/sql-send-region (start end)
      "Send region to SQLi and stays in the same region."
      (interactive "r")
      (let ((sql-pop-to-buffer-after-send-region nil))
        (sql-send-region start end)))

    (defun spacemacs/sql-send-line-and-next ()
      "Send the current line to SQLi and stays in the same region."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region nil))
        (sql-send-line-and-next)))

    (spacemacs/declare-prefix-for-mode 'sql-mode "mb" "buffer")
    (spacemacs/declare-prefix-for-mode 'sql-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'sql-mode "mh" "dialects")
    (spacemacs/declare-prefix-for-mode 'sql-mode "ml" "listing")
    (spacemacs/declare-prefix-for-mode 'sql-mode "ms" "REPL")
    (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      "'" 'spacemacs/sql-start

      ;; sqli buffer
      "bb" 'sql-show-sqli-buffer
      "bc" 'sql-connect
      "bs" 'sql-set-sqli-buffer

      ;; dialects
      "hk" 'spacemacs/sql-highlight

      ;; repl
      "sb" 'sql-send-buffer
      "sB" 'spacemacs/sql-send-buffer-and-focus
      "si" 'spacemacs/sql-start
      ;; paragraph gets "f" here because they can be assimilated to functions.
      ;; If you separate your commands in a SQL file, this key will send the
      ;; command around point, which is what you probably want.
      "sf" 'spacemacs/sql-send-paragraph
      "sF" 'spacemacs/sql-send-paragraph-and-focus
      "sl" 'spacemacs/sql-send-line-and-next
      "sL" 'spacemacs/sql-send-line-and-next-and-focus
      "sq" 'spacemacs/sql-send-string
      "sQ" 'spacemacs/sql-send-string-and-focus
      "sr" 'spacemacs/sql-send-region
      "sR" 'spacemacs/sql-send-region-and-focus

      ;; listing
      "la" 'sql-list-all
      "lt" 'sql-list-table)

    (spacemacs/declare-prefix-for-mode 'sql-interactive-mode "mb" "buffer")
    (spacemacs/set-leader-keys-for-major-mode 'sql-interactive-mode
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
      (setq lsp-sqls-workspace-config-path path-config))))

(defun sql/init-sql-indent ()
  (use-package sql-indent
    :if sql-auto-indent
    :defer t
    :init (add-hook 'sql-mode-hook 'sqlind-minor-mode)
    :config (spacemacs|hide-lighter sqlind-minor-mode)))

(defun sql/init-sqlfmt ()
  (use-package sqlfmt
    :commands sqlfmt-buffer
    :init
    (spacemacs/declare-prefix-for-mode 'sql-mode "m=" "formatting")
    (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      "=r" 'sqlfmt-region
      "==" 'sqlfmt-buffer)))

(defun sql/init-sqlup-mode ()
  (use-package sqlup-mode
    :defer t
    :init
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (unless sql-capitalize-keywords-disable-interactive
      (add-hook 'sql-interactive-mode-hook 'sqlup-mode))
    (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      "c" 'sqlup-capitalize-keywords-in-region)
    :config
    (spacemacs|hide-lighter sqlup-mode)
    (setq sqlup-blacklist (append sqlup-blacklist
                                  sql-capitalize-keywords-blacklist))))

(defun sql/post-init-company ()
  (spacemacs//sql-setup-company))

(defun sql/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sql . t))))
