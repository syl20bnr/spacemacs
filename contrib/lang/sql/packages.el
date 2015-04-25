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

      (evil-leader/set-key-for-mode 'sql-mode
        ;; sqli buffer
        "mbb" 'sql-show-sqli-buffer
        "mbs" 'sql-set-sqli-buffer

        ;; dialects
        "mhk" 'spacemacs/sql-highlight

        ;; interactivity
        "msb" 'sql-send-buffer
        "msB" 'spacemacs/sql-send-buffer-and-focus
        "msi" 'spacemacs/sql-start
        ;; paragraph gets "f" here because they can be assimilated to functions.
        ;; If you separate your commands in a SQL file, this key will send the
        ;; command under the point, which is what you probably want.
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
