;;; packages.el --- lsp-python layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lsp-python-packages
  '(lsp-python
    pyvenv
    ))

(defun lsp-python/init-lsp-python ()
  (use-package lsp-python
    :config
    (progn
      (add-hook 'python-mode-hook #'flycheck-mode)
      (add-hook 'python-mode-hook #'lsp-python-enable)
      (when python-enable-format-on-save
        (add-hook 'python-mode-hook 'spacemacs//add-python-format-on-save))
      ;; other python settings
      ;; (ensure-lsp-python-server)
      ;; (setenv "PATH"
      ;;         (concat
      ;;          lsp-python-server-install-directory ";"
      ;;          (getenv "PATH")
      ;;          ))
    )))

(defun lsp-python/init-pyvenv ()
  (use-package pyvenv
    ))


;;; packages.el ends here
