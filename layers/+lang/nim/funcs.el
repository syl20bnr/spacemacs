;;; funcs.el --- Nim Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//nim-backend ()
  "Returns selected backend."
  (if nim-backend
      nim-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-nim))))

(defun spacemacs//nim-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//nim-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes nim-mode nimscript-mode))
    (`company-nim (spacemacs|add-company-backends
                    :backends company-nimsuggest
                    :modes nim-mode nimscript-mode))))

(defun spacemacs//nim-setup-backend ()
  "Conditionally setup nim backend."
  (pcase (spacemacs//nim-backend)
    (`lsp (lsp))
    (`company-nim (progn
                    (nimsuggest-mode)
                    (add-to-list 'spacemacs-jump-handlers-nim-mode 'nimsuggest-find-definition)))))


(defun spacemacs/nim-compile-run ()
  "Compile current buffer file."
  (interactive)
  (shell-command (concat "nim compile --run " (buffer-file-name))))
