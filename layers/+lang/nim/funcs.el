;;; funcs.el --- Nim Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//nim-backend ()
  "Returns selected backend."
  (if nim-backend
      nim-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-nim))))

(defun space-macs//nim-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//nim-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes nim-mode nimscript-mode))
    (`company-nim (space-macs|add-company-backends
                    :backends company-nimsuggest
                    :modes nim-mode nimscript-mode))))

(defun space-macs//nim-setup-backend ()
  "Conditionally setup nim backend."
  (pcase (space-macs//nim-backend)
    (`lsp (lsp))
    (`company-nim (progn
                    (nimsuggest-mode)
                    (add-to-list 'space-macs-jump-handlers-nim-mode 'nimsuggest-find-definition)))))


(defun space-macs/nim-compile-run ()
  "Compile current buffer file."
  (interactive)
  (shell-command (concat "nim compile --run " (buffer-file-name))))


