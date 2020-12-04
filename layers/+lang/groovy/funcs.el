;;; funcs.el --- Groovy functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//groovy-backend ()
  "Return selected backend."
  (if groovy-backend
      groovy-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-groovy))))

(defun space-macs//groovy-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//groovy-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes groovy-mode))
    (`company-groovy (space-macs|add-company-backends
                       :modes groovy-mode))))

(defun space-macs//groovy-setup-backend ()
  "Conditionally setup groovy backend."
  (pcase (space-macs//groovy-backend)
    (`lsp (lsp))))


;; REPL

(defun space-macs/groovy-send-region-switch (start end)
  "Send region content to REPL and switch to it in insert mode."
  (interactive "r")
  (groovy-send-region-and-go start end)
  (evil-insert-state))

(defun space-macs/groovy-send-definition-switch ()
  "Send function content to REPL and switch to it in insert mode."
  (interactive)
  (groovy-send-definition-and-go)
  (evil-insert-state))

(defun space-macs/groovy-load-file ()
  "Save buffer, load it to REPL."
  (interactive)
  (save-buffer)
  (groovy-load-file (buffer-file-name)))

(defun space-macs/groovy-load-file-switch ()
  "Save buffer, load buffer to REPL and switch to it in insert mode."
  (interactive)
  (space-macs/groovy-load-file)
  (switch-to-groovy nil)
  (evil-insert-state))


