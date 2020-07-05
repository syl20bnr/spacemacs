;;; funcs.el --- Groovy functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//groovy-backend ()
  "Return selected backend."
  (if groovy-backend
      groovy-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-groovy))))

(defun spacemacs//groovy-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//groovy-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes groovy-mode))
    (`company-groovy (spacemacs|add-company-backends)
                     :modes groovy-mode)))

(defun spacemacs//groovy-setup-backend ()
  "Conditionally setup groovy backend."
  (pcase (spacemacs//groovy-backend)
    (`lsp (lsp))))


;; REPL

(defun spacemacs/groovy-send-region-switch (start end)
  "Send region content to REPL and switch to it in insert mode."
  (interactive "r")
  (groovy-send-region-and-go start end)
  (evil-insert-state))

(defun spacemacs/groovy-send-definition-switch ()
  "Send function content to REPL and switch to it in insert mode."
  (interactive)
  (groovy-send-definition-and-go)
  (evil-insert-state))

(defun spacemacs/groovy-load-file ()
  "Save buffer, load it to REPL."
  (interactive)
  (save-buffer)
  (groovy-load-file (buffer-file-name)))

(defun spacemacs/groovy-load-file-switch ()
  "Save buffer, load buffer to REPL and switch to it in insert mode."
  (interactive)
  (spacemacs/groovy-load-file)
  (switch-to-groovy nil)
  (evil-insert-state))
