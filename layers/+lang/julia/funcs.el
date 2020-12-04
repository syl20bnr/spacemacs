;;; funcs.el --- Julia Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


(defun space-macs//julia-setup-backend ()
  "Conditionally setup julia backend."
  (pcase julia-backend
    ('lsp (space-macs//julia-setup-lsp))))

(defun space-macs//julia-setup-buffer ()
  "Configure julia-mode"
  (when (not julia-mode-enable-ess)
    (space-macs//julia-setup-repl)))


;; lsp

(defun space-macs//julia-setup-lsp ()
  "Start lsp-mode and configure for buffer."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; repl

(defun space-macs//julia-setup-repl ()
  "Start julia-repl minor mode and configure for buffer."
  (julia-repl-mode))


;; misc

(defun space-macs//julia-hash-to-alist (hash)
  "Convert a `hash-table' to an `alist' for the use in a helm buffer."
  (let (res)
    (maphash (lambda (key value)
               (push `(,key . ,value) res))
             hash)
    res))

(when (configuration-layer/layer-used-p 'helm)
  (defun space-macs//julia-helm-math-insert()
    "Insert a utf8 symbol from `julia-latexsubs'"
    (interactive)
    (helm :sources (helm-build-sync-source "test"
                     :candidates (space-macs//julia-hash-to-alist julia-latexsubs)
                     :fuzzy-match t
                     :action (lambda (candidate) (insert candidate)))
          :buffer "*helm julia latex insert*")))


