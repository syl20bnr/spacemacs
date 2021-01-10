;;; funcs.el --- Julia Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun spacemacs//julia-setup-backend ()
  "Conditionally setup julia backend."
  (pcase julia-backend
    ('lsp (spacemacs//julia-setup-lsp))))

(defun spacemacs//julia-setup-buffer ()
  "Configure julia-mode"
  (when (not julia-mode-enable-ess)
    (spacemacs//julia-setup-repl)))


;; lsp

(defun spacemacs//julia-setup-lsp ()
  "Start lsp-mode and configure for buffer."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; repl

(defun spacemacs//julia-setup-repl ()
  "Start julia-repl minor mode and configure for buffer."
  (julia-repl-mode))


;; misc

(defun spacemacs//julia-hash-to-alist (hash)
  "Convert a `hash-table' to an `alist' for the use in a helm buffer."
  (let (res)
    (maphash (lambda (key value)
               (push `(,key . ,value) res))
             hash)
    res))

(when (configuration-layer/layer-used-p 'helm)
  (defun spacemacs//julia-helm-math-insert()
    "Insert a utf8 symbol from `julia-latexsubs'"
    (interactive)
    (helm :sources (helm-build-sync-source "test"
                     :candidates (spacemacs//julia-hash-to-alist julia-latexsubs)
                     :fuzzy-match t
                     :action (lambda (candidate) (insert candidate)))
          :buffer "*helm julia latex insert*")))
