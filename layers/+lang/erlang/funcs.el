;;; funcs.el --- Erlang Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Carlos F. Clavijo <arkan1313@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//erlang-backend ()
  "Returns selected backend."
  (if erlang-backend
      erlang-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-erlang))))

(defun spacemacs//erlang-setup-backend ()
  "Conditionally setup erlang backend."
  (pcase (spacemacs//erlang-backend)
    (`lsp (spacemacs//erlang-setup-lsp)))
  )

(defun spacemacs//erlang-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//erlang-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes erlang-mode
            :append-hooks t))))

(defun spacemacs//erlang-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//erlang-default ()
  "Default settings for erlang buffers"

  ;; Use a custom fill-column for erlang buffers
  (set-fill-column erlang-fill-column))
