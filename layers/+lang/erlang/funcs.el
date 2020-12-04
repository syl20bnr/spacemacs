;;; funcs.el --- Erlang Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Carlos F. Clavijo <arkan1313@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//erlang-backend ()
  "Returns selected backend."
  (if erlang-backend
      erlang-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-erlang))))

(defun space-macs//erlang-setup-backend ()
  "Conditionally setup erlang backend."
  (pcase (space-macs//erlang-backend)
    (`lsp (space-macs//erlang-setup-lsp)))
  )

(defun space-macs//erlang-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//erlang-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes erlang-mode
            :append-hooks t))))

(defun space-macs//erlang-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun space-macs//erlang-default ()
  "Default settings for erlang buffers"

  ;; Use a custom fill-column for erlang buffers
  (set-fill-column erlang-fill-column))


