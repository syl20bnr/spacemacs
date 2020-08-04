;;; funcs.el --- JSON Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//json-backend ()
  "Returns selected backend."
  (if json-backend
      json-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-json))))

(defun spacemacs//json-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//json-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes json-mode))))

(defun spacemacs//json-setup-backend ()
  "Conditionally setup json backend."
  (pcase (spacemacs//json-backend)
    (`lsp (lsp))))

(defun spacemacs/json-navigator-dwim (arg &optional start end)
  "Display the JSON hierarchy of the whole buffer or the active region.
If ARG is a universal prefix argument then display the hierarchy after point."
  (interactive "P\nr")
  (if arg
      (json-navigator-navigate-after-point)
    (if (equal start end)
        (save-excursion (json-navigator-navigate-region (point-min) (point-max)))
      (json-navigator-navigate-region start end))))

(defun spacemacs/json-reformat-dwim (arg &optional start end)
  "Reformat the whole buffer of the active region.
If ARG is non-nil (universal prefix argument) then try to decode the strings.
If ARG is a numerical prefix argument then specify the indentation level."
  (interactive "P\nr")
  (let ((json-reformat:indent-width js-indent-level)
        (json-reformat:pretty-string? nil))
    (cond
     ((numberp arg) (setq json-reformat:indent-width arg))
     (arg (setq json-reformat:pretty-string? t)))
    (if (equal start end)
        (save-excursion (json-reformat-region (point-min) (point-max)))
      (json-reformat-region start end))))

(defun spacemacs/json-setup-prettier ()
  "Tell prettier the content is to be parsed as JSON regardless of any file
extensions."
  (setq-local prettier-js-args '("--parser=json")))
