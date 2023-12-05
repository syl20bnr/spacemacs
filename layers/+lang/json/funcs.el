;;; funcs.el --- JSON Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs//json-setup-company ()
  "Conditionally setup company based on backend."
  ;; Activate lsp company explicitly to activate
  ;; standard backends as well
  (when (eq json-backend 'lsp)
    (spacemacs|add-company-backends
      :backends company-capf
      :modes json-mode)))

(defun spacemacs//json-setup-backend ()
  "Conditionally setup json backend."
  (when (eq json-backend 'lsp)
    (lsp-deferred)))

(defun spacemacs/json-navigator-dwim (arg)
  "Display the JSON hierarchy of the whole buffer or the active region.
If ARG is a universal prefix argument then display the hierarchy after point."
  (interactive "P")
  (if arg
      (json-navigator-navigate-after-point)
    (if (not (use-region-p))
        (save-excursion (json-navigator-navigate-region (point-min) (point-max)))
      (json-navigator-navigate-region (region-beginning) (region-end)))))

(defun spacemacs/json-reformat-dwim (arg)
  "Reformat the whole buffer of the active region.
If ARG is non-nil (universal prefix argument) then try to decode the strings.
If ARG is a numerical prefix argument then specify the indentation level."
  (interactive "P")
  (let ((json-reformat:indent-width js-indent-level)
        (json-reformat:pretty-string? nil))
    (cond
     ((numberp arg) (setq json-reformat:indent-width arg))
     (arg (setq json-reformat:pretty-string? t)))
    (if (not (use-region-p))
        (save-excursion (json-reformat-region (point-min) (point-max)))
      (json-reformat-region (region-beginning) (region-end)))))

(defun spacemacs/json-setup-prettier ()
  "Tell prettier the content is to be parsed as JSON regardless of any file
extensions."
  (setq-local prettier-js-args '("--parser=json")))
