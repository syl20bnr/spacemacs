;;; funcs.el --- Perl5 Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
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


(defun spacemacs//perl5-setup-company ()
  "Conditionally setup company based on backend."
  (pcase perl5-backend
    ('lsp
     (spacemacs|add-company-backends ;; Activate lsp company explicitly to activate
       :backends 'company-capf       ;; standard backends as well
       :modes cperl-mode))
    ('company-plsense
     (spacemacs|add-company-backends
       :backends 'company-plsense
       :modes cperl-mode))))

(defun spacemacs//perl5-setup-backend ()
  "Conditionally setup perl5 backend."
  (when (eq perl5-backend 'lsp)
    (lsp-deferred)))

(defun spacemacs//perl5-setup-dap ()
  "Conditionally setup perl5 DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq perl5-backend 'lsp)
    (add-hook 'cperl-mode-hook #'dap-mode)))

(defun spacemacs//perl5-smartparens-enable ()
  (define-key cperl-mode-map "{" nil))

(defun spacemacs//perl5-spartparens-disable ()
  (define-key cperl-mode-map "{" 'cperl-electric-lbrace))

(defun spacemacs/perltidy-format ()
  "Format Perl code with perltidy.
If region is active, operate on it, else operate on line."
  (interactive)
  (let ((old-point (point))
        (pos
         (if (use-region-p)
             (cons (region-beginning)
                   (if (char-equal ?\n (char-before (region-end)))
                       (region-end)
                     (save-excursion ;; must including terminating newline
                       (goto-char (region-end))
                       (1+ (line-end-position)))))
           (cons (line-beginning-position)
                 (1+ (line-end-position))))))
    (apply #'call-process-region (car pos) (cdr pos) perl5-perltidy-executable t '(t nil)
           "--quiet"
           "--standard-error-output"
           perl5-perltidy-options)
    (goto-char old-point)))

(defun spacemacs/perltidy-format-buffer ()
  "Format current buffer with perltidy."
  (interactive)
  (let ((old-point (point))
        (old-window-start (window-start)))
    (mark-whole-buffer)
    (spacemacs/perltidy-format)
    (goto-char old-point)
    (set-window-start (selected-window) old-window-start)))

(defun spacemacs/perltidy-format-function ()
  "Format current function with perltidy."
  (interactive)
  (let ((old-point (point))
        (old-window-start (window-start)))
    (mark-defun)
    (spacemacs/perltidy-format)
    (goto-char old-point)
    (set-window-start (selected-window) old-window-start)))
