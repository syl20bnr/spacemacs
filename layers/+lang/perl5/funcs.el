;;; funcs.el --- Perl5 Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//perl5-backend ()
  "Return selected backend."
  (if perl5-backend
      perl5-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-plsense))))

(defun space-macs//perl5-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//perl5-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes cperl-mode))
    (`company-plsense (space-macs|add-company-backends)
                      :backends company-plsense
                      :modes cperl-mode)))

(defun space-macs//perl5-setup-backend ()
  "Conditionally setup perl5 backend."
  (pcase (space-macs//perl5-backend)
    (`lsp (lsp))))

(defun space-macs//perl5-setup-dap ()
  "Conditionally setup perl5 DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (space-macs//perl5-backend)
    (`lsp (add-hook 'cperl-mode-hook #'dap-mode))))

(defun space-macs//perl5-smartparens-enable ()
  (define-key cperl-mode-map "{" nil))

(defun space-macs//perl5-spartparens-disable ()
  (define-key cperl-mode-map "{" 'cperl-electric-lbrace))

(defun space-macs/perltidy-format ()
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

(defun space-macs/perltidy-format-buffer ()
  "Format current buffer with perltidy."
  (interactive)
  (let ((old-point (point))
        (old-window-start (window-start)))
    (mark-whole-buffer)
    (space-macs/perltidy-format)
    (goto-char old-point)
    (set-window-start (selected-window) old-window-start)))

(defun space-macs/perltidy-format-function ()
  "Format current function with perltidy."
  (interactive)
  (let ((old-point (point))
        (old-window-start (window-start)))
    (mark-defun)
    (space-macs/perltidy-format)
    (goto-char old-point)
    (set-window-start (selected-window) old-window-start)))


