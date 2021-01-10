;;; funcs.el --- Perl5 Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//perl5-backend ()
  "Return selected backend."
  (if perl5-backend
      perl5-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-plsense))))

(defun spacemacs//perl5-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//perl5-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes cperl-mode))
    (`company-plsense (spacemacs|add-company-backends)
                      :backends company-plsense
                      :modes cperl-mode)))

(defun spacemacs//perl5-setup-backend ()
  "Conditionally setup perl5 backend."
  (pcase (spacemacs//perl5-backend)
    (`lsp (lsp))))

(defun spacemacs//perl5-setup-dap ()
  "Conditionally setup perl5 DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//perl5-backend)
    (`lsp (add-hook 'cperl-mode-hook #'dap-mode))))

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
