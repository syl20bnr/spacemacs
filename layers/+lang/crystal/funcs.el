;;; funcs.el --- Crystal Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//crystal-auto-format-setup ()
  (if crystal-enable-auto-format
      (add-hook 'before-save-hook 'crystal-tool-format nil 'local)
    (remove-hook 'before-save-hook 'crystal-tool-format 'local)))

(defun space-macs/crystal-run-main ()
  (interactive)
  (let ((default-directory (crystal-find-project-root)))
    (shell-command
     (format "crystal run %s"
             (shell-quote-argument (buffer-file-name))))))

(defun space-macs//crystal-backend ()
  "Return selected backend."
  (if crystal-backend
      crystal-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-crystal))))

(defun space-macs//crystal-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//crystal-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes crystal-mode))
    (`company-crystal (space-macs|add-company-backends
                        :backends company-capf
                        :modes crystal-mode
                        :variables company-tooltip-align-annotations t))))

(defun space-macs//crystal-setup-backend ()
  "Conditionally setup crystal backend."
  (pcase (space-macs//crystal-backend)
    (`lsp (lsp))))


