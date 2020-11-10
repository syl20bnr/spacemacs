;;; funcs.el --- Crystal Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//crystal-auto-format-setup ()
  (if crystal-enable-auto-format
      (add-hook 'before-save-hook 'crystal-tool-format nil 'local)
    (remove-hook 'before-save-hook 'crystal-tool-format 'local)))

(defun spacemacs/crystal-run-main ()
  (interactive)
  (let ((default-directory (crystal-find-project-root)))
    (shell-command
     (format "crystal run %s"
             (shell-quote-argument (buffer-file-name))))))

(defun spacemacs//crystal-backend ()
  "Return selected backend."
  (if crystal-backend
      crystal-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-crystal))))

(defun spacemacs//crystal-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//crystal-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes crystal-mode))
    (`company-crystal (spacemacs|add-company-backends
                        :backends company-capf
                        :modes crystal-mode
                        :variables company-tooltip-align-annotations t))))

(defun spacemacs//crystal-setup-backend ()
  "Conditionally setup crystal backend."
  (pcase (spacemacs//crystal-backend)
    (`lsp (lsp))))
