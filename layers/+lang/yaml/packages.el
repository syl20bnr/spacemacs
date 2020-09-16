;;; packages.el --- YAML Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst yaml-packages '(company
                          flycheck
                          yaml-mode))

(defun yaml/post-init-company ()
  (unless yaml-enable-lsp
    (spacemacs|add-company-backends :modes yaml-mode)))

(defun yaml/post-init-flycheck ()
  (spacemacs/enable-flycheck 'yaml-mode))

(defun yaml/init-yaml-mode ()
  "Initialize YAML mode"
  (use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode))
    :init
    (when yaml-enable-lsp
      (add-hook 'yaml-mode-hook #'lsp))
    :config (add-hook 'yaml-mode-hook
                      '(lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))
