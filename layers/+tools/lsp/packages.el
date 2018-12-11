;;; packages.el --- Language Server Protocol Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lsp-packages
  '(
     lsp-mode
     lsp-ui
     (company-lsp :requires company)
     ))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :config
    (progn
      (require 'lsp-clients)
      (setq lsp-prefer-flymake nil)
      (spacemacs/lsp-bind-keys)
      (add-hook 'lsp-after-open-hook (lambda ()
                                       "Setup xref jump handler and declare keybinding prefixes"
                                       (spacemacs//setup-lsp-jump-handler major-mode)
                                       (spacemacs//lsp-declare-prefixes-for-mode major-mode))))))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :config
    (progn
      (if lsp-remap-xref-keybindings
        (progn (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
          (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

      (spacemacs/lsp-define-key
        lsp-ui-peek-mode-map
        "h" #'lsp-ui-peek--select-prev-file
        "j" #'lsp-ui-peek--select-next
        "k" #'lsp-ui-peek--select-prev
        "l" #'lsp-ui-peek--select-next-file
        )
      )))

(defun lsp/init-company-lsp ()
  (use-package company-lsp :defer t))
