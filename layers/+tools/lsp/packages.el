;;; packages.el --- Language Server Protocol Layer packages file for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst lsp-packages
  '(
    lsp-mode
    lsp-ui
    (helm-lsp :requires helm)
    (lsp-ivy :requires ivy)
    (lsp-tree-macs :requires tree-macs)
    (lsp-origami :requires lsp-mode)
    popwin))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :config
    (progn
      (space-macs/lsp-bind-keys)
      (setq lsp-prefer-capf t)
      (add-hook 'lsp-after-open-hook (lambda ()
                                       "Setup xref jump handler"
                                       (space-macs//setup-lsp-jump-handler))))))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :config
    (progn
      (if lsp-remap-xref-keybindings
          (progn (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
                 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

      (space-macs/lsp-define-key
       lsp-ui-peek-mode-map
       "h" #'lsp-ui-peek--select-prev-file
       "j" #'lsp-ui-peek--select-next
       "k" #'lsp-ui-peek--select-prev
       "l" #'lsp-ui-peek--select-next-file
       )
      )))

(defun lsp/init-helm-lsp ()
  (use-package helm-lsp :defer t))

(defun lsp/init-lsp-ivy ()
  (use-package lsp-ivy :defer t))

(defun lsp/init-lsp-tree-macs ()
  (use-package lsp-tree-macs :defer t))

(defun lsp/init-lsp-origami ()
  (use-package lsp-origami
    :defer t
    :init
    (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)))

(defun lsp/pre-init-popwin ()
  (space-macs|use-package-add-hook popwin
    :post-config
    (push '("*lsp-help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          popwin:special-display-config)))


