;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typescript-packages
      '(
        add-node-modules-path
        company
        eldoc
        flycheck
        lsp-javascript-typescript
        smartparens
        tide
        typescript-mode
        web-mode
        yasnippet
        ))

(defun typescript/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(typescript-mode-hook
                                             typescript-tsx-mode-hook)))

(defun typescript/post-init-company ()
  (spacemacs/add-to-hooks #'spacemacs//typescript-setup-company
                   '(typescript-mode-local-vars-hook
                   typescript-tsx-mode-local-vars-hook)))

(defun typescript/post-init-eldoc ()
  (spacemacs/add-to-hooks #'spacemacs//typescript-setup-eldoc
                   '(typescript-mode-local-vars-hook
                     typescript-tsx-mode-local-vars-hook) t))

(defun typescript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'typescript-mode)
  (spacemacs/enable-flycheck 'typescript-tsx-mode)
  (with-eval-after-load 'tide
    (with-eval-after-load 'flycheck
      (flycheck-add-mode 'typescript-tide 'typescript-tsx-mode)
      (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))))

(defun typescript/post-init-lsp-javascript-typescript ()
  (spacemacs//setup-lsp-jump-handler 'typescript-mode
                                     'typescript-tsx-mode))

(defun typescript/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (spacemacs/add-to-hooks #'smartparens-strict-mode '(typescript-mode-hook
                                                   typescript-tsx-mode-hook))
    (spacemacs/add-to-hooks #'smartparens-mode '(typescript-mode-hook
                                          typescript-tsx-mode-hook))))

(defun typescript/post-init-tide ()
  (add-to-list 'tide--key-bindings-modes 'typescript-mode)
  (add-to-list 'tide--key-bindings-modes 'typescript-tsx-mode)
  (spacemacs//tide-set-leader-keys-for-major-modes 'typescript-mode 'typescript-tsx-mode))

(defun typescript/post-init-web-mode ()
  (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))

(defun typescript/post-init-yasnippet ()
  (spacemacs/add-to-hooks #'spacemacs/typescript-yasnippet-setup '(typescript-mode-hook
                                                     typescript-tsx-mode-hook)))

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :init
    (progn
      ;; setup typescript backend
      (spacemacs/add-to-hooks #'spacemacs//typescript-setup-backend
                       '(typescript-mode-local-vars-hook
                         typescript-tsx-mode-local-vars-hook))
      ;; safe values for backend to be used in directory file variables
      (dolist (value '(lsp tide))
        (add-to-list 'safe-local-variable-values
                     (cons 'typescript-backend value))))
    :config
    (progn
      (when typescript-fmt-on-save
        (spacemacs/add-to-hooks 'spacemacs/typescript-fmt-before-save-hook
                         '(typescript-mode-hook typescript-tsx-mode-hook)))
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "="  'spacemacs/typescript-format
        "sp" 'spacemacs/typescript-open-region-in-playground)
      (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
        "="  'spacemacs/typescript-format
        "sp" 'spacemacs/typescript-open-region-in-playground))))
