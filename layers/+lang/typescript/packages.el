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
        tide
        typescript-mode
        web-mode
        ))

(defun typescript/post-init-add-node-modules-path ()
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'typescript-tsx-mode-hook #'add-node-modules-path))

(defun typescript/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-tide
    :modes typescript-mode typescript-tsx-mode))

(defun typescript/pre-init-eldoc ()
  (spacemacs|use-package-add-hook tide :post-init
                           (add-hook 'typescript-tsx-mode-hook 'eldoc-mode t)
                           (add-hook 'typescript-mode-hook 'eldoc-mode t)))

(defun typescript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'typescript-mode)
  (spacemacs/enable-flycheck 'typescript-tsx-mode)
  (with-eval-after-load 'tide
    (flycheck-add-mode 'typescript-tide 'typescript-tsx-mode))
  (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))

(defun typescript/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
    :init
    (progn
      (evilified-state-evilify tide-references-mode tide-references-mode-map
        (kbd "C-k") 'tide-find-previous-reference
        (kbd "C-j") 'tide-find-next-reference
        (kbd "C-l") 'tide-goto-reference)
      (spacemacs/add-to-hooks 'tide-setup '(typescript-mode-hook
                                            typescript-tsx-mode-hook))
      (add-to-list 'spacemacs-jump-handlers-typescript-tsx-mode
                   '(tide-jump-to-definition :async t))
      (add-to-list 'spacemacs-jump-handlers-typescript-mode
                   '(tide-jump-to-definition :async t)))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mE" "errors")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mE" "errors")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "name")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mn" "name")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mS" "server")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mS" "server")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "ms" "send")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "ms" "send")

      (setq keybindingList '("Ee" tide-fix
                             "gb" tide-jump-back
                             "gt" typescript/jump-to-type-def
                             "gu" tide-references
                             "hh" tide-documentation-at-point
                             "rr" tide-rename-symbol
                             "sr" tide-restart-server)
            typescriptList (cons 'typescript-mode keybindingList)
            typescriptTsxList (cons 'typescript-tsx-mode
                                    (cons "gg" (cons 'tide-jump-to-definition
                                                     keybindingList ))))
      (apply 'spacemacs/set-leader-keys-for-major-mode typescriptList)
      (apply 'spacemacs/set-leader-keys-for-major-mode typescriptTsxList))))

(defun typescript/post-init-web-mode ()
  (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :config
    (progn
      (when typescript-fmt-on-save
        (add-hook 'typescript-mode-hook 'spacemacs/typescript-fmt-before-save-hook)
        (add-hook 'typescript-tsx-mode-hook 'spacemacs/typescript-fmt-before-save-hook))
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "="  'spacemacs/typescript-format
        "sp" 'spacemacs/typescript-open-region-in-playground)
      (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
        "="  'spacemacs/typescript-format
        "sp" 'spacemacs/typescript-open-region-in-playground))))
