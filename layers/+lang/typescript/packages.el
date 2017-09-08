;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
  (add-hook 'web-mode-hook #'add-node-modules-path))

(defun typescript/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-tide
    :modes typescript-mode web-mode))

(defun typescript/post-init-eldoc ()
  (add-hook 'typescript-mode-hook 'eldoc-mode)
  (add-hook 'web-mode-hook 'spacemacs//typescript-web-mode-enable-eldoc))

(defun typescript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'typescript-mode)
  (add-hook 'web-mode-hook 'spacemacs//typescript-web-mode-enable-flycheck))

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
      (add-hook 'typescript-mode-hook 'tide-setup)
      (add-to-list 'spacemacs-jump-handlers-typescript-mode
                   '(tide-jump-to-definition :async t)))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "name")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mr" "rename")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mS" "server")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "ms" "send")

      (setq keybindingList '("gb" tide-jump-back
                             "gt" typescript/jump-to-type-def
                             "gu" tide-references
                             "hh" tide-documentation-at-point
                             "rr" tide-rename-symbol
                             "sr" tide-restart-server)
            typescriptList (cons 'typescript-mode keybindingList)
            webList (cons 'web-mode (cons "gg" (cons 'tide-jump-to-definition
                                                     keybindingList ))))
      (apply 'spacemacs/set-leader-keys-for-major-mode typescriptList)
      (apply 'spacemacs/set-leader-keys-for-major-mode webList))))

(defun typescript/post-init-web-mode ()
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook 'spacemacs//typescript-web-mode-enable-tide))

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :config
    (progn
      (when typescript-fmt-on-save
        (add-hook 'typescript-mode-hook 'spacemacs/typescript-fmt-before-save-hook))
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "="  'spacemacs/typescript-format
        "sp" 'spacemacs/typescript-open-region-in-playground))))
