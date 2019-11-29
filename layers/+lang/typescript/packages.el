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
        emmet-mode
        flycheck
        smartparens
        tide
        typescript-mode
        import-js
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

(defun typescript/post-init-emmet-mode ()
  (add-hook 'typescript-tsx-mode-hook #'spacemacs/typescript-emmet-mode))

(defun typescript/set-tide-linter ()
  (with-eval-after-load 'tide
    (with-eval-after-load 'flycheck
      (cond ((eq typescript-linter `tslint)
             (flycheck-add-mode 'typescript-tide 'typescript-tsx-mode)
             (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
            ((eq typescript-linter `eslint)
             (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
             (flycheck-add-mode 'javascript-eslint 'typescript-mode)
             (add-to-list 'flycheck-disabled-checkers 'typescript-tslint)
             (flycheck-disable-checker 'typescript-tslint)
             (flycheck-add-mode 'tsx-tide 'typescript-tsx-mode)
             (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
             (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append))
            (t
             (message
              "Invalid typescript-layer configuration, no such linter: %s" typescript-linter))))))

(defun typescript/set-lsp-linter ()
  (with-eval-after-load 'lsp-ui
    (with-eval-after-load 'flycheck
      (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
      (flycheck-add-mode 'javascript-eslint 'typescript-mode))))

(defun typescript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'typescript-mode)
  (spacemacs/enable-flycheck 'typescript-tsx-mode)
  (cond ((eq (spacemacs//typescript-backend) `tide)
         (typescript/set-tide-linter))
        ((eq (spacemacs//typescript-backend) `lsp)
         (typescript/set-lsp-linter)))

  (spacemacs/add-to-hooks #'spacemacs//typescript-setup-checkers
                          '(typescript-mode-hook typescript-tsx-mode-hook)
                          t))

(defun typescript/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (spacemacs/add-to-hooks #'smartparens-strict-mode '(typescript-mode-hook
                                                          typescript-tsx-mode-hook))
    (spacemacs/add-to-hooks #'smartparens-mode '(typescript-mode-hook
                                                 typescript-tsx-mode-hook))))

(defun typescript/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
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
                             "Ed" tide-add-tslint-disable-next-line
                             "gb" tide-jump-back
                             "gg" tide-jump-to-definition
                             "gt" spacemacs/typescript-jump-to-type-def
                             "gu" tide-references
                             "hh" tide-documentation-at-point
                             "ri" tide-organize-imports
                             "rr" tide-rename-symbol
                             "rf" tide-rename-file
                             "sr" tide-restart-server)
            typescriptList (cons 'typescript-mode keybindingList)
            typescriptTsxList (cons 'typescript-tsx-mode
                                    (cons "gg" (cons 'tide-jump-to-definition
                                                     keybindingList ))))
      (apply 'spacemacs/set-leader-keys-for-major-mode typescriptList)
      (apply 'spacemacs/set-leader-keys-for-major-mode typescriptTsxList)))

  (add-to-list 'spacemacs-jump-handlers-typescript-tsx-mode
               '(tide-jump-to-definition :async t))
  (add-to-list 'spacemacs-jump-handlers-typescript-mode
               '(tide-jump-to-definition :async t)))

(defun typescript/post-init-web-mode ()
  (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

  ;; setup typescript backend
  (add-hook 'typescript-tsx-mode-local-vars-hook 'spacemacs//typescript-setup-backend)
  (spacemacs/typescript-safe-local-variables '(lsp tide))
  (when typescript-fmt-on-save
    (add-hook 'typescript-tsx-mode-hook 'spacemacs/typescript-fmt-before-save-hook))
  (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
    "="  'spacemacs/typescript-format
    "sp" 'spacemacs/typescript-open-region-in-playground))

(defun typescript/post-init-yasnippet ()
  (spacemacs/add-to-hooks #'spacemacs/typescript-yasnippet-setup '(typescript-mode-hook
                                                                   typescript-tsx-mode-hook)))

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :init
    (progn
      ;; setup typescript backend
      (add-hook 'typescript-mode-local-vars-hook 'spacemacs//typescript-setup-backend)
      (spacemacs/typescript-safe-local-variables '(lsp tide))
      :config
      (progn
        (when typescript-fmt-on-save
          (add-hook 'typescript-mode-hook 'spacemacs/typescript-fmt-before-save-hook))
        (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
          "="  'spacemacs/typescript-format
          "sp" 'spacemacs/typescript-open-region-in-playground)
        (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
          "="  'spacemacs/typescript-format
          "sp" 'spacemacs/typescript-open-region-in-playground)))))

(defun typescript/pre-init-import-js ()
  (if (eq javascript-import-tool 'import-js)
      (progn
        (add-to-list 'spacemacs--import-js-modes (cons 'typescript-mode 'typescript-mode-hook))
        (add-to-list 'spacemacs--import-js-modes (cons 'typescript-tsx-mode 'typescript-tsx-mode-hook)))))
