;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq typescript-packages
      '(
        add-node-modules-path
        company
        eldoc
        emmet-mode
        flycheck
        npm-mode
        smartparens
        typescript-mode
        import-js
        web-mode
        yasnippet))


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
  (pcase typescript-linter
    ('tslint (flycheck-add-mode 'typescript-tide 'typescript-tsx-mode)
             (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
    ('eslint (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
             (flycheck-add-mode 'javascript-eslint 'typescript-mode)
             (add-to-list 'flycheck-disabled-checkers 'typescript-tslint)
             (flycheck-add-mode 'tsx-tide 'typescript-tsx-mode)
             (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
             (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append))
    (_ (message
        "Invalid typescript-layer configuration, no such linter: %s" typescript-linter))))

(defun typescript/set-lsp-linter ()
  (pcase typescript-linter
    ('tslint (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
    ;; This sets tslint unconditionally for all lsp clients which is wrong
    ;; Must be set for respective modes only, see go layer for examples.
    ('eslint (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
             (flycheck-add-mode 'javascript-eslint 'typescript-mode))
    (_ (message
        "Invalid typescript-layer configuration, no such linter: %s" typescript-linter))))

(defun typescript/set-linter ()
  (pcase typescript-backend
    ('tide (typescript/set-tide-linter))
    ('lsp (typescript/set-lsp-linter))))

(defun typescript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'typescript-mode)
  (spacemacs/enable-flycheck 'typescript-tsx-mode)
  (spacemacs/add-to-hooks #'spacemacs//typescript-setup-checkers
                          '(typescript-mode-hook typescript-tsx-mode-hook)
                          t)
  (spacemacs/add-to-hooks #'typescript/set-linter
                          '(typescript-mode-local-vars-hook typescript-tsx-mode-local-vars-hook)
                          t))

(defun typescript/post-init-npm-mode ()
  (add-hook 'typescript-mode-hook #'npm-mode)
  (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "npm")
  (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
    "ni" 'npm-mode-npm-install
    "nr" 'npm-mode-npm-run
    "ns" 'npm-mode-npm-install-save
    "nd" 'npm-mode-npm-install-save-dev
    "nn" 'npm-mode-npm-init
    "nu" 'npm-mode-npm-uninstall
    "nl" 'npm-mode-npm-list
    "np" 'npm-mode-visit-project-file)
  (add-hook 'typescript-tsx-mode-hook #'npm-mode)
  (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mn" "npm")
  (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
    "ni" 'npm-mode-npm-install
    "nr" 'npm-mode-npm-run
    "ns" 'npm-mode-npm-install-save
    "nd" 'npm-mode-npm-install-save-dev
    "nn" 'npm-mode-npm-init
    "nu" 'npm-mode-npm-uninstall
    "nl" 'npm-mode-npm-list
    "np" 'npm-mode-visit-project-file))

(defun typescript/post-init-smartparens ()
  (spacemacs/add-to-hooks #'spacemacs//activate-smartparens '(typescript-mode-hook
                                                              typescript-tsx-mode-hook)))

(defun typescript/post-init-yasnippet ()
  (spacemacs/add-to-hooks #'spacemacs/typescript-yasnippet-setup '(typescript-mode-hook
                                                                   typescript-tsx-mode-hook)))

(defun typescript/post-init-web-mode ()
  (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  (spacemacs/typescript-mode-config 'typescript-tsx-mode))

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :init
    (spacemacs/typescript-safe-local-variables '(lsp tide))
    (spacemacs/typescript-mode-init 'typescript-mode-local-vars-hook)
    ;; init tsx locals here to get proper order 
    (spacemacs/typescript-mode-init 'typescript-tsx-mode-local-vars-hook)
    :config (spacemacs/typescript-mode-config 'typescript-mode)))

(defun typescript/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'spacemacs--import-js-modes (cons 'typescript-mode 'typescript-mode-hook))
    (add-to-list 'spacemacs--import-js-modes (cons 'typescript-tsx-mode 'typescript-tsx-mode-hook))))
