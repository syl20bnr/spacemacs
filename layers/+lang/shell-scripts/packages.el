;;; packages.el --- Shell Scripts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq shell-scripts-packages
      '(
        (company-shell :requires company)
        fish-mode
        flycheck
        flycheck-bashate
        ggtags
        counsel-gtags
        helm-gtags
        insert-shebang
        org
        (sh-script :location built-in)
        ))

(defun shell-scripts/init-company-shell ()
  (use-package company-shell
    :defer t
    :init
    (progn
      (spacemacs|add-company-backends
        :backends (company-shell company-shell-env)
        :modes sh-mode)
      (spacemacs|add-company-backends
        :backends (company-shell company-shell-env company-fish-shell)
        :modes fish-mode))))

(defun shell-scripts/post-init-flycheck ()
  (spacemacs/enable-flycheck 'sh-mode))

(defun shell-scripts/init-flycheck-bashate ()
  (use-package flycheck-bashate
    :defer t
    :init (add-hook 'sh-mode-hook 'flycheck-bashate-setup)))

(defun shell-scripts/init-fish-mode ()
  (use-package fish-mode
    :defer t))

(defun shell-scripts/init-sh-script ()
  (use-package sh-script
    :defer t
    :init
    (progn
      ;; Add meaningful names for prefix categories
      (spacemacs/declare-prefix-for-mode 'sh-mode "mi" "insert")
      (spacemacs/declare-prefix-for-mode 'sh-mode "mg" "goto")

      ;; Add standard key bindings for insert commands
      (spacemacs/set-leader-keys-for-major-mode 'sh-mode
        "\\" 'sh-backslash-region
        "ic" 'sh-case
        "ii" 'sh-if
        "if" 'sh-function
        "io" 'sh-for
        "ie" 'sh-indexed-loop
        "iw" 'sh-while
        "ir" 'sh-repeat
        "is" 'sh-select
        "iu" 'sh-until
        "ig" 'sh-while-getopts)

      ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
      (dolist (pattern '("\\.zsh\\'"
                         "zlogin\\'"
                         "zlogout\\'"
                         "zpreztorc\\'"
                         "zprofile\\'"
                         "zshenv\\'"
                         "zshrc\\'"))
        (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

      (defun spacemacs//setup-shell ()
        (when (and buffer-file-name
                   (string-match-p "\\.zsh\\'" buffer-file-name))
          (sh-set-shell "zsh")))
      (add-hook 'sh-mode-hook 'spacemacs//setup-shell)
      (add-hook 'sh-mode-hook 'spacemacs//shell-scripts-setup-backend))))

(defun shell-scripts/post-init-ggtags ()
  (add-hook 'sh-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun shell-scripts/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'sh-mode))

(defun shell-scripts/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'sh-mode))

(defun shell-scripts/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(shell . t))))

(defun shell-scripts/init-insert-shebang ()
  (use-package insert-shebang
    :defer t
    :init
    (progn
      ;; Insert shebang must be available for non shell modes like python or
      ;; groovy but also in the major mode menu with shell specific inserts
      (spacemacs/set-leader-keys-for-major-mode 'sh-mode
        "i!" 'spacemacs/insert-shebang)
      (spacemacs/set-leader-keys "i!" 'spacemacs/insert-shebang)
      ;; we don't want to insert shebang lines automatically
      (remove-hook 'find-file-hook 'insert-shebang))))
