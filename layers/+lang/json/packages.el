;;; packages.el --- JSON Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


(defconst json-packages
  '(
    company
    add-node-modules-path
    flycheck
    json-mode
    json-navigator
    json-reformat
    json-snatcher
    prettier-js
    web-beautify))

(defun json/post-init-company ()
  (spacemacs//json-setup-company))

(defun json/post-init-add-node-modules-path ()
  (add-hook 'json-mode-hook #'add-node-modules-path))

(defun json/post-init-flycheck ()
  (spacemacs/enable-flycheck 'json-mode))

(defun json/init-json-mode ()
  (use-package json-mode
    :defer t
    :init
    (unless (eq json-backend 'lsp)
      (spacemacs/declare-prefix-for-mode 'json-mode "mT" "toggle")
      (spacemacs/declare-prefix-for-mode 'json-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'json-mode "m=" "format"))
    (add-hook 'json-mode-hook #'spacemacs//json-setup-backend)))

(defun json/init-json-navigator ()
  (use-package json-navigator
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'json-mode
            "Th" 'spacemacs/json-navigator-dwim)
    :config (evilified-state-evilify-map json-navigator-mode-map
              :mode json-navigator-mode)))

(defun json/init-json-reformat ()
  (use-package json-reformat
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'json-mode
      "==" 'spacemacs/json-reformat-dwim)))

(defun json/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'json-mode
      "hp" 'jsons-print-path)))

(defun json/pre-init-prettier-js ()
  (when (eq json-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'json-mode)
    (add-hook 'json-mode-hook #'spacemacs/json-setup-prettier)
    (when (eq json-fmt-on-save t)
      (add-hook 'json-mode-hook 'prettier-js-mode))))

(defun json/pre-init-web-beautify ()
  (when (eq json-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'json-mode 'web-beautify-js))
    (when (eq json-fmt-on-save t)
      (add-hook 'json-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))))
