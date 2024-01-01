;;; packages.el --- cfengine layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Nick Anderson <nick@cmdln.org>
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


(defconst cfengine-packages
  '(
    (cfengine3-mode :location built-in)
    company
    eldoc
    flycheck
    (ob-cfengine3 :requires org)
    org
    mustache-mode
    ))

(defun cfengine/init-cfengine3-mode ()
  (use-package cfengine3-mode
    :defer t
    :mode ("\\.cf\\'" . cfengine3-mode)
    :init (spacemacs/set-leader-keys-for-major-mode 'cfengine3-mode
            "j" 'cfengine3-reformat-json-string)))

(defun cfengine/post-init-company ()
  (spacemacs|add-company-backends :modes cfengine3-mode))

(defun cfengine/post-init-eldoc ()
  (add-hook 'cfengine3-mode-hook 'eldoc-mode))

(defun cfengine/post-init-flycheck ()
  (spacemacs/enable-flycheck 'cfengine3-mode))

(defun cfengine/init-ob-cfengine3 ()
  (use-package ob-cfengine3
    :defer t))

(defun cfengine/pre-init-org ()
  (when (configuration-layer/layer-used-p 'org)
    (spacemacs|use-package-add-hook org
      :post-config (add-to-list 'org-babel-load-languages '(cfengine3 . t)))))

(defun cfengine/init-mustache-mode ()
  (use-package mustache-mode
    :init (add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode))
    :defer t))
