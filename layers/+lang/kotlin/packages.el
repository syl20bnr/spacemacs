;;; packages.el --- kotlin layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Shanavas M <shanavas@disroot.org>
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


(defconst kotlin-packages
  '(
    company
    flycheck
    (flycheck-kotlin :requires flycheck)
    ggtags
    counsel-gtags
    kotlin-mode))

(defun kotlin/post-init-company ()
  (spacemacs//kotlin-setup-company))

(defun kotlin/post-init-flycheck ()
  (spacemacs/enable-flycheck 'kotlin-mode))

(defun kotlin/init-flycheck-kotlin ()
  (use-package flycheck-kotlin
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-kotlin-setup)))

(defun kotlin/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t
    :init
    (setq lsp-clients-kotlin-server-executable kotlin-lsp-jar-path)
    (add-hook 'kotlin-mode-hook #'spacemacs//kotlin-setup-backend)))

(defun kotlin/post-init-ggtags ()
  (add-hook 'kotlin-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun kotlin/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'kotlin-mode))
