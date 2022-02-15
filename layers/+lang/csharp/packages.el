;;; packages.el --- C# Layer packages File for Spacemacs
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


(setq csharp-packages
      '(
        company
        csharp-mode
        evil-matchit
        ggtags
        counsel-gtags
        helm-gtags
        omnisharp
        flycheck
        ))

(defun csharp/init-omnisharp ()
  (use-package omnisharp
    :defer t
    :init
    (spacemacs//csharp-setup-backend)
    :config
    (spacemacs//csharp-configure)
    ))

(defun csharp/post-init-company ()
  (spacemacs//csharp-setup-company))

(defun csharp/init-csharp-mode ()
  (use-package csharp-mode
    :defer t))

(defun csharp/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'csharp-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-c-get-tag evilmi-c-jump))))
  (add-hook 'csharp-mode-hook 'turn-on-evil-matchit-mode))

(defun csharp/post-init-flycheck ()
  (spacemacs/enable-flycheck 'csharp-mode))

(defun csharp/post-init-ggtags ()
  (add-hook 'csharp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun csharp/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'csharp-mode))

(defun csharp/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'csharp-mode))
