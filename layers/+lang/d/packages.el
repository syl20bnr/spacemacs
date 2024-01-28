;;; packages.el --- d Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq d-packages
      '(
        company
        (company-dcd :requires company)
        d-mode
        flycheck
        (flycheck-dmd-dub :requires flycheck)
        ggtags
        counsel-gtags))


(defun d/post-init-company ()
  ;; Need to convince company that this C-derived mode is a code mode.
  (with-eval-after-load 'company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'd-mode)))

(defun d/init-company-dcd ()
  (use-package company-dcd
    :defer t
    :init
    (spacemacs|add-company-backends :backends company-dcd :modes d-mode)
    (spacemacs/set-leader-keys-for-major-mode 'd-mode
      "gg" 'company-dcd-goto-definition
      "gb" 'company-dcd-goto-def-pop-marker
      "hh" 'company-dcd-show-ddoc-with-buffer
      "gr" 'company-dcd-ivy-search-symbol)))

(defun d/init-d-mode ()
  (use-package d-mode :defer t))

(defun d/post-init-flycheck ()
  (spacemacs/enable-flycheck 'd-mode))

(defun d/init-flycheck-dmd-dub ()
  (use-package flycheck-dmd-dub :defer t
    :init
    (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables)))

(defun d/post-init-ggtags ()
  (add-hook 'd-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun d/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'd-mode))
