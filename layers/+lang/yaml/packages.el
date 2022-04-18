;;; packages.el --- YAML Layer packages File for Spacemacs
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


(defconst yaml-packages '(company
                          flycheck
                          yaml-mode))

(defun yaml/post-init-company ()
  (unless yaml-enable-lsp
    (spacemacs|add-company-backends :modes yaml-mode)))

(defun yaml/post-init-flycheck ()
  (spacemacs/enable-flycheck 'yaml-mode))

(defun yaml/init-yaml-mode ()
  "Initialize YAML mode"
  (use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode))
    :init
    (when yaml-enable-lsp
      (add-hook 'yaml-mode-hook #'lsp))
    :config (add-hook 'yaml-mode-hook
                      (lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))
