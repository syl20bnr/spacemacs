;;; packages.el --- Groovy Layer packages File for Spacemacs
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


(defconst groovy-packages
  '(
    company
    flycheck
    groovy-imports
    groovy-mode
    org))

(defun groovy/post-init-company ()
  (add-hook 'groovy-mode-local-vars-hook 'spacemacs//groovy-setup-company))

(defun groovy/post-init-flycheck ()
  (spacemacs/enable-flycheck 'groovy-mode))

(defun groovy/init-groovy-imports ()
  (use-package groovy-imports
    :defer t
    :init
    (add-hook 'groovy-mode-hook 'groovy-imports-scan-file)
    (spacemacs/set-leader-keys-for-major-mode 'groovy-mode
      "ri" 'groovy-imports-add-import-dwim)))

(defun groovy/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    :hook (groovy-mode-local-vars . spacemacs//groovy-setup-backend)
    :init
    (setq lsp-groovy-server-file groovy-lsp-jar-path)
    (spacemacs/declare-prefix-for-mode 'groovy-mode "ms" "REPL")
    (spacemacs/set-leader-keys-for-major-mode 'groovy-mode
      "'"  'run-groovy
      "sB" 'spacemacs/groovy-load-file-switch
      "sb" 'spacemacs/groovy-load-file
      "sF" 'spacemacs/groovy-send-definition-switch
      "sf" 'groovy-send-definition
      "si" 'run-groovy
      "sR" 'spacemacs/groovy-send-region-switch
      "sr" 'groovy-send-region)))

(defun groovy/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(groovy . t))))
