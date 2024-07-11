;;; packages.el --- helpful layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Johnson Denen <johnson@johnsons-macbook-pro.local>
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


(defconst helpful-packages
  '(
    helpful
    link-hint
    popwin))

(defun helpful/init-helpful ()
  (use-package helpful
    :defer t
    :init
    (spacemacs/declare-prefix-for-mode 'helpful-mode "mg" "goto")
    (with-eval-after-load 'help-fns
      (defalias 'helpful/original-describe-function (symbol-function 'describe-function))
      (defalias 'helpful/original-describe-variable (symbol-function 'describe-variable))
      (defalias 'helpful/original-describe-key (symbol-function 'describe-key))
      (defalias 'describe-function 'helpful-callable)
      (defalias 'describe-variable 'helpful-variable)
      (defalias 'describe-key 'helpful-key))
    :config
    (evil-set-initial-state 'helpful-mode 'normal)
    (spacemacs/set-leader-keys-for-major-mode 'helpful-mode
      (kbd "q") 'helpful-kill-buffers)
    (evil-define-key 'normal helpful-mode-map (kbd "gr") 'helpful-update)
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window)
    (add-hook 'helpful-mode-hook (lambda () (setq-local tab-width 8)))
    (advice-add 'helpful--navigate :after (lambda (_) (setq-local tab-width 8)))))

(defun helpful/post-init-link-hint ()
  (with-eval-after-load 'helpful
    (evil-define-key 'normal helpful-mode-map (kbd "o") 'link-hint-open-link)))

(defun helpful/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '(helpful-mode :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)))
