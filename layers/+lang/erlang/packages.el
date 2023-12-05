;;; packages.el --- Erlang Layer packages File for Spacemacs
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


(setq erlang-packages
      '(
        company
        erlang
        dap-mode
        ggtags
        counsel-gtags
        flycheck))


(defun erlang/post-init-company ()
  ;; backend specific
  (add-hook 'erlang-mode-local-vars-hook #'spacemacs//erlang-setup-company))

(defun erlang/init-erlang ()
  (use-package erlang
    :defer t
    ;; explicitly run prog-mode hooks since erlang mode does is not
    ;; derived from prog-mode major-mode
    :hook (erlang-mode . spacemacs/run-prog-mode-hooks)
          (erlang-mode . spacemacs//erlang-default)
          (erlang-mode-local-vars . spacemacs//erlang-setup-backend)
    :init
    ;; (setq erlang-root-dir "/usr/lib/erlang/erts-5.10.3")
    ;; (add-to-list 'exec-path "/usr/lib/erlang/erts-5.10.3/bin")
    ;; (setq erlang-man-root-dir "/usr/lib/erlang/erts-5.10.3/man")
    ;; (add-hook 'erlang-mode-hook
    ;;           (lambda ()
    ;;             (setq mode-name "Erlang")
    ;;             ;; when starting an Erlang shell in Emacs, with a custom node name
    ;;             (setq inferior-erlang-machine-options '("-sname" "syl20bnr"))
    ;;             ))
    (setq erlang-compile-extra-opts '(debug_info))
    :config (require 'erlang-start)))

(defun erlang/pre-init-dap-mode ()
  (add-hook 'erlang-mode-local-vars-hook #'spacemacs//erlang-setup-dap))

(defun erlang/post-init-flycheck ()
  (spacemacs/enable-flycheck 'erlang-mode))

(defun erlang/post-init-ggtags ()
  (add-hook 'erlang-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun erlang/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'erlang-mode))
