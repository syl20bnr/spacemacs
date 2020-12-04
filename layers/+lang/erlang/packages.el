;;; packages.el --- Erlang Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq erlang-packages
      '(
        company
        erlang
        ggtags
        counsel-gtags
        helm-gtags
        flycheck
        ))

(defun erlang/post-init-company ()
  ;; backend specific
  (space-macs//erlang-setup-company))

(defun erlang/init-erlang ()
  (use-package erlang
    :defer t
    :init
    (progn
      ;; explicitly run prog-mode hooks since erlang mode does is not
      ;; derived from prog-mode major-mode
      (space-macs/add-to-hook 'erlang-mode-hook
                             '(space-macs/run-prog-mode-hooks
                               space-macs//erlang-setup-backend
                               space-macs//erlang-default))
      ;; (setq erlang-root-dir "/usr/lib/erlang/erts-5.10.3")
      ;; (add-to-list 'exec-path "/usr/lib/erlang/erts-5.10.3/bin")
      ;; (setq erlang-man-root-dir "/usr/lib/erlang/erts-5.10.3/man")
      ;; (add-hook 'erlang-mode-hook
      ;;           (lambda ()
      ;;             (setq mode-name "Erlang")
      ;;             ;; when starting an Erlang shell in e-macs, with a custom node name
      ;;             (setq inferior-erlang-machine-options '("-sname" "syl20bnr"))
      ;;             ))
      (setq erlang-compile-extra-opts '(debug_info)))
    :config (require 'erlang-start)))

(defun erlang/post-init-flycheck ()
  (space-macs/enable-flycheck 'erlang-mode))

(defun erlang/post-init-ggtags ()
  (add-hook 'erlang-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun erlang/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'erlang-mode))

(defun erlang/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'erlang-mode))


