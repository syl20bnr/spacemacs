;;; packages.el --- Erlang Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq erlang-packages
  '(
    company
    erlang
    flycheck
    ))

(defun erlang/post-init-company ()
  (add-hook 'erlang-mode-hook 'company-mode))

(defun erlang/init-erlang ()
  (use-package erlang
    :defer t
    :init
    (progn
      ;; explicitly run prog-mode hooks since erlang mode does is not
      ;; derived from prog-mode major-mode
      (add-hook 'erlang-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
      ;; (setq erlang-root-dir "/usr/lib/erlang/erts-5.10.3")
      ;; (add-to-list 'exec-path "/usr/lib/erlang/erts-5.10.3/bin")
      ;; (setq erlang-man-root-dir "/usr/lib/erlang/erts-5.10.3/man")
      ;; (add-hook 'erlang-mode-hook
      ;;           (lambda ()
      ;;             (setq mode-name "Erlang")
      ;;             ;; when starting an Erlang shell in Emacs, with a custom node name
      ;;             (setq inferior-erlang-machine-options '("-sname" "syl20bnr"))
      ;;             ))
      (setq erlang-compile-extra-opts '(debug_info)))
    :config
    (require 'erlang-start)))

(defun erlang/post-init-flycheck ()
  (add-hook 'erlang-mode-hook 'flycheck-mode))
