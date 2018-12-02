;;; packages.el --- phoenix layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mykhailo Panarin <mykhailopanarin@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/phoenix-server-start ()
  "Start phoenix server with `mix'"
  (interactive)
  (alchemist-mix-execute "phoenix.server"))

(defun spacemacs/phoenix-server-iex ()
  "Start phoenix server in `iex', to allow prying"
  (interactive)
  (if (alchemist-project-p)
      (let ((default-directory (alchemist-project-root)))
        (pop-to-buffer (process-buffer (alchemist-iex-process " -S mix phoenix.server"))))
    (message "No mix.exs file available. Please use `alchemist-iex-run' instead.")))
