;;; auto-layer.el --- auto-mode-alist entries for layer installation
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(configuration-layer/lazy-install 'elixir
  :extensions '("\\.\\(ex\\|exs\\|elixir\\)\\'" elixir-mode))
(configuration-layer/lazy-install 'nginx
  :extensions '("\\(nginx\\.conf\\'\\|/nginx/.+\\.conf\\'\\)" nginx-mode))
