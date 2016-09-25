;;; packages.el --- phoenix layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Lyuben Petrov <lyuben.y.petrov@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst phoenix-packages
  '(alchemist))

(defun phoenix/post-init-alchemist ()
  (progn
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mp" "phoenix")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mpf" "find")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "pfw" 'alchemist-phoenix-find-web
      "pfv" 'alchemist-phoenix-find-views
      "pfc" 'alchemist-phoenix-find-controllers
      "pfC" 'alchemist-phoenix-find-channels
      "pft" 'alchemist-phoenix-find-templates
      "pfm" 'alchemist-phoenix-find-models
      "pfs" 'alchemist-phoenix-find-static
      "pfr" 'alchemist-phoenix-router
      "pr" 'alchemist-phoenix-routes)))
