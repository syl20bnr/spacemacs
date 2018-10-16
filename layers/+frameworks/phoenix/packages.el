;;; packages.el --- phoenix layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mf" "phoenix")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mff" "find")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "ffw" 'alchemist-phoenix-find-web
      "ffv" 'alchemist-phoenix-find-views
      "ffc" 'alchemist-phoenix-find-controllers
      "ffC" 'alchemist-phoenix-find-channels
      "fft" 'alchemist-phoenix-find-templates
      "ffm" 'alchemist-phoenix-find-models
      "ffs" 'alchemist-phoenix-find-static
      "ffr" 'alchemist-phoenix-router
      "fr" 'alchemist-phoenix-routes)))
