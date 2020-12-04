;;; packages.el --- phoenix layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Lyuben Petrov <lyuben.y.petrov@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst phoenix-packages '(alchemist))

(defun phoenix/post-init-alchemist ()
  (progn
    (space-macs/declare-prefix-for-mode 'elixir-mode "mf" "phoenix")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mff" "find")
    (space-macs/set-leader-keys-for-major-mode 'elixir-mode
      "ffw" 'alchemist-phoenix-find-web
      "ffv" 'alchemist-phoenix-find-views
      "ffc" 'alchemist-phoenix-find-controllers
      "ffC" 'alchemist-phoenix-find-channels
      "fft" 'alchemist-phoenix-find-templates
      "ffm" 'alchemist-phoenix-find-models
      "ffs" 'alchemist-phoenix-find-static
      "ffr" 'alchemist-phoenix-router
      "fr" 'alchemist-phoenix-routes)))


