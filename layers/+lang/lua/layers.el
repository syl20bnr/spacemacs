;;; layers.el --- Lua Layer declarations File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Lin Sun <sunlin7@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and (boundp 'lua-backend)
           (eq lua-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
