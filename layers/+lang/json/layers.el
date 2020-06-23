;;; layers.el --- JSON Layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layer-dependencies '(node prettier web-beautify))

(when (and (boundp 'json-backend)
           (eq json-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
