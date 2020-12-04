;;; layers.el --- C/C++ Layer declarations File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(when (fboundp 'space-macs//c-c++-backend)
  (pcase (space-macs//c-c++-backend)
    (`lsp-clangd (configuration-layer/declare-layer-dependencies '(lsp dap)))
    (`lsp-ccls (configuration-layer/declare-layer-dependencies '(lsp dap)))
    (`rtags (configuration-layer/declare-layer-dependencies '(ggtags)))
    (`ycmd (configuration-layer/declare-layer-dependencies '(ycmd)))))


