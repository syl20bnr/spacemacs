;;; config.el --- html layer configuration file for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(space-macs|define-jump-handlers css-mode)
(space-macs|define-jump-handlers less-css-mode)
(space-macs|define-jump-handlers scss-mode)
(space-macs|define-jump-handlers web-mode)

(defvar web-fmt-tool 'web-beautify
  "The formatter to format a CSS/SCSS/Less file. Possible values are `web-beautify' and `prettier'.")

(defvar css-enable-lsp nil
  "If non-nil, enable lsp-mode in css-mode buffers.")

(defvar less-enable-lsp nil
  "If non-nil, enable lsp-mode in less-css-mode buffers.")

(defvar scss-enable-lsp nil
  "If non-nil, enable lsp-mode in scss-mode buffers.")

(defvar html-enable-lsp nil
  "If non-nil, enable lsp-mode in web-mode html buffers having.")


