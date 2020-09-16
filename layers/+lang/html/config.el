;;; config.el --- html layer configuration file for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers css-mode)
(spacemacs|define-jump-handlers less-css-mode)
(spacemacs|define-jump-handlers scss-mode)
(spacemacs|define-jump-handlers web-mode)

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
