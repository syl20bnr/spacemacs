;;; config.el --- html layer configuration file for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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

(defvar html-enable-leex-support nil
  "If non nil, enable support for `.leex' files.")
