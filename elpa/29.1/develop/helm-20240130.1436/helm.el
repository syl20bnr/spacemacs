;;; helm.el --- Helm is an Emacs incremental and narrowing framework  -*- lexical-binding: t -*-

;; Copyright (C) 2007         Tamas Patrovics
;;               2008 ~ 2011  rubikitch <rubikitch@ruby-lang.org>
;;               2011 ~ 2023  Thierry Volpiatto 

;; This is a fork of anything.el wrote by Tamas Patrovics.

;; Authors of anything.el: Tamas Patrovics
;;                         rubikitch <rubikitch@ruby-lang.org>
;;                         Thierry Volpiatto

;; Author: Thierry Volpiatto <thievol@posteo.net>
;; Version: 3.9.7
;; URL: https://emacs-helm.github.io/helm/
;; Package-Requires: ((helm-core "3.9.7") (wfnames "1.1") (popup "0.5.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is just a wrapper for helm-core.el and a place holder we
;; currently use only to hold the package's metadata in the header.

;;; Code:

(require 'helm-core)
(require 'helm-global-bindings)

;; Build info sources and commands once called (bug #2608). We need to autoload
;; only these commands which are bound in helm-global-bindings, if we add more
;; helm-info* commands to helm-global-bindings we will have to autoload them
;; here. Requiring helm-info here instead will make recursive require to helm so
;; don't do that.
(autoload 'helm-info-emacs "helm-info" nil t)
(autoload 'helm-info-gnus "helm-info" nil t)
(autoload 'helm-info-at-point "helm-info" nil t)

(provide 'helm)

;;; helm.el ends here
