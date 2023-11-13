;;; polymode-base.el --- Root Host and Polymode Configuration Objects -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013-2022  Free Software Foundation, Inc.
;; Author: Vitalie Spinu
;; URL: https://github.com/polymode/polymode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;; Code:

(require 'polymode-core)


;; HOST MODES

(define-obsolete-variable-alias 'pm-host/ada 'poly-ada-hostmode "v0.2")
(define-hostmode poly-ada-hostmode :mode 'ada-mode)

(define-obsolete-variable-alias 'pm-host/coffee 'poly-coffee-hostmode "v0.2")
(define-hostmode poly-coffee-hostmode :mode 'coffee-mode)

(define-obsolete-variable-alias 'pm-host/emacs-lisp 'poly-emacs-lisp-hostmode "v0.2")
(define-hostmode poly-emacs-lisp-hostmode :mode 'emacs-lisp-mode)

(define-obsolete-variable-alias 'pm-host/fundamental 'poly-fundamental-hostmode "v0.2")
(define-hostmode poly-fundamental-hostmode :mode 'fundamental-mode)

(define-obsolete-variable-alias 'pm-host/java 'poly-java-hostmode "v0.2")
(define-hostmode poly-java-hostmode :mode 'java-mode)

(define-obsolete-variable-alias 'pm-host/js 'poly-js-hostmode "v0.2")
(define-hostmode poly-js-hostmode :mode 'js-mode)

(define-obsolete-variable-alias 'pm-host/latex 'poly-latex-hostmode "v0.2")
(define-hostmode poly-latex-hostmode :mode 'latex-mode)

(define-obsolete-variable-alias 'pm-host/html 'poly-html-hostmode "v0.2")
(define-hostmode poly-html-hostmode
  :mode 'html-mode
  :indent-offset 'sgml-basic-offset
  :protect-font-lock nil
  :protect-syntax t)

(define-obsolete-variable-alias 'pm-host/R 'poly-R-hostmode "v0.2")
(define-hostmode poly-R-hostmode :mode 'R-mode)

(define-obsolete-variable-alias 'pm-host/perl 'poly-perl-hostmode "v0.2")
(define-hostmode poly-perl-hostmode :mode 'perl-mode)

(define-obsolete-variable-alias 'pm-host/ruby 'poly-ruby-hostmode "v0.2")
(define-hostmode poly-ruby-hostmode :mode 'ruby-mode)

(define-obsolete-variable-alias 'pm-host/pascal 'poly-pascal-hostmode "v0.2")
(define-hostmode poly-pascal-hostmode :mode 'pascal-mode)

(define-obsolete-variable-alias 'pm-host/C++ 'poly-c++-hostmode "v0.2")
(define-hostmode poly-c++-hostmode :mode 'C++-mode :protect-font-lock nil)

(define-obsolete-variable-alias 'pm-host/sgml 'poly-sgml-hostmode "v0.2")
(define-hostmode poly-sgml-hostmode :mode 'sgml-mode)

(define-obsolete-variable-alias 'pm-host/text 'poly-text-hostmode "v0.2")
(define-hostmode poly-text-hostmode :mode 'text-mode)

(define-obsolete-variable-alias 'pm-host/yaml 'poly-yaml-hostmode "v0.2")
(define-hostmode poly-yaml-hostmode :mode 'yaml-mode)


;;; ROOT POLYMODES

;; These are simple generic configuration objects. More specialized polymodes
;; should clone these.

(define-obsolete-variable-alias 'pm-poly/brew 'poly-brew-root-polymode "v0.2")
(defvar poly-brew-root-polymode
  (pm-polymode :name "brew-root" :hostmode 'poly-text-hostmode)
  "Brew root configuration.")

(define-obsolete-variable-alias 'pm-poly/html 'poly-html-root-polymode "v0.2")
(defvar poly-html-root-polymode
  (pm-polymode :name "html-root" :hostmode 'poly-html-hostmode)
  "HTML root configuration.")

(define-obsolete-variable-alias 'pm-poly/C++ 'poly-c++-root-polymode "v0.2")
(defvar poly-c++-root-polymode
  (pm-polymode :name "c++-root" :hostmode 'poly-c++-hostmode)
  "C++ root configuration.")

(define-obsolete-variable-alias 'pm-poly/latex 'poly-latex-root-polymode "v0.2")
(defvar poly-latex-root-polymode
  (pm-polymode :name "latex-root" :hostmode 'poly-latex-hostmode)
  "LaTeX root configuration.")

(defvar poly-js-root-polymode
  (pm-polymode :name "js-root" :hostmode 'poly-js-hostmode)
  "JS root polymode.")

(defvar poly-coffee-root-polymode
  (pm-polymode :name "coffee-root" :hostmode 'poly-coffee-hostmode)
  "JS root polymode.")

(provide 'polymode-base)
;;; polymode-base.el ends here
