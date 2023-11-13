;;; forge-semi.el --- Support for semi-forges  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'forge)

;;; Classes

(defclass forge-gitweb-repository (forge-noapi-repository)
  ((commit-url-format :initform "https://%h/gitweb/?p=%P.git;a=commitdiff;h=%r")
   (branch-url-format :initform "https://%h/gitweb/?p=%P.git;a=log;h=refs/heads/%r")
   (remote-url-format :initform "https://%h/gitweb/?p=%P.git;a=summary"))
  "Gitweb from https://git-scm.com/docs/gitweb.")

(defclass forge-cgit-repository (forge-noapi-repository)
  ((commit-url-format :initform "https://%h/%p.git/commit/?id=%r")
   (branch-url-format :initform "https://%h/%p.git/log/?h=%r")
   (remote-url-format :initform "https://%h/%p.git/about"))
  "Cgit from https://git.zx2c4.com/cgit/about.
Different hosts use different url schemata, so we need multiple
classes.  See their definitions in \"forge-semi.el\".")

(defclass forge-cgit*-repository (forge-cgit-repository)
  ((commit-url-format :initform "https://%h/cgit/%p.git/commit/?id=%r")
   (branch-url-format :initform "https://%h/cgit/%p.git/log/?h=%r")
   (remote-url-format :initform "https://%h/cgit/%p.git/about"))
  "Cgit from https://git.zx2c4.com/cgit/about.
Different hosts use different url schemata, so we need multiple
classes.  See their definitions in \"forge-semi.el\".")

(defclass forge-cgit**-repository (forge-cgit-repository)
  ((commit-url-format :initform "https://%h/cgit/%n.git/commit/?id=%r")
   (branch-url-format :initform "https://%h/cgit/%n.git/log/?h=%r")
   (remote-url-format :initform "https://%h/cgit/%n.git/about"))
  "Cgit from https://git.zx2c4.com/cgit/about.
Different hosts use different url schemata, so we need multiple
classes.  See their definitions in \"forge-semi.el\".")

(defclass forge-repoorcz-repository (forge-cgit-repository)
  ((commit-url-format :initform "https://%h/%p.git/commit/%r")
   (branch-url-format :initform "https://%h/%p.git/log/%r")
   (remote-url-format :initform "https://%h/%p.git"))
  "Cgit fork used on https://repo.or.cz/cgit.git.
Different hosts use different url schemata, so we need multiple
classes.  See their definitions in \"forge-semi.el\".")

(defclass forge-stagit-repository (forge-noapi-repository)
  ((commit-url-format :initform "https://%h/%n/commit/%r.html")
   (branch-url-format :initform "https://%h/%n/refs.html")
   (remote-url-format :initform "https://%h/%n/file/README.html"))
  "Stagit from https://codemadness.org/git/stagit/file/README.html.
Only the history of \"master\" can be shown, so this links to the
list of refs instead of the log of the specified branch.")

(defclass forge-srht-repository (forge-noapi-repository)
  ((commit-url-format :initform "https://%h/~%o/%n/commit/%r")
   (branch-url-format :initform "https://%h/~%o/%n/log/%r")
   (remote-url-format :initform "https://%h/~%o/%n"))
  "See https://meta.sr.ht.")

;;; _
(provide 'forge-semi)
;;; forge-semi.el ends here
