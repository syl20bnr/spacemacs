;;; packages.el --- copy-as-format layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Ruslan Kamashev <rynffoll@gmail.com>
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


(defconst copy-as-format-packages '(copy-as-format))

(defun copy-as-format/init-copy-as-format ()
  (use-package copy-as-format
    :defer t
    :init
    (spacemacs/declare-prefix "xf" "copy-as-format")
    (spacemacs/set-leader-keys
      "xff" 'copy-as-format
      "xfa" 'copy-as-format-asciidoc
      "xfb" 'copy-as-format-bitbucket
      "xfd" 'copy-as-format-disqus
      "xfg" 'copy-as-format-github
      "xfl" 'copy-as-format-gitlab
      "xfc" 'copy-as-format-hipchat
      "xfh" 'copy-as-format-html
      "xfj" 'copy-as-format-jira
      "xfm" 'copy-as-format-markdown
      "xfw" 'copy-as-format-mediawiki
      "xfo" 'copy-as-format-org-mode
      "xfp" 'copy-as-format-pod
      "xfr" 'copy-as-format-rst
      "xfs" 'copy-as-format-slack)))
