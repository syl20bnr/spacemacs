;;; packages.el --- copy-as-format layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Ruslan Kamashev <rynffoll@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
