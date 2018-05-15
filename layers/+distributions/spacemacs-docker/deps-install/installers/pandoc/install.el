#!/usr/bin/emacs --script
;;; install.el --- pandoc layer dependencies installation script
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(load (expand-file-name "../../lib/deps-install-helpers.el"
                        (file-name-directory
                         load-file-name)) nil t)

(let* ((pandoc-version "2.2.1")
       (pandoc-url (concat "https://github.com/jgm/pandoc/releases/download/"
                           (format "%s/pandoc-%s-1-amd64.deb"
                                   pandoc-version
                                   pandoc-version))))
  (with-build-dir (tpd "/tmp/pandoc")
    (with-installed (wget)
      ($ `("wget %s -O pandoc.deb" ,pandoc-url)
         "dpkg -i ./pandoc.deb"
         "apt-get install -f"))))
