#!/usr/bin/emacs --script
;;; install.el --- TypeScript layer dependencies installation script
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

(with-installed (curl software-properties-common bash)
  ($ "curl -sL https://nsolid-deb.nodesource.com/nsolid_setup_3.x | bash -")
  (install nsolid-carbon nsolid-console)
  ($ "npm install -g typescript tslint typescript-formatter"
     "npm install httpd-node"))
