#!/usr/bin/emacs --script
;;; install.el --- pdf-tools layer dependencies installation script
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

(install libpng-dev libz-dev libpoppler-glib-dev
         libpoppler-private-dev imagemagick)
(with-installed (curl git autotools-dev gcc g++ make automake autoconf python)
  (with-build-dir (tpdft "/tmp/tpdft/")
    ($ '("su - ${UNAME} -c 'curl -fsSL %s | python'"
         "https://raw.githubusercontent.com/cask/cask/master/go"))
    (add-glob-paths (format "%s.cask/bin" (dir $UHOME)))
    ($ "git clone https://github.com/politza/pdf-tools.git ."
       "make -s"
       "tar -xf pdf-tools-*.tar"
       `("cp ./pdf-tools-*/epdfinfo %s"
         ,($ ["find \"${UHOME}/.emacs.d/elpa/\""
              "-name pdf-tools* -type d -print -quit"])))))
