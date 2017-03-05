#!/usr/bin/emacs --script
;;; install.el --- python layer dependencies installation script
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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

(defconst anaconda-version "0.1.7")

(install python-pip)
($ "pip install --upgrade pip")

($ ["pip install --upgrade"
    "json-rpc>=1.8.1"
    "service_factory>=0.1.5"
    "autoflake"
    "isort"
    "hy"
    "nose"
    "yapf"
    "pylint"
    "pytest"
    "flake8"])

;; https://github.com/proofit404/anaconda-mode/issues/225#issuecomment-280009142
(with-build-dir (tanacond "/tmp/anaconda_td/")
  (with-installed (git)
    (let ((anaconda-dir (format "%s/.emacs.d/.cache/anaconda-mode/%s"
                                $UHOME
                                anaconda-version)))
      (mkdirp anaconda-dir)
      ($ "git clone https://github.com/davidhalter/jedi ."
         "git checkout v0.10.0"
         "python setup.py sdist"
         (s-format (concat "cd ./dist && "
                           "PYTHONPATH=$0 easy_install -d $0 -S $0 "
                           "-a -Z jedi-0.10.0.tar.gz")
                   'elt
                   (l anaconda-dir))
         "chown $UNAME:$GNAME -R $UHOME/.emacs.d/.cache"))))
