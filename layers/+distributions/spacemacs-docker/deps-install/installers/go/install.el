#!/usr/bin/emacs --script
;;; install.el --- go layer dependencies installation script
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

(let ((go-version "1.10"))
  (with-build-dir (tgp "/tmp/gopath")
    (with-installed (tar gzip wget)
      ($ `("wget https://storage.googleapis.com/golang/go%s.linux-amd64.tar.gz"
           ,go-version)
         `("tar -xf go%s.linux-amd64.tar.gz"
           ,go-version)
         "mv go /usr/local"))
    (set-glob-envs '("GOROOT" . "/usr/local/go")
                   '("GOBIN" . "/usr/local/go/bin")
                   `("GOPATH" . ,(get-glob-env
                                  "WORKSPACE")))
    (add-glob-paths "/usr/local/go/bin"
                    (format "%s/bin"
                            (get-glob-env "WORKSPACE")))
    (if (not (string-match-p (format "^.*go%s.*$"
                                     go-version)
                             ($ "go version")))
        (progn (setq silient nil)
               (! (v "Golang installation failed!\n"
                     (l "Expected verison %s but got %s"
                        go-version
                        ($ "go version")))))
      (! "Building Golang tools...")
      (setenv "GOPATH" tgp)
      (setenv "PATH" (format "PATH=%s:%s/bin"
                             (getenv "PATH")
                             tgp))
      ($ ["go get -u -buildmode=exe"
          "gopkg.in/check.v1"
          "github.com/nsf/gocode"
          "github.com/rogpeppe/godef"
          "golang.org/x/tools/cmd/guru"
          "golang.org/x/tools/cmd/gorename"
          "golang.org/x/tools/cmd/goimports"])
      (when (get-config 'go-use-gometalinter)
        (! "Gometalinter used. Installing...")
        ($ ["go get -u -buildmode=exe"
            "github.com/alecthomas/gometalinter"]
           "gometalinter --install")))))
