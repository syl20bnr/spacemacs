;;; funcs.el --- Go Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun load-gopath-file(gopath name)
  "Search for NAME file in all paths referenced in GOPATH."
  (let* ((sep (if (spacemacs/system-is-mswindows) ";" ":"))
         (paths (split-string gopath sep))
         found)
    (loop for p in paths
          for file = (concat p name) when (file-exists-p file)
          do
          (load-file file)
          (setq found t)
          finally return found)))

(defun spacemacs//go-enable-gometalinter ()
   "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
   (setq flycheck-disabled-checkers '(go-gofmt
                                      go-golint
                                      go-vet
                                      go-build
                                      go-test
                                      go-errcheck))
   (flycheck-gometalinter-setup))
