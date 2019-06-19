;;; packages.el --- Dotnet Layer packages File for Spacemacs
;;
;; Author: Jordan Kaye <jordan.kaye2@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq dotnet-packages
      '(
        dotnet
        ))

(defun dotnet/init-dotnet ()
  (use-package dotnet
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'fsharp-mode
      "dap" 'dotnet-add-package
      "dar" 'dotnet-add-reference
      "db"  'dotnet-build
      "dc"  'dotnet-clean
      "dn"  'dotnet-new
      "dp"  'dotnet-publish
      "dra" 'dotnet-run-with-args
      "drr" 'dotnet-run
      "drs" 'dotnet-restore
      "dsa" 'dotnet-sln-add
      "dsl" 'dotnet-sln-list
      "dsn" 'dotnet-sln-new
      "dsr" 'dotnet-sln-remove
      "dt"  'dotnet-test)))
