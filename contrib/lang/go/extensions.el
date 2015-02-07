(defvar go-pre-extensions
  '(
    ;; pre extension gos go here
    )
  "List of all extensions to load before the packages.")

(defvar go-post-extensions
  '(
    ;; post extension gos go here
    go-oracle
    go-rename
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function go/init-<extension-go>
;;
;; (defun go/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun go/init-go-oracle()
  (load-file "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
  (add-hook 'go-mode-hook 'go-oracle-mode)
  (spacemacs|diminish go-oracle-mode " O")
  (evil-leader/set-key-for-mode 'go-mode
    "moo" 'go-oracle-set-scope
    "mo<" 'go-oracle-callers
    "mo>" 'go-oracle-callees
    "moc" 'go-oracle-peers
    "mod" 'go-oracle-definition
    "mof" 'go-oracle-freevars
    "mog" 'go-oracle-callgraph
    "moi" 'go-oracle-implements
    "mop" 'go-oracle-pointsto
    "mor" 'go-oracle-referrers
    "mos" 'go-oracle-callstack
    "mot" 'go-oracle-describe
  ))

(defun go/init-go-rename()
;;; Copyright 2014 The Go Authors. All rights reserved.
;;; Use of this source code is governed by a BSD-style
;;; license that can be found in the LICENSE file.
;;;
;;; Integration of the 'gorename' tool into Emacs.
;;;
;;; To install:
;;; % go get golang.org/x/tools/cmd/gorename
;;; % go build golang.org/x/tools/cmd/gorename
;;; % mv gorename $HOME/bin/         # or elsewhere on $PATH
;;;
;;; The go-rename-command variable can be customized to specify an
;;; alternative location for the installed command.
  (defgroup go-rename nil
    "Options specific to the Go rename."
    :group 'go)

  (defcustom go-rename-command "gorename"
    "The `gorename' command; by the default, $PATH is searched."
    :type 'string
    :group 'go-rename)

  (evil-leader/set-key-for-mode 'go-mode "mr" 'go-rename)
  )
