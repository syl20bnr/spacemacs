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


(defun load-gopath-file(name)
  (setq paths
    (split-string
      (substitute-in-file-name "$GOPATH")
      ":"
      )
    )
  (loop for p in paths
    for file = (concat p name)
    when (file-exists-p file)
    do (load-file file)
    )
  )

(defun go/init-go-oracle()
  (load-gopath-file "/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
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
  (use-package go-rename
    :init
    (evil-leader/set-key-for-mode 'go-mode "mr" 'go-rename)))
