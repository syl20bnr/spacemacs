(setq go-pre-extensions
  '(
    ;; pre extension gos go here
    ))

(setq go-post-extensions
  '(
    ;; post extension gos go here
    go-oracle
    go-rename
    ))

;; For each extension, define a function go/init-<extension-go>
;;
;; (defun go/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

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

(defun go/init-go-oracle()
  (let ((go-path (getenv "GOPATH")))
    (if (not go-path)
        (spacemacs-buffer/warning
         "GOPATH variable not found, go-oracle configuration skipped.")
      (when (load-gopath-file
             go-path "/src/golang.org/x/tools/cmd/oracle/oracle.el")
        (evil-leader/set-key-for-mode 'go-mode
          "mro" 'go-oracle-set-scope
          "mr<" 'go-oracle-callers
          "mr>" 'go-oracle-callees
          "mrc" 'go-oracle-peers
          "mrd" 'go-oracle-definition
          "mrf" 'go-oracle-freevars
          "mrg" 'go-oracle-callgraph
          "mri" 'go-oracle-implements
          "mrp" 'go-oracle-pointsto
          "mrr" 'go-oracle-referrers
          "mrs" 'go-oracle-callstack
          "mrt" 'go-oracle-describe)))))

(defun go/init-go-rename()
  (use-package go-rename
    :init
    (evil-leader/set-key-for-mode 'go-mode "mrn" 'go-rename)))
