(defvar org-repo-todo-pre-extensions
  '(
    ;; pre extension org-repo-todos go here
    org-repo-todo
    )
  "List of all extensions to load before the packages.")

(defvar org-repo-todo-post-extensions
  '(
    ;; post extension org-repo-todos go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function org-repo-todo/init-<extension-org-repo-todo>
;;
(defun org-repo-todo/init-org-repo-todo ()
  "Initialize my extension"
  (use-package org-repo-todo
    :commands (ort/capture-todo
               ort/capture-todo-check
               ort/goto-todos)
    :load-path "contrib/org-repo-todo/org-repo-todo"   
    :init
    (evil-leader/set-key
      "oct"  'ort/capture-todo
      "occ"  'ort/capture-todo-check
      "ogt"  'ort/goto-todos
      )))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
