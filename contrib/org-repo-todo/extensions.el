(defvar org-repo-todo-post-extensions '(org-repo-todo))

(defun org-repo-todo/init-org-repo-todo ()
  "Initialize org-repo-todo"
  (use-package org-repo-todo
    :commands (ort/capture-todo
               ort/capture-todo-check
               ort/goto-todos)
    :init
    (evil-leader/set-key
      "oct"  'ort/capture-todo
      "occ"  'ort/capture-todo-check
      "ogt"  'ort/goto-todos
      )))
