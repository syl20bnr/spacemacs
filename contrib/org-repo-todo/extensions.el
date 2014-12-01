(defvar org-repo-todo-post-extensions '(org-repo-todo))

(defun org-repo-todo/init-org-repo-todo ()
  "Initialize org-repo-todo"
  (use-package org-repo-todo
    :commands (ort/capture-todo
               ort/capture-todo-check
               ort/goto-todos)
    :init
    (progn
      (evil-leader/set-key-for-mode 'org-mode
        "mct"  'ort/capture-todo
        "mcc"  'ort/capture-todo-check
        "mgt"  'ort/goto-todos
        ))))
