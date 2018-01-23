(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))
