(load-file  "./doc-fmt/toc-org.el")
(defun toc-apply ()
  (toc-org-insert-toc)
  (save-buffer 0))
