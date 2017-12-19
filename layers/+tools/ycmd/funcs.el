(defun ycmd/manual-semantic-company-completer ()
  "A useful function that can be bound, if users prefer to trigger company
completion manually"

  (interactive)
  (company-cancel)
  (let ((ycmd-force-semantic-completion (not (company-ycmd--in-include))))
    (setq company-backend 'company-ycmd)
    (company-manual-begin)))
