(defun spacemacs/get-parent-dir ()
  (car (cdr ; Last item
        (reverse
         (split-string
          (file-name-sans-extension (buffer-file-name))
          "/")))))
