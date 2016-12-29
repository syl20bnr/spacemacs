(defun spacemacs/debug-enable ()
  (interactive)
  (let ((current-prefix-arg t))
    (if (not (equal major-mode 'gud-mode))
        (call-interactively
         (cl-case debug-mode
           (gdb (gdb
                 (read-string
                  (concat"Override '" debug-prog " " debug-target "' >: ") nil nil (concat debug-prog " " debug-target) nil))))))))
