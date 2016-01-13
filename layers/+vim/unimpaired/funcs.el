;; funcs.el file for unimpaired contribution layer

(defun unimpaired//find-relative-filename (offset)
  (when buffer-file-name
    (let* ((directory (f-dirname buffer-file-name))
           (files (f--files directory (not (s-matches? "^\\.?#" it))))
           (index (+ (-elem-index buffer-file-name files) offset))
           (file (and (>= index 0) (nth index files))))
      (when file
        (f-expand file directory)))))

(defun unimpaired/previous-file ()
  (interactive)
  (-if-let (filename (unimpaired//find-relative-filename -1))
      (find-file filename)
    (user-error "No previous file")))

(defun unimpaired/next-file ()
  (interactive)
  (-if-let (filename (unimpaired//find-relative-filename 1))
      (find-file filename)
    (user-error "No next file")))

(defun unimpaired/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun unimpaired/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))
