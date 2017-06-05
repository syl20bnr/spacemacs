
(defun persistent-scratch-file-match-regexp-p ()
  "Return non-nil if the current buffer's name is *scratch* or untitled."
  (string-match "*scratch.*\\|untitled.*" (buffer-name)))

(defun persistent-scratch-persistent-file-match-regexp-p ()
  "Return non-nil if the current buffer's name is *scratch*."
  (string-match "*scratch.*" (buffer-name)))

(defun filter-scratch-buffers (buffer-list)
  "Filter all buffers that are not recognized as scratch buffers."
  (delq nil (mapcar
             (lambda (buffer)
               (if (with-current-buffer
                       buffer
                     (persistent-scratch-file-match-regexp-p))
                   buffer nil))
             buffer-list)))
