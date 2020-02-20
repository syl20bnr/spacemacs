(defgroup sqlfmt nil
  "Easy sqlfmt invocation in Emacs"
  :group 'sql)

(defcustom sqlfmt-executable
  "sqlfmt"
  "Location of sqlfmt executable."
  :type 'string)

(defcustom sqlfmt-options
  '("--use-spaces")
  "Command line options to pass to sqlfmt."
  :type '(repeat string))

(defcustom sqlfmt-reuse-error-buffer
  t
  "Reuse the same buffer for sqlfmt errors, replacing content on new invocations, or generate new buffers on each invocation"
  :type 'boolean)

(defun sqlfmt-buffer ()
  (interactive)
  (sqlfmt-region (point-min) (point-max)))

(defun sqlfmt-region (start end)
  "Calls sqlfmt on region"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (not (and start end))
      (error "No region active, sqlfmt cancelled"))
  (let* ((sqlfmt-buffer-base "*sqlfmt*")
         (inhibit-read-only t)
         (orig-buffer (current-buffer))
         (orig-point (point))
         (tmpbuf (if sqlfmt-reuse-error-buffer
                     (get-buffer-create sqlfmt-buffer-base)
                   (generate-new-buffer sqlfmt-buffer-base)))
         (status-code (progn
                        (with-current-buffer tmpbuf
                          (erase-buffer)
                          (setq buffer-read-only t))
                        (apply #'call-process-region start end
                               sqlfmt-executable nil tmpbuf nil
                               sqlfmt-options))))
    (deactivate-mark)
    (if (eq status-code 0)
        (progn
          (with-current-buffer orig-buffer
            (delete-region start end)
            (insert-buffer tmpbuf)
            (kill-buffer tmpbuf)
            (goto-char orig-point))
          (message "sqlfmt applied"))
      (error "sqlfmt failed, see %s buffer for details." (buffer-name tmpbuf)))))
