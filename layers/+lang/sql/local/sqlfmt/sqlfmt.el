(defcustom sqlfmt-executable
  "sqlfmt"
  "Location of sqlfmt executable."
  :type 'string)

(defcustom sqlfmt-options
  '("-u")
  "Command line options to pass to sqlfmt."
  :type '(repeat string))

(defun sqlfmt-buffer ()
  (interactive)
  (let* ((orig-buffer (current-buffer))
         (orig-point (point))
         (tmpbuf (generate-new-buffer "*sqlfmt*"))
         (status-code (apply #'call-process-region (point-min) (point-max)
                             sqlfmt-executable nil tmpbuf nil
                             sqlfmt-options)))
    (deactivate-mark)
    (with-current-buffer tmpbuf
      (setq buffer-read-only t))
    (if (eq status-code 0)
        (progn
          (with-current-buffer tmpbuf
            (copy-to-buffer orig-buffer (point-min) (point-max)))
          (kill-buffer tmpbuf)
          (goto-char orig-point))
      (error "sqlfmt failed, see %s buffer for details." (buffer-name tmpbuf)))))
