;;; core-format.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-format//format-goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun spacemacs-format//format-apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in spacemacs-format//format-apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (spacemacs-format//format-goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (let ((beg (point)))
                  (forward-line len)
                  (delete-region (point) beg))))
             (t
              (error "Invalid rcs patch or internal error in spacemacs-format//format-apply-rcs-patch")))))))))

(defun spacemacs-format//format-process-errors (errorfile errbuf)
  "Process errors using an ERRORFILE and display the output in ERRBUF."
  (with-current-buffer errbuf
    (if (eq dotspacemacs-format-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (spacemacs-format//kill-error-buffer errbuf))
      (insert-file-contents errorfile nil nil nil)
      (compilation-mode)
      (display-buffer errbuf))))

(defun spacemacs-format//kill-error-buffer (errbuf)
  "Kill buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))

(defun spacemacs-format//format-buffer (name program args)
  (let* ((ext (file-name-extension buffer-file-name t))
         (bufferfile (make-temp-file name nil ext))
         (outputfile (make-temp-file name nil ext))
         (errorfile (make-temp-file name nil ext))
         (errbuf (when dotspacemacs-format-show-errors
                   (get-buffer-create (format "*%s errors*" name))))
         (patchbuf (get-buffer-create (format "*%s patch*" name)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (unwind-protect
        (save-restriction
          (widen)
          (write-region nil nil bufferfile)
          (when errbuf
            (with-current-buffer errbuf
              (setq buffer-read-only nil)
              (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))
          (if (zerop (apply 'call-process program bufferfile
                            (list (list :file outputfile) errorfile)
                            nil args))
              (progn
                (call-process-region (point-min) (point-max)
                                     "diff" nil patchbuf nil
                                     "-n" "--strip-trailing-cr" "-" outputfile)
                (spacemacs-format//format-apply-rcs-patch patchbuf)
                (message "Applied %s" name)
                (when errbuf (spacemacs-format//kill-error-buffer errbuf)))
            (message "Could not apply %s" name)
            (when errbuf
              (spacemacs-format//format-process-errors errorfile errbuf))))
      (kill-buffer patchbuf)
      (delete-file errorfile)
      (delete-file bufferfile)
      (delete-file outputfile))))

(defmacro spacemacs|define-fmt-tool (&rest props)
  "Generate an interactive format command `spacemacs/xxx-format' where xxx is ID.

Available PROPS:

`:id ID'
A symbol naming the formatter function. Must be started with the layer name.

`:name NAME'
The formatter name.

`:program PROGRAM'
A literal string which holds the program path.

`:args ARGS'
A list of arguments or a function which evaluates to a list of arguments."
  (let* ((id      (spacemacs/mplist-get-value props :id))
         (name    (spacemacs/mplist-get-value props :name))
         (program (spacemacs/mplist-get-value props :program))
         (args    (spacemacs/mplist-get-value props :args))
         (format-func-name (intern (format "spacemacs/%S-format" id))))
    ;; define format function
    `(defun ,format-func-name ()
       (interactive)
       (let* (,(if (functionp args) `(args (,args)) `(args ',args)))
         (spacemacs-format//format-buffer ,name ,program args)))))

(provide 'core-format)
