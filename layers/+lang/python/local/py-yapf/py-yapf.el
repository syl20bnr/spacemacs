;;; py-yapf.el --- Use yapf to beautify a Python buffer

;; Copyright (C) 2015, Friedrich Paetzke <paetzke@fastmail.fm>

;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: https://github.com/paetzke/py-yapf.el
;; Version: 0.1

;; Licensed under GPLv3
;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Provides commands, which use the external "yapf"
;; tool to tidy up the current buffer according to Python's PEP8.

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;;; Code:

(defgroup py-yapf nil
  "Use yapf to beautify a Python buffer."
  :group 'convenience
  :prefix "py-yapf-")


(defcustom py-yapf-options nil
  "Options used for yapf.

Note that `--in-place' is used by default."
  :group 'py-yapf
  :type '(repeat (string :tag "option")))


(defun py-yapf-apply-rcs-patch (patch-buffer)
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
            (error "Invalid rcs patch or internal error in py-yapf-apply-rcs-patch"))
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
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in py-yapf-apply-rcs-patch")))))))))


(defun py-yapf ()
  "Formats the current buffer according to the yapf tool."
  (when (not (executable-find "yapf"))
    (error "\"yapf\" command not found.  Install yapf with \"pip install yapf\""))
  (let ((tmpfile (make-temp-file "yapf" nil ".py"))
        (patchbuf (get-buffer-create "*yapf patch*"))
        (errbuf (get-buffer-create "*yapf Errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))
    (write-region nil nil tmpfile)
    (let ((yapf-ret-code (apply 'call-process "yapf" nil errbuf nil
                                       (append py-yapf-options `("--in-place" ,tmpfile)))))
      (cond
       ((eq yapf-ret-code 2)
        (py-yapf-apply-rcs-patch-if-needed patchbuf tmpfile errbuf)
        )
       ((eq yapf-ret-code 0)
        (py-yapf-apply-rcs-patch-if-needed patchbuf tmpfile errbuf)
        )
       (error "Could not apply yapf. Check *yapf Errors* for details"))
      )
    (kill-buffer patchbuf)
    (delete-file tmpfile)
    ))

(defun py-yapf-apply-rcs-patch-if-needed (patchbuf tmpfile errbuf)
  "Create and apply patch to current buffer from yapfed tmpfile"
  (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
      (progn
        (kill-buffer errbuf)
        (message "Buffer is already yapfed"))
    (progn
      (py-yapf-apply-rcs-patch patchbuf)
      (kill-buffer errbuf)
      (message "Applied yapf")
      )
    )
  )

;;;###autoload
(defun py-yapf-buffer ()
  "Uses the \"yapf\" tool to reformat the current buffer."
  (interactive)
  (py-yapf))


;;;###autoload
(defun py-yapf-enable-on-save ()
  "Pre-save hooked to be used before running py-yapf."
  (interactive)
  (add-hook 'before-save-hook 'py-yapf-buffer nil t))


(provide 'py-yapf)

;;; py-yapf.el ends here
