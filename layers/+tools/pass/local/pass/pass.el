;;; pass.el --- Support for the pass password manager

;; Copyright (C) 2015 Allen Li
;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: pass
;; Package-Requires: ((helm "1.7.9"))

;; This file is not part of GNU Emacs.

;;; Code:

(defun pass--copy-pass (entry)
  "Copy password and show notes."
  (with-output-to-temp-buffer "*pass*"
    (call-process "pass" nil (get-buffer "*pass*") nil entry)
    (set-buffer "*pass*")
    (goto-char (point-min))
    (end-of-line)
    (clipboard-kill-region (point-min) (point))
    (delete-char 1)))

(defun pass--pass-candidates ()
  "Return list of password candidates."
  (let (output)
    (setq output
          (shell-command-to-string
           (concat
            "cd ~/.password-store;"
            "find . -name \"*.gpg\" | "
            "sed 's/\\.\\/\\(.*\\)\\.gpg/\\1/'")))
    (split-string output "\n")))

(defun pass-get-pass ()
  "Get password.

This command prompts the user to select a password name using
Helm, then copies the password (the first line of pass's output)
to the clipboard and displays the remaining lines in a buffer.

"
  (interactive)
  (let (choice)
    (setq choice (helm-comp-read "Pass" (pass--pass-candidates)))
    (pass--copy-pass choice)))

(provide 'pass)

;;; pass.el ends here
