(defun java/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))

  (insert ".")
  (company-emacs-eclim 'interactive))


(defun java/maven-test ()
  (interactive)
  (eclim-maven-run "test"))

(defun java/maven-clean-install ()
  (interactive)
  (eclim-maven-run "clean install"))

(defun java/maven-install ()
  (interactive)
  (eclim-maven-run "install"))
