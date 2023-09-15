(defun spacemacs/bibtex-find-bib-file ()
  (interactive)
  (pop-to-buffer
   (find-file-noselect
    (completing-read "Select file for opening: " ebib-preload-bib-files))))

(defun execute-papiers-command (command)
  (start-process-shell-command "papiers-process" "*papiers-output*" command))

(defun papiers-list ()
  (interactive)
  (kill-new (prompt-paper)))

(defun papiers-download (url)
  (interactive (list (read-string "URL: ")))
  (execute-papiers-command (concat "pap dl " url)))

(defun get-paper-list ()
  (shell-command-to-string "pap ls"))

(defun extract-paper-id (pstr)
  (if (string-match "\\[\\([0-9]+\\)\\]:" pstr)
       (match-string 1 pstr)
     nil))

(defun compare-papers (p1 p2)
  (let ((i1 (extract-paper-id p1))
        (i2 (extract-paper-id p2)))
    (- (string-to-number i2) (string-to-number i1))))

(defun prompt-paper ()
  (ivy-read "Paper: "
            (sort (split-string (string-trim (get-paper-list)) "\n") #'compare-papers)))

(defun prompt-paper-for-id ()
  (let ((pap (prompt-paper)))
    (extract-paper-id pap)))

(defun match-paper (pid)
  (interactive (list (prompt-paper-for-id)))
  (execute-papiers-command (concat "pap match " pid)))

