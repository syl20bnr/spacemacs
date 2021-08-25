(defun spacemacs/bibtex-find-bib-file ()
  (interactive)
  (pop-to-buffer
   (find-file-noselect
    (completing-read "Select file for opening: " ebib-preload-bib-files))))
