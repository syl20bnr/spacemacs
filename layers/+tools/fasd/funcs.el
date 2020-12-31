(with-eval-after-load 'fasd
  (defun ivy-search-from-action (x)
    (if (file-directory-p x)
        (spacemacs/counsel-search dotspacemacs-search-tools nil x)
      (message "Selected item is not a directory path")))
  
  (ivy-set-actions
   'fasd-find-file
   '(("o" fasd-find-file-action "find-file")
     ("s" ivy-search-from-action "search-from"))))
