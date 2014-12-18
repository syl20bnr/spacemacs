(defvar perforce-packages '(p4)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun perforce/init-p4 ()
  (use-package p4
    :commands (p4-add
               p4-delete
               p4-describe
               p4-edit
               p4-revert)
    :init
    (evil-leader/set-key
      "p4a" 'p4-add
      "p4d" 'p4-delete
      "p4D" 'p4-describe
      "p4e" 'p4-edit
      "p4R" 'p4-revert
      "p4r" 'p4-rename
      "p4S" 'p4-submit)))
