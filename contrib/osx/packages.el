(defvar osx-packages
  '(
    pbcopy
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :init
    (unless (display-graphic-p)
      (turn-on-pbcopy))))
