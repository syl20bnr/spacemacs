(defvar osx-packages
  '(
    pbcopy
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (not (display-graphic-p))
    :init (turn-on-pbcopy)))
