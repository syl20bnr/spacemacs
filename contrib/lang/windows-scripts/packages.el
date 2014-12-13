(defvar windows-scripts-packages
  '(
    powershell
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun windows-scripts/init-powershell ()
  (use-package powershell
    :defer t))
