(defvar scale-packages
  '(
    ensime
    sbt-mode
    scala-mode2
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun spacemacs/init-ensime ()
  (use-package ensime
    :defer t))
