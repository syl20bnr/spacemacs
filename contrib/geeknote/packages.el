(defvar geeknote-packages
  '(
    geeknote
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar geeknote-excluded-packages '()
  "List of packages to exclude.")

(defun geeknote/init-geeknote ()
  "Initializes geeknote and adds keybindings for its exposed functionalities."
  (use-package geeknote
    :commands (geeknote-create
               geeknote-edit
               geeknote-find
               geeknote-show
               geeknote-remove
               )
    :init
    (progn
      (evil-leader/set-key
        "agc" 'geeknote-create
        "age" 'geeknote-edit
        "agf" 'geeknote-find
        "ags" 'geeknote-show
        "agr" 'geeknote-remove))))
