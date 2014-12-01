# What is this
This is a simple package for capturing and visiting todo items for
the repository you are currently in. Under the hood it uses
`org-capture` to provide a popup window for inputting `org-mode`
checkboxed todo items (http://orgmode.org/manual/Checkboxes.html)
or regular *TODO items that get saved to a TODO.org file in the
root of the repository.

# Keybindings

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC T c</kbd>    | ort/capture-todo
<kbd>SPC T C</kbd>    | ort/capture-todo-check
<kbd>SPC T g</kbd>    | ort/goto-todos

Using the `<SPC> u` command prefix with either of these commands will
capture/visit the todos in your `user-emacs-directory` instead.
