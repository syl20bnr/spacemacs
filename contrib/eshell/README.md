# Eshell

This layer adds some fancy features and better defaults to the built in Emacs Eshell:
- Commands for popping out shells
- Better integration with evil mode
- Company mode completion integration
- Some handy aliases and config options

## Aliases

#### cdp
Changes to the root directory of the project.

#### x
Exits the shell and closes the window

## Keybindings

Key Binding         |                 Description
--------------------|------------------------------------------------------------------
<kbd>SPC E s  </kbd>| Start eshell in current window
<kbd>SPC E p  </kbd>| Pop open an eshell window below the current buffer in its dir
<kbd>SPC E d  </kbd>| Switch an existing eshell to the directory of the current buffer
