# evil-escape

Customizable key sequence to escape from insert state and everything else in
Emacs.

Press `fd` quickly to:

- escape from all evil states to normal state
- escape from evil-lisp-state to normal state
- abort evil ex command
- quit minibuffer
- abort isearch
- quit magit buffers
- quit help buffers
- hide neotree buffer

And more to come !

Contributions to support more buffers are _very welcome_:
**Escape Everything !**

## Install

The package _will be available soon_ in [MELPA][].

If you have MELPA in `package-archives`, use

    M-x package-install RET evil-escape RET

If you don't, open `evil-escape.el` in Emacs and call
`package-install-from-buffer`.

## Usage

To toggle the `evil-escape` mode globally:

    M-x evil-escape-mode

## Customization

Open the customization group buffer:

    M-x customize-group RET evil-escape RET

There you can change the key sequence to your desire.
The default value is `fd`.

[MELPA]: http://melpa.org/
