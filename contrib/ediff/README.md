# Ediff Mode

This contribution layer adds some support for ediff-mode into spacemacs

## Key Bindings

## TODO:
We currently just set a few variables to make it look nicer.
Here is my first attempt at evilifying the buffer, does not work correctly, help is very much welcome.

```
(defun ediff/setup-ediff-keymaps ()
  "setup the evil ediff keymap"
    (progn
     (add-to-list 'evil-emacs-state-modes 'Ediff)
     (spacemacs|evilify ediff-mode-map)
     (spacemacs/activate-evil-leader-for-map 'ediff-mode-map)
      )
  )

;; inside the use-package function
(add-hook 'ediff-keymap-setup-hook 'ediff/setup-ediff-keymaps)
```
