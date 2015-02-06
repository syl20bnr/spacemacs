# Perspectives 

This contrib layer sets up perspective-mode. And also defines custom
perspectives with a macro so that you don't have to do more steps.

## Custom Perspective Macro

If you want to add a new custom-persp (for example if you want to have
IRC on its own perspective or maybe calendar or gnus) you have to use
the macro `custom-persp` as follows:

```elisp
      (defun custom-persp/<persp-function-name> ()
        (interactive)
        (custom-persp "<name-to-be-shown-in-the-modeline>"
            (... stuff to be done in the persp activating a major mode like twittering or whatever ...)))
```

You can check out the layer's packages.el to see some examples of the
custom-perspectives. if you define something like this you may be able
to define a perspective with a layout.

```elisp
      (defun custom-persp/c++-project ()
        (interactive)
        (custom-persp "c++"
            (progn
                (find-file "~/path/to/first/file.cpp")
                (split-window-right)
                (find-file "~/path/to/second/file.cpp")
                (... do more stuff but be careful not to destroy the universe ...)
            )))
```

Then you just need to add a keybinding to your custom persp, we use
`<SPC> P o [your key here]` (Perspective open <key>)

``` elisp
        (evil-leader/set-key "Po<your key here>" 'custom-persp/<persp-function-name>))
```

### org-agenda `<SPC> P o o` (Perspective Open Org)

Here we define a custom perspective that adds items to your org-agenda if you do not know what that is check the [docs](https://www.gnu.org/software/emacs/manual/html_node/org/Agenda-commands.html). The cool part is that you can have many org files with todos in the agenda and with one simple command you can gather all the todos from all the agenda files you have and show them in a single buffer. (in evil the command starts with `; a`)

## Keybindings

Perspective-mode defines keybindings under `C-x x` so we will take a
more `spacemacsy` approach and define them under out perspective map
`<SPC> P`

  (projectile-persp-bridge helm-projectile)
  (setq projectile-switch-project-action 'helm-projectile)


## Persp-Projectile

As the name suggests, this persp-projectile mode creates a new
perspective once you switch to a new project with `<SPC> p s`. It must
be said that in the current implementation in order for this to work
you must first open a custom-perspective like `SPC P o e` to go to the
init.el in the spacemacs.

If you are a helm person, and would rather use helm for projectile add this to your config as well:

```elisp 
(projectile-persp-bridge helm-projectile)
(setq projectile-switch-project-action 'helm-projectile)
```
