# Perspectives 

This contrib layer sets up perspective-mode. And also defines custom
perspectives with a macro so that you don't have to do more steps.

## Custom Perspective Macro

If you want to add a new custom-persp (for example if you want to have IRC on its own perspective or maybe calendar or gnus) you have to use the macro `custom-persp` as follows:

```elisp
      (defun custom-persp/<persp-function-name> ()
        (interactive)
        (custom-persp "<name-to-be-shown-in-the-modeline>"
            (... stuff to be done in the persp activating a major mode like twittering or whatever ...)))
```

Then you just need to add a keybinding to your custom persp, we use
`<SPC> P o [your key here]` (Perspective open <key>)

``` elisp
        (evil-leader/set-key "Po<your key here>" 'custom-persp/<persp-function-name>))
```
## Keybindings

Perspective-mode defines keybindings under `C-x x` so we will take a more `spacemacsy` approach and define them under out perspective map `<SPC> P` 


## Persp-Projectile

As the name suggests, this persp-projectile mode creates a new
perspective once you switch to a new project with `<SPC> p s` you can
enable it by putting `(require 'persp-projectile)` in your configuration files.

If you are a helm person, and would rather use helm for projectile add this to your config as well:

```elisp 
(projectile-persp-bridge helm-projectile)
(setq projectile-switch-project-action 'helm-projectile)
```
