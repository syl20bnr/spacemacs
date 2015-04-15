# Deft

[Deft](http://jblevins.org/projects/deft/) is an Emacs mode inspired by Notational Velocity for quickly browsing and creating notes.

**Note:** You may have to update the `evil-escape` package for deft to work, otherwise filtering will ignore your escape character.

## Differences from default

This layer sets the default to use filenames for note titles as well as org-mode for editing.
The default extension is still `txt`.

## Configuration

By default deft tries to put notes in `~/.deft` but you can change this like so:
```
(setq deft-directory "~/Dropbox/notes")
```

## Keybindings

Key Binding         |                 Description
--------------------|------------------------------------------------------------------
<kbd>SPC a n  </kbd>| Open Deft (works globally)
<kbd>SPC m d  </kbd>| Delete selected note
<kbd>SPC m r  </kbd>| Rename selected note
<kbd>SPC m i  </kbd>| Toggle to regex search
<kbd>SPC m n  </kbd>| Create new file with filter text
