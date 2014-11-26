# Paradox

This contrib layer switches the old list-packages for the more feature
rich Paradox package. you can find more info
[here](https://github.com/Bruce-Connor/paradox). Important this will
override your old `list-packages`.

## Usage

You can do `SPC a P` to open paradox-list-packages. Sad part is that
for now you can only swap to evil-emacs-state `C-z` and navigate with
n and p, other helpful tips show when you press `h`. Refer to the
github page of the package for more info.

## Repo stars

To be able to star packages you have to create a github
[token](https://github.com/settings/tokens/new) and only add Read
permissions to your public repos then you set it in your
configuration files like this:

```elisp
  (setq paradox-github-token "<replace your token here>")
```

### OR

If you are feeling like so, create a .authinfo.gpg file in your home
directory. (With Emacs, obviously... `C-x C-f ~/.authinfo.gpg RET`).

The contents of this file follow
[netrc guidelines](http://www.gnu.org/software/emacs/manual/html_node/auth/Help-for-users.html).
You should have a file like this:

```
machine github.com user paradox password <here goes your token without quotes> port paradox 
```

And then proceed to save that buffer. Just going for `OK` in the pop up
menu is OK. Enter your super secure password and now you have your
GitHub token being accesed securely ( even though you only gave
permissions to read your public repos, I'm sure I'll find another use
for this code later)
