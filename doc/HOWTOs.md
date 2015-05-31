# Compilation of quick HOW-TOs for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Compilation of quick HOW-TOs for Spacemacs](#compilation-of-quick-how-tos-for-spacemacs)
    - [Disable a package completely](#disable-a-package-completely)
    - [Disable a package only for a specific major-mode](#disable-a-package-only-for-a-specific-major-mode)
    - [Disable company for a specific major-mode](#disable-company-for-a-specific-major-mode)
    - [Change special buffer rules](#change-special-buffer-rules)
    - [Enable navigation by visual lines](#enable-navigation-by-visual-lines)

<!-- markdown-toc end -->

## Disable a package completely

To completely disable a package and effectively uninstalling it even if it
is part of your used layers, look for the variable
`dotspacemacs-excluded-packages` in your dotfile and add the package name
to it:

```elisp
dotspacemacs-excluded-packages '(package1 package2 ...)
```

## Disable a package only for a specific major-mode

This is done by removing the hook added by Spacemacs. For example to
remove `flycheck` support in python buffers, look for the function
`dotspacemacs/config` in your dotfile and add the following code:

```elisp
(remove-hook 'python-mode-hook 'flycheck-mode)
```

**Hint** to know the name of the major-mode of the current buffer press:
<kbd>SPC h d v major-mode RET</kbd>

## Disable company for a specific major-mode

It may be handy to disable `company` for a given mode if you plan on
configuring `auto-complete` instead. On easy way to do it is to use
the macro `spacemacs|disable-company` in the function
`dotspacemacs/config` of your dotfile. The following snippet disables
company for `python-mode`:

```elisp
(spacemacs|disable-company python-mode)
```

## Change special buffer rules

To change the way spacemacs marks buffers as useless, you can customize
`spacemacs-useless-buffers-regexp` which marks buffers matching the regexp
as useless. The variable `spacemacs-useful-buffers-regexp` marks buffers
matching the regexp as useful buffers. Both can be customized the same way.

Examples:

```elisp
;; Only mark helm buffers as useless
(setq spacemacs-useless-buffers-regexp '("\\*helm\.\+\\*"))

;; Marking the *Messages* buffer as useful
(push "\\*Messages\\*" spacemacs-useful-buffers-regexp)
```
## Enable navigation by visual lines

Add the following snippet to your `dostpacemacs/config` function:

```elisp
;; Make evil-mode up/down operate in screen lines instead of logical lines		
(define-key evil-motion-state-map "j" 'evil-next-visual-line)		
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)		
;; Also in visual mode		
(define-key evil-visual-state-map "j" 'evil-next-visual-line)		
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)
```
