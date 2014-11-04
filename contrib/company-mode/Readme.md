# Company Mode

In order to use this you should disable the default `auto-complete` package plus everything that depends on it:

```elisp
(setq-default dotspacemacs-excluded-packages
  '(auto-complete ac-ispell tern-auto-complete auto-complete-clang enslime edts))
```
